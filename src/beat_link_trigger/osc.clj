(ns beat-link-trigger.osc
  "Robust OSC sender with prioritized queues; tracked updates send immediately."
  (:require [clojure.string :as str]
            [taoensso.timbre :as timbre])
  (:import [java.util.concurrent Executors TimeUnit LinkedBlockingQueue ThreadFactory]))

;; Client cache --------------------------------------------------------------

(defonce clients (atom {}))

(defn- ensure-osc-loaded! []
  (try (require 'overtone.osc)
       (catch Throwable _)))

(defn get-client
  ^Object [host port]
  (ensure-osc-loaded!)
  (let [k [host (int port)]]
    (if-let [c (get @clients k)]
      c
      (let [osc-client-fn (resolve 'overtone.osc/osc-client)
            c             (osc-client-fn host (int port))]
        (when (or (= host "255.255.255.255") (str/ends-with? host ".255"))
          (try
            (when-let [^java.net.DatagramSocket s (:socket c)]
              (.setBroadcast s true))
            (catch Throwable _)))
        (swap! clients assoc k c)
        c))))

(defn close-all-clients! []
  (ensure-osc-loaded!)
  (let [close-fn (resolve 'overtone.osc/osc-close)]
    (doseq [c (vals @clients)]
      (try (close-fn c) (catch Throwable _)))
    (reset! clients {})))

;; Sender infrastructure ------------------------------------------------------

;; Priority queues: critical (activation/deactivation), beat, tracked
(defonce ^:private critical-q (LinkedBlockingQueue. 1024))
(defonce ^:private beat-q     (LinkedBlockingQueue. 2048))
(defonce ^:private tracked-q  (LinkedBlockingQueue. 4096))

(defrecord OscMsg [host port path args])

(defn- send-now! [{:keys [^String host ^long port ^String path args]}]
  (try
    (let [client (get-client host port)
          send-f (resolve 'overtone.osc/osc-send)]
      (timbre/debug "[OSC] send" host port path args)
      (apply send-f client path args))
    (catch Throwable t
      (timbre/warn t "OSC send failed" host port path))))

(defn- named-thread-factory [^String name ^long prio]
  (reify ThreadFactory
    (newThread [_ runnable]
      (doto (Thread. runnable name)
        (.setDaemon true)
        (.setPriority (int prio))))))

(defonce ^:private sender-exec (atom nil))

(defn start! []
  (when (nil? @sender-exec)
    (let [exec (Executors/newSingleThreadExecutor (named-thread-factory "OSC-Sender" Thread/MAX_PRIORITY))]
      (.submit exec
               (reify Runnable
                 (run [_]
                   (timbre/debug "OSC sender thread started")
                   (while (not (.isShutdown exec))
                     (try
                       (when-some [m (.poll critical-q 1 TimeUnit/MILLISECONDS)]
                         (send-now! m))
                       (when-some [m (.poll beat-q 1 TimeUnit/MILLISECONDS)]
                         (send-now! m))
                       (when-some [m (.poll tracked-q 1 TimeUnit/MILLISECONDS)]
                         (send-now! m))
                       (catch Throwable t
                         (timbre/warn t "Problem in OSC sender loop")))))))
      (reset! sender-exec exec)))
  true)

(defn stop! []
  (when-let [^java.util.concurrent.ExecutorService exec @sender-exec]
    (reset! sender-exec nil)
    (.shutdownNow exec)))

;; Public enqueue APIs --------------------------------------------------------

(defn enqueue-critical! [host port path args]
  (start!)
  (.offer critical-q (->OscMsg host port path (vec args))))

(defn enqueue-beat! [host port path args]
  (start!)
  (.offer beat-q (->OscMsg host port path (vec args))))

(defn publish-tracked!
  [host port path args]
  (start!)
  (.offer tracked-q (->OscMsg host port path (vec args))))

;; High-level router ----------------------------------------------------------

(defn route-send!
  "Route a computed OSC message to the proper lane by semantic type.
   Arity 5: kind host port path args."
  [kind host port path args]
  (case kind
    (:activation :deactivation) (enqueue-critical! host port path args)
    :beat                       (enqueue-beat! host port path args)
    :tracked                    (publish-tracked! host port path args)
    (enqueue-beat! host port path args)))