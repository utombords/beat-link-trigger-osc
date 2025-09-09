(ns beat-link-trigger.triggers
  "Implements the main window, displaying a list of triggers that send
  events in response to changes in CDJ states."
  (:require [beat-link-trigger.carabiner :as carabiner]
            [beat-link-trigger.editors :as editors]
            [beat-link-trigger.expressions :as expressions]
            [beat-link-trigger.help :as help]
            [beat-link-trigger.menus :as menus]
            [beat-link-trigger.nrepl :as nrepl]
            [beat-link-trigger.players :as players]
            [beat-link-trigger.playlist-writer :as writer]
            [beat-link-trigger.overlay :as overlay]
            [beat-link-trigger.track-loader :as track-loader]
            [beat-link-trigger.settings :as settings]
            [beat-link-trigger.settings-loader :as settings-loader]
            [beat-link-trigger.simulator :as sim]
            [beat-link-trigger.show :as show]
            [beat-link-trigger.show-util :as show-util]
            [beat-link-trigger.prefs :as prefs]
            [beat-link-trigger.util :as util]
            [beat-carabiner.core :as beat-carabiner]
            [clojure.java.browse]
            [clojure.set]
            [clojure.string]
            [fipp.edn :as fipp]
            [inspector-jay.core :as inspector]
            [overtone.osc :as osc]
            [seesaw.bind :as bind]
            [seesaw.chooser :as chooser]
            [seesaw.core :as seesaw]
            [seesaw.mig :as mig]
            [taoensso.timbre :as timbre])
  (:import [beat_link_trigger.util PlayerChoice]
           [java.awt Color Graphics2D RenderingHints]
           [java.awt.event WindowEvent]
           [java.io File]
           [javax.swing JFrame JMenu JMenuItem JCheckBoxMenuItem JPanel JRadioButtonMenuItem UIManager]
           [org.deepsymmetry.beatlink BeatFinder BeatListener CdjStatus CdjStatus$TrackSourceSlot
            DeviceAnnouncementListener DeviceFinder DeviceUpdateListener LifecycleListener
            Util VirtualCdj]
           [org.deepsymmetry.beatlink.data AnalysisTagFinder ArtFinder BeatGridFinder CrateDigger MetadataFinder
            SearchableItem SignatureFinder TimeFinder TrackMetadata WaveformFinder]))

;; This used to be where this was defined, but for compilation order reasons, and then also for
;; better isolation of user code, it has been swapped into the expressions namespace.
(require 'beat-link-trigger.expressions.triggers)
(refer 'beat-link-trigger.expressions.triggers :only '[globals] :rename '{globals expression-globals})

(defonce ^{:private true
           :doc "Holds the trigger window, through which we can access and
  manipulate the triggers themselves."}
  trigger-frame
  (atom nil))

(def ^:private theme-colors
  "Holds colors used to draw the user interface appropriately for the
  current user interface theme."
  (atom {}))

(defn- online-menu-item
  "Helper function to find the Online menu item, which is often toggled
  by code."
  ^JCheckBoxMenuItem []
  (seesaw/select @trigger-frame [:#online]))

(def ^DeviceFinder device-finder
  "A convenient reference to the [Beat Link
  `DeviceFinder`](https://deepsymmetry.org/beatlink/apidocs/org/deepsymmetry/beatlink/DeviceFinder.html)
  singleton."
  (DeviceFinder/getInstance))

(def ^VirtualCdj virtual-cdj
  "A convenient reference to the [Beat Link
  `VirtualCdj`](https://deepsymmetry.org/beatlink/apidocs/org/deepsymmetry/beatlink/VirtualCdj.html)
  singleton."
  (VirtualCdj/getInstance))

(def ^MetadataFinder metadata-finder
  "A convenient reference to the [Beat Link
  `MetadataFinder`](https://deepsymmetry.org/beatlink/apidocs/org/deepsymmetry/beatlink/data/MetadataFinder.html)
  singleton."
  (MetadataFinder/getInstance))

;; Register the custom readers needed to read back in the defrecords that we use.
(prefs/add-reader 'beat_link_trigger.util.PlayerChoice util/map->PlayerChoice)
(prefs/add-reader 'beat_link_trigger.util.MidiChoice util/map->MidiChoice)
;; For backwards compatibility:
;; Also register under the old package names before they were moved to the util namespace.
(prefs/add-reader 'beat_link_trigger.triggers.PlayerChoice util/map->PlayerChoice)
(prefs/add-reader 'beat_link_trigger.core.PlayerChoice util/map->PlayerChoice)
(prefs/add-reader 'beat_link_trigger.triggers.MidiChoice util/map->MidiChoice)
(prefs/add-reader 'beat_link_trigger.core.MidiChoice util/map->MidiChoice)

(defn- initial-trigger-prefs
  "Create the values to assign the trigger preferences map."
  []
  (merge {:global true}
         (select-keys (prefs/get-preferences) [:send-status? :tracks-using-playlists?])))

(defonce ^{:private true
           :doc "Global trigger configuration and expressions."}
  trigger-prefs
  (atom (initial-trigger-prefs)))

(defn quit
  "Gracefully attempt to quit, giving the user a chance to veto so they
  can save unsaved changes in editor windows. Essentially, click the
  close box of the Triggers window, if it is open; otherwise we are in
  a state where there is nothing to save anyway, so just exit."
  []
  (if @trigger-frame
    (.dispatchEvent ^JFrame @trigger-frame (WindowEvent. @trigger-frame WindowEvent/WINDOW_CLOSING))
    (System/exit 0)))

(defn settings
  "Present a user interface allowing users to adjust behaviors to better
  fit their goals."
  []
  (settings/show-dialog))

(defn real-player?
  "Checks whether the user wants to pose as a real player, numbered 1
  through 4, and send status update packets. This is the only way we
  can take charge of other players' tempo, and get metadata for CD and
  unanalyzed media."
  []
  (boolean (:send-status? @trigger-prefs)))

(defn- enabled?
  "Check whether a trigger is enabled."
  ([trigger]
   (let [data @(seesaw/user-data trigger)]
     (enabled? trigger data)))
  ([_ data]
   (case (get-in data [:value :enabled])
     "Always" true
     "On-Air" (:on-air data)
     "Custom" (get-in data [:expression-results :enabled])
     false)))

(defn- run-trigger-function
  "Checks whether the trigger has a custom function of the specified
  kind installed, and if so runs it with the supplied status argument
  and the trigger local and global atoms. Returns a tuple of the
  function return value and any thrown exception. If `alert?` is
  `true` the user will be alerted when there is a problem running the
  function."
  [trigger kind status alert?]
  (let [data @(seesaw/user-data trigger)]
    (when-let [custom-fn (get-in data [:expression-fns kind])]
      (try
        (binding [*ns* (the-ns (expressions/expressions-namespace))]
          [(custom-fn status data) nil])
        (catch Throwable t
          (timbre/error t (str "Problem running " (editors/triggers-editor-title kind trigger false) ":\n"
                               (get-in data [:expressions kind])))
          (when alert?
            (seesaw/alert (str "<html>Problem running trigger " (name kind) " expression.<br><br>" t)
                          :title "Exception in Trigger Expression" :type :error))
          [nil t])))))

(defn- run-custom-enabled
  "Invokes the custom enabled filter assigned to a trigger, if any,
  recording the result in the trigger user data."
  [status trigger]
  (when (= "Custom" (get-in @(seesaw/user-data trigger) [:value :enabled]))
    (let [[enabled? _] (run-trigger-function trigger :enabled status false)]
      (swap! (seesaw/user-data trigger) assoc-in [:expression-results :enabled] enabled?))))

(defn- device-present?
  "Checks whether a device is on the network by number. Works both when
  online and when simulated playback is happening."
  [device-number]
  (boolean (or (sim/for-player device-number)
               (some? (when (util/online?) (.getLatestAnnouncementFrom device-finder device-number))))))

(defn- latest-status-for
  "Returns the latest status packet received from a device by number.
  Works both when online and when simulated playback is happening."
  [^Long device-number]
  (or (:latest-status (sim/for-player device-number))
      (when (util/online?) (.getLatestStatusFor virtual-cdj device-number))))

(defn- is-better-match?
  "Checks whether the current status packet represents a better
  matching device for a trigger to track than the one it is currently
  tracking. The best kind of match is both enabled and playing, second
  best is at least enabled, third best is playing. Ties are broken in
  favor of the player with the lowest number.

  In order to determine the enabled state, we need to run the custom
  enabled function if it is in use, so that will be called for all
  incoming packets for such triggers."
  [^CdjStatus status trigger]
  (let [data                             @(seesaw/user-data trigger)
        enable-mode                      (get-in data [:value :enabled])
        custom-enable                    (= enable-mode "Custom")
        custom-result                    (when custom-enable
                                           (first (run-trigger-function trigger :enabled status false)))
        would-enable?                    (case enable-mode
                                           "Always" true
                                           "On-Air" (.isOnAir status)
                                           "Custom" custom-result
                                           false)
        this-device                      (.getDeviceNumber status)
        match-score                      (+ (if would-enable? 1024 0)
                                            (if (.isPlaying status) 512 0)
                                            (- this-device))
        [existing-score existing-device] (:last-match data)
        best                             (or (when (some? existing-device) (not (device-present? existing-device)))
                                             (>= match-score (or existing-score -256)))]
    (when (or best (= this-device existing-device))
      (swap! (seesaw/user-data trigger) assoc :last-match [match-score this-device]))
    (when (and best custom-enable)
      (swap! (seesaw/user-data trigger) assoc-in [:expression-results :enabled] custom-result))
    best))

(defn- matching-player-number?
  "Checks whether a CDJ status update matches a trigger, handling the
  special cases of the Master Player and Any Player. For Any Player we
  want to stay tracking the same Player most of the time, so we will
  keep track of the last one we matched, and change only if this is a
  better match. This should only be called with full-blown status
  updates, not beats."
  [^CdjStatus status trigger player-selection]
  (and (some? player-selection)
       (or (= (:number player-selection) (.getDeviceNumber status))
           (and (zero? (:number player-selection)) (.isTempoMaster status))
           (and (neg? (:number player-selection)) (is-better-match? status trigger)))))

(defn get-player-choices
  "Returns a sorted list of the player watching choices, including
  options to watch Any Player and the Master Player, plus any
  currently visible player numbers (handles CDJ-3000 5/6, etc.)."
  []
  (let [visible (try (sort (util/visible-player-numbers)) (catch Throwable _ []))
        base    [-1 0]
        fallback [1 2 3 4]
        nums    (distinct (concat base (if (seq visible) visible fallback)))]
    (map #(PlayerChoice. %) nums)))



(defn- report-activation
  "Trigger any custom Activation expression."
  ([trigger status data]
   (report-activation trigger status data true))
  ([trigger status data real?]
   (try
     (run-trigger-function trigger :activation status (not real?))
     (catch Exception e
       (timbre/error e "Problem reporting player activation.")))))

(defn- report-deactivation
  "Trigger any custom Deactivation expression."
  ([trigger status data]
   (report-deactivation trigger status data true))
  ([trigger status data real?]
   (try
     (run-trigger-function trigger :deactivation status (not real?))
     (catch Exception e
       (timbre/error e "Problem reporting player deactivation.")))))

(declare send-osc-if-configured!)

(defn- update-player-state
  "If the Playing state of a device being watched by a trigger has
  changed, send appropriate messages, start/update or stop its
  associated clock synchronization thread, and record the new state.
  Finally, run the Tracked Update Expression, if there is one, and we
  actually received a status update."
  [trigger playing on-air ^CdjStatus status]
  (let [old-data @(seesaw/user-data trigger)
        updated  (swap! (seesaw/user-data trigger)
                        (fn [data]
                          (let [tripped (and playing (enabled? trigger))]
                            (merge data
                                   {:playing playing :on-air on-air :tripped tripped}
                                   (when (some? status) {:status status})))))]
    (let [tripped (:tripped updated)]
      (when-not (= tripped (:tripped old-data))
        (if tripped
          (do (report-activation trigger status updated)
              (when (= "Activation" (get-in (:value @(seesaw/user-data trigger)) [:osc-send-on]))
                (send-osc-if-configured! trigger status)))
          (do (report-deactivation trigger status updated)
              (when (= "Deactivation" (get-in (:value @(seesaw/user-data trigger)) [:osc-send-on]))
                (send-osc-if-configured! trigger status)))))
      )
    )
  (when (some? status)
    (run-trigger-function trigger :tracked status false)
    (when (and (= "Tracked Update" (get-in (:value @(seesaw/user-data trigger)) [:osc-send-on]))
               (enabled? trigger))
      (send-osc-if-configured! trigger status)))
  (seesaw/repaint! (seesaw/select trigger [:#state])))

(defn describe-track
  "Identifies a track with the best information available from its
  status update. If a non-`nil` `custom-description` is available, use
  it. Otherwise, honor the user preference setting to display either
  the rekordbox id information associated with it (when available), or
  simply the track's position within its playlist."
  [^CdjStatus status custom-description]
  (cond
    (some? custom-description)
    custom-description

    (and (pos? (.getRekordboxId status)) (not (:tracks-using-playlists? @trigger-prefs)))
    (str "Track id " (.getRekordboxId status) " [" (.getTrackSourcePlayer status) ":"
         (util/case-enum (.getTrackSourceSlot status)
           CdjStatus$TrackSourceSlot/USB_SLOT "usb"
           CdjStatus$TrackSourceSlot/SD_SLOT "sd"
           CdjStatus$TrackSourceSlot/CD_SLOT "cd"
           CdjStatus$TrackSourceSlot/COLLECTION "rb"
           "?")
         "]")

    :else
    (str "Track #" (.getTrackNumber status))))

(defn- extract-label
  "Given a `SearchableItem` from track metadata, extracts the string
  label. If passed `nil` simply returns `nil`."
  [^SearchableItem item]
  (when item
    (.label item)))

(defn- format-metadata
  "Include the appropriate track metadata items for display. If a
  non-nil `custom-summary` is available, use it. Otherwise show the
  track title, an em dash, and the track artist."
  [^TrackMetadata metadata custom-summary]
  (let [summary (or custom-summary (str (.getTitle metadata) "&mdash;" (extract-label (.getArtist metadata))))]
    (str "<br>&nbsp;&nbsp; " summary)))

(defn build-status-label
  "Create a brief textual summary of a player state given a status
  update object from beat-link, and track description and metadata
  summary overrides from the trigger's custom expression
  locals (either of which may be `nil`, which means to build the
  standard description and summary for the track)."
  [^CdjStatus status track-description metadata-summary]
  (let [beat (.getBeatNumber status)
        [metadata metadata-summary] (if (.isRunning metadata-finder)
                                      [(.getLatestMetadataFor metadata-finder status) metadata-summary]
                                      (let [sim-data (get-in (sim/for-player (.getDeviceNumber status))
                                                             [:track :metadata])]
                                        [nil (or metadata-summary
                                                 (str (:title sim-data) "&mdash;" (:artist sim-data)))]))
        using-metadata? (or metadata metadata-summary)]
    (str (when using-metadata? "<html>")
         (.getDeviceNumber status) (if (.isPlaying status) " Playing" " Stopped")
         (when (.isTempoMaster status) ", Master")
         (when (.isOnAir status) ", On-Air")
         ", " (describe-track status track-description)
         ", " (if (= 65535 (.getBpm status)) "--.-" (format "%.1f" (.getEffectiveTempo status))) " BPM ("
         (format "%+.2f%%" (Util/pitchToPercentage (.getPitch status))) ")"
         (cond
           (neg? beat) ", beat n/a"
           (zero? beat) ", lead-in"
           :else (str ", beat " beat " (" (inc (quot (dec beat) 4)) "." (inc (rem (dec beat) 4)) ")"))
         (when using-metadata? (format-metadata metadata metadata-summary)))))

(defn- show-device-status
  "Set the device satus label for a trigger outside of the context of
  receiving an update from the device (for example, the user chose a
  device in the menu which is not present on the network, or we just
  received a notification from the DeviceFinder that the device has
  disappeared. In either case, we are already on the Swing Event
  Update thread."
  [trigger]
  (try
    (let [player-menu             (seesaw/select trigger [:#players])
          ^PlayerChoice selection (seesaw/selection player-menu)
          status-label            (seesaw/select trigger [:#status])
          track-description       (:track-description @(:locals @(seesaw/user-data trigger)))
          metadata-summary        (:metadata-summary @(:locals @(seesaw/user-data trigger)))]
      (if (nil? selection)
        (do (seesaw/config! status-label :foreground "red")
            (seesaw/value! status-label "No Player selected.")
            (update-player-state trigger false false nil))
        (let [device-number (int (.number selection))
              found         (device-present? device-number)
              sel-status    (when found (latest-status-for device-number))]
          (if found
            (if (instance? CdjStatus sel-status)
              (do (seesaw/config! status-label :foreground (:valid-status-color @theme-colors))
                  (seesaw/value! status-label (build-status-label sel-status track-description metadata-summary)))
              (do (seesaw/config! status-label :foreground "red")
                  (seesaw/value! status-label (cond (some? sel-status)  "Non-Player status received."
                                                    (not (util/online?)) "Offline."
                                                    :else                "No status received."))))
            (do (seesaw/config! status-label :foreground "red")
                (seesaw/value! status-label (if (util/online?) "Player not found." "Offline."))
                (update-player-state trigger false false nil))))))
    (catch Exception e
      (timbre/error e "Problem showing Trigger Player status."))))



(defn get-triggers
  "Returns the list of triggers that currently exist. If `show` is
  supplied, returns the triggers that belong to that show (if `show`
  is supplied but `nil`, returns the triggers that exist independently
  of any show)."
  ([]
   (when-let [frame @trigger-frame]
     (seesaw/config (seesaw/select frame [:#triggers]) :items)))
  ([show]
   (filter #(= (:file show) (:show-file @(seesaw/user-data %))) (get-triggers))))

(defn find-trigger
  "Returns the trigger with the matching UUID, if it exists."
  [uuid]
  (some #(when (= uuid (seesaw/user-data (seesaw/select % [:#index]))) %) (get-triggers)))

(defn scroll-to-trigger
  "Makes sure the specified trigger is visible on behalf of an
  expression report."
  [uuid]
  (when-let [trigger (find-trigger uuid)]
    (timbre/info "Scroll to" trigger)
    (seesaw/invoke-later (seesaw/scroll! (seesaw/select @trigger-frame [:#triggers])
                                         :to (.getBounds ^JPanel trigger)))))

(defn- adjust-triggers
  "Called when a trigger is added or removed to restore the proper
  alternation of background colors and identification of source shows.
  Also resize the window if it still fits the screen, and update any
  other user interface elements that might be affected. If the current
  dark mode state is known, it can be passed in to save a redundant
  lookup of that and the user preferences."
  ([]
   (adjust-triggers (prefs/dark-mode?)))
  ([dark?]
   (swap! theme-colors assoc :valid-status-color (if dark? Color/CYAN Color/BLUE))
   (when (seq (get-triggers))
     (loop [triggers       (get-triggers)
            trigger-index  1
            show-hue       nil
            last-show-file nil]
       (let [trigger         (first triggers)
             remaining       (rest triggers)
             ^File show-file (:show-file @(seesaw/user-data trigger))
             show-hue        (if (= show-file last-show-file)
                               show-hue ; Either let show explicitly set hue, or rotate through color wheel
                               (or (:show-hue @(seesaw/user-data trigger)) (mod (+ (or show-hue 0.0) 62.5) 360.0)))]
         (seesaw/config! trigger :background (util/trigger-color trigger-index show-hue dark?))
         (seesaw/config! (seesaw/select trigger [:#index]) :text (str trigger-index "."))
         (if (= show-file last-show-file)
           (seesaw/config! (seesaw/select trigger [:#from-show]) :visible? false)
           (seesaw/config! (seesaw/select trigger [:#from-show]) :visible? true
                           :text (str "Triggers from Show " (util/trim-extension (.getPath show-file)) ":")))
         (doseq [editor (vals (:expression-editors @(seesaw/user-data trigger)))]
           (editors/retitle editor))
         (when (seq remaining)
           (recur remaining
                  (inc trigger-index)
                  show-hue
                  show-file)))))
   (let [^JFrame frame @trigger-frame]
     (when (< 100 (- (.height (.getBounds (.getGraphicsConfiguration frame)))
                     (.height (.getBounds frame))))
       (.pack frame)))))

(defn- run-global-function
  "Checks whether the trigger frame has a custom function of the
  specified kind installed, and if so runs it with a nil status and
  trigger local atom, and the trigger global atom. Returns a tuple of
  the function return value and any thrown exception."
  [kind]
  (let [data @trigger-prefs]
    (when-let [custom-fn (get-in data [:expression-fns kind])]
      (try
        (binding [*ns* (the-ns (expressions/expressions-namespace))]
          (let [ret (custom-fn nil nil)]
            (when (= "Setup" (get-in @trigger-prefs [:value :osc-send-on]))
              (when-let [t (first (get-triggers))] ; best-effort: send for first trigger row
                (send-osc-if-configured! t nil)))
            [ret nil]))
        (catch Throwable t
          (timbre/error t "Problem running global " kind " expression,"
                        (get-in data [:expressions kind]))
          (seesaw/alert (str "<html>Problem running global " (name kind) " expression.<br><br>" t)
                        :title "Exception in Custom Expression" :type :error)
          [nil t])))))

(defn- paint-state
  "Draws a representation of the state of the trigger, including both
  whether it is enabled and whether it has tripped (or would have, if
  it were not disabled)."
  [trigger c ^Graphics2D g]
  (let [w (double (seesaw/width c))
        h (double (seesaw/height c))
        outline (java.awt.geom.Ellipse2D$Double. 1.0 1.0 (- w 2.5) (- h 2.5))
        enabled? (enabled? trigger)
        state @(seesaw/user-data trigger)]
    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)

    (if (:tripped state)
      (do  ; Draw the inner filled circle showing the trigger is tripped
        (.setPaint g Color/green)
        (.fill g (java.awt.geom.Ellipse2D$Double. 4.0 4.0 (- w 8.0) (- h 8.0))))
      (when (:playing state)  ; Draw the inner gray circle showing it would trip if it were not disabled
        (.setPaint g Color/lightGray)
        (.fill g (java.awt.geom.Ellipse2D$Double. 4.0 4.0 (- w 8.0) (- h 8.0)))))

    ;; Draw the outer circle that reflects the enabled state
    (.setStroke g (java.awt.BasicStroke. 2.0))
    (.setPaint g (if enabled? Color/green Color/red))
    (.draw g outline)
    (when-not enabled?
      (.clip g outline)
      (.draw g (java.awt.geom.Line2D$Double. 1.0 (- h 1.5) (- w 1.5) 1.0)))))

(defn close-trigger-editors?
  "Tries closing all open expression editors for the trigger. If
  `force?` is true, simply closes them even if they have unsaved
  changes. Othewise checks whether the user wants to save any unsaved
  changes. Returns truthy if there are none left open the user wants
  to deal with."
  [force? trigger]
  (every? (partial editors/close-editor? force?) (vals (:expression-editors @(seesaw/user-data trigger)))))

(defn- cleanup-trigger
  "Process the removal of a trigger, either via deletion, or importing a
  different trigger on top of it. If `force?` is true, any unsaved
  expression editors will simply be closed. Otherwise, they will block
  the trigger removal, which will be indicated by this function
  returning falsey."
  [force? trigger]
  (when (close-trigger-editors? force? trigger)
    (run-trigger-function trigger :shutdown nil true)
    (seesaw/selection! (seesaw/select trigger [:#enabled]) "Never")  ; Ensures any clock thread stops.
    true))

(defn delete-trigger
  "Removes a trigger row from the window, running its shutdown function
  if needed, closing any editor windows associated with it, and
  readjusting any triggers that remain. If `force?` is true, editor
  windows will be closed even if they have unsaved changes, otherwise
  the user will be given a chance to cancel the operation. Returns
  truthy if the trigger was actually deleted."
  [force? trigger]
  (try
    (when (close-trigger-editors? force? trigger)
      (cleanup-trigger true trigger)
      (seesaw/config! (seesaw/select @trigger-frame [:#triggers])
                      :items (remove #(= % trigger) (get-triggers)))
      (adjust-triggers)
      (.pack ^JFrame @trigger-frame)
      true)
    (catch Exception e
      (timbre/error e "Problem deleting Trigger."))))

(declare update-global-expression-icons)

(defn- delete-all-triggers
  "Closes any global expression editors, then removes all non-show
  triggers, running their own shutdown functions, and finally runs the
  global offline (if we were online) and shutdown functions. If
  `force?` is true, editor windows will be closed even if they have
  unsaved changes, otherwise the user will be given a chance to cancel
  the operation. Returns truthy if the trigger was actually deleted."
  [force?]
  (when (and (every? (partial close-trigger-editors? force?) (get-triggers nil))
             (every? (partial editors/close-editor? force?) (vals (:expression-editors @trigger-prefs))))
    (doseq [trigger (get-triggers nil)]
      (delete-trigger true trigger))
    (when (util/online?) (run-global-function :offline))
    (run-global-function :shutdown)
    (reset! expression-globals {})
    (reset! trigger-prefs (initial-trigger-prefs))
    (update-global-expression-icons)
    true))

(defn- update-gear-icon
  "Determines whether the gear button for a trigger should be hollow
  or filled in, depending on whether any expressions have been
  assigned to it."
  [trigger gear]
  (prefs/update-gear-button gear (not-every? empty? (vals (:expressions @(seesaw/user-data trigger))))))

(declare export-trigger)
(declare import-trigger)
(declare format-trigger)

(defn- initial-trigger-user-data
  "Create the values to assign the user-data atom for a freshly
  created trigger."
  []
  {:creating true :playing false :tripped false :locals (atom {})})

(defn- load-trigger-from-map
  "Repopulate the content of a trigger row from a map as obtained from
  the preferences, a save file, or an export file."
  ([trigger m]
   (load-trigger-from-map trigger m (seesaw/select trigger [:#gear])))
  ([trigger m gear]
   (let [show (when-let [file (:show-file @(seesaw/user-data trigger))] (get (show-util/get-open-shows) file))]
     (reset! (seesaw/user-data trigger) (merge (initial-trigger-user-data)
                                               (when show {:show-file (:file show)})))
     (when-let [exprs (:expressions m)]
       (swap! (seesaw/user-data trigger) assoc :expressions exprs)
       (doseq [[kind expr] (editors/sort-setup-to-front exprs)]
         (let [editor-info (get editors/trigger-editors kind)]
           (try
             (swap! (seesaw/user-data trigger) assoc-in [:expression-fns kind]
                    (expressions/build-user-expression
                     expr (:bindings editor-info)
                     (merge {:description  (editors/triggers-editor-title kind trigger false)
                             :fn-sym       (editors/triggers-editor-symbol kind trigger false)
                             :raw-for-show show}
                            (select-keys editor-info [:nil-status? :no-locals?]))))
             (catch Exception e
               (swap! (seesaw/user-data trigger) assoc :expression-load-error true)
               (timbre/error e (str "Problem parsing " (:title editor-info)
                                    " when loading Triggers. Expression:\n" expr "\n"))))))))
   (seesaw/value! trigger m)
   (swap! (seesaw/user-data trigger) dissoc :creating)
   (let [[_ exception] (run-trigger-function trigger :setup nil false)]
     (when exception
       (swap! (seesaw/user-data trigger) assoc :expression-load-error true)))
   (update-gear-icon trigger gear)))

(defn cache-value
  "Make a copy of the values of the UI elements of a trigger into its
  user data map, so that it can safely be used by threads other than
  the Swing Event Dispatch thread. The alternative would be for other
  threads to use `invoke-now` to call functions on the Event Dispatch
  thread to read the values, but that would be far too slow for time
  sensitive high priority threads that are processing status and beat
  packets."
  [e]
  (let [trigger (.getParent (seesaw/to-widget e))]
    (swap! (seesaw/user-data trigger) assoc :value (seesaw/value trigger))))

;; ----- OSC (Open Sound Control) support: client caching, templating, and sending -----

(defonce osc-clients (atom {}))

(defn- ensure-osc-loaded! []
  (try (require 'overtone.osc)
       (catch Throwable _)))

(defn- broadcast-host?
  "Returns true if host appears to be a broadcast address (any octet is 255)."
  [host]
  (try
    (when (string? host)
      (let [octets (clojure.string/split host #"\\.")]
        (and (= 4 (count octets)) (some #(= % "255") octets))))
    (catch Throwable _ false)))

(defn- get-osc-client
  [host port]
  (ensure-osc-loaded!)
  (let [k [host (int port)]]
    (if-let [c (get @osc-clients k)]
      c
      (let [osc-client-fn (resolve 'overtone.osc/osc-client)
            c             (osc-client-fn host (int port))]
        (when (broadcast-host? host)
          (try
            (when-let [^java.net.DatagramSocket s (:socket c)]
              (.setBroadcast s true))
            (catch Throwable _)))
        (swap! osc-clients assoc k c)
        c))))

(defn- close-all-osc-clients []
  (ensure-osc-loaded!)
  (let [close-fn (resolve 'overtone.osc/osc-close)]
    (doseq [c (vals @osc-clients)]
      (try (close-fn c) (catch Throwable _)))
    (reset! osc-clients {})))

(defn- template-tokens
  "Extract placeholder names used in address and args strings."
  [addr args]
  (let [s (str (or addr "") "," (or args ""))]
    (into #{} (map second (re-seq #"\{([a-zA-Z0-9_-]+)\}" s)))))

(defonce ^{:private true}
  osc-placeholder-fns (atom {}))

(defn- ctx-fn-key [clazz tokens]
  [clazz (vec (sort tokens))])

(defn- build-ctx-fn
  "Builds and caches a function that returns a map of token->value using expressions bindings."
  [clazz tokens]
  (let [k (ctx-fn-key clazz tokens)]
    (if-let [f (get @osc-placeholder-fns k)]
      f
      (let [syms      (map symbol tokens)
            pairs     (mapcat (fn [s] [(str ":" (name s)) (name s)]) syms)
            form-str  (str "(hash-map " (clojure.string/join " " pairs) ")")
            bindings  (expressions/bindings-for-update-class clazz)
            f         (expressions/build-user-expression form-str bindings
                                                         {:description "OSC placeholders"
                                                          :fn-sym 'osc-placeholders
                                                          :nil-status? false
                                                          :no-locals? true})]
        (swap! osc-placeholder-fns assoc k f)
        f))))

(defn- osc-context
  "Build a map of values for placeholders using expressions convenience bindings."
  [evt tokens]
  (if (empty? tokens)
    {}
    (let [clazz (cond
                  (instance? org.deepsymmetry.beatlink.Beat evt) org.deepsymmetry.beatlink.Beat
                  (instance? CdjStatus evt)                        CdjStatus
                  :else                                            org.deepsymmetry.beatlink.DeviceUpdate)
          f     (build-ctx-fn clazz tokens)]
      (try
        (let [m (f evt {:locals nil})]
          (if (map? m) m {}))
        (catch Throwable _ {}))))
)

(defn- render-template
  "Replace {name} occurrences using values in ctx. Unknowns -> empty."
  [s ctx]
  (when s
    (clojure.string/replace s #"\{([a-zA-Z0-9_-]+)\}"
                            (fn [[_ k]] (str (get ctx (keyword k) ""))))))

(defn- send-osc-if-configured!
  "Send a single OSC message if host/port/address are present."
  [trigger status]
  (ensure-osc-loaded!)
  (let [cached (:value @(seesaw/user-data trigger))
        live   (try (seesaw/invoke-now (seesaw/value trigger)) (catch Throwable _ nil))
        val    (merge cached live)
        host   (:osc-host val)
        port   (:osc-port val)
        addr   (:osc-address val)
        args   (:osc-args val)]
    (when (and (seq host) port (pos? (int port)) (seq addr))
      (let [tokens (template-tokens addr args)
            ctx    (osc-context status tokens)
            path   (render-template addr ctx)
            client (get-osc-client host (int port))
            send-f (resolve 'overtone.osc/osc-send)
            parsed (->> (when (and args (not (clojure.string/blank? args)))
                          (clojure.string/split args #",") )
                        (map clojure.string/trim)
                        (remove clojure.string/blank?)
                        (map #(render-template % ctx))
                        (map (fn [s]
                               (let [t (:osc-arg-type val "string")] 
                                 (case t
                                   "int"   (try (int (Math/round (Double/parseDouble s))) (catch Throwable _ 0))
                                   "float" (try (float (Double/parseDouble s)) (catch Throwable _ (float 0)))
                                   s))) )
                        vec)]
        (try
          (timbre/debug "[OSC] sending" host port path parsed)
          (apply send-f client path parsed)
          (catch Throwable t
            (timbre/warn t "OSC send failed" host port path)))))))

(defn- missing-expression?
  "Checks whether the expression body of the specified kind is empty
  given a trigger panel."
  [trigger kind]
  (empty? (get-in @(seesaw/user-data trigger) [:expressions kind])))

(defn- simulate-enabled?
  "Checks whether the specified event type can be simulated for the
  given trigger. Always allow Activation/Deactivation; for Beat and
  Tracked Update, enable when an expression body exists."
  [trigger event]
  (case event
    :activation true
    :deactivation true
    :beat true
    :tracked true
    (not (missing-expression? trigger event))))

(defn- create-trigger-row
  "Create a row for watching a player in the trigger window.

  If `m` is supplied, it is a map containing values to recreate the
  row from a saved version.

  If `index` is supplied, it is the initial index to assign the
  trigger (so exceptions logged during load can be meaningful),
  otherwise 1 is assumed (and will get renumbered by
  `adjust-triggers`).

  If `show` is supplied, the trigger is being created as a raw trigger
  belonging to the specified show."
  ([]
   (create-trigger-row nil 1))
  ([m index]
   (create-trigger-row m index nil))
  ([m index show]
   (let [outputs (util/get-midi-outputs)
         gear    (seesaw/button :id :gear :icon (seesaw/icon (prefs/gear-icon false)))
         panel   (mig/mig-panel
                  :id :panel
                  :items [[(seesaw/label :id :from-show :text "Triggers from no Show." :visible? false :halign :center)
                           "hidemode 3, span, grow, wrap unrelated"]
                          [(seesaw/label :id :index :text (str index ".") :user-data (random-uuid)) "align right"]
                          [(seesaw/text :id :comment :paint (partial util/paint-placeholder "Comment"))
                           "span, grow, wrap"]

                          [gear]
                          ;; OSC Host, Watch, and Port on one row

                          ["Watch:" "alignx trailing"]
                          [(seesaw/combobox :id :players :model (get-player-choices)
                                            :listen [:item-state-changed cache-value])]

                          [(seesaw/label :text "OSC Host:") "alignx trailing"]
                          [(seesaw/text :id :osc-host :text "127.0.0.1" :columns 12)]

                          [(seesaw/label :text "Port:") "alignx trailing"]
                          [(seesaw/spinner :id :osc-port :model (seesaw/spinner-model 8000 :from 1 :to 65535))]
                          [(seesaw/label :id :status-spacer :text "")  "wrap, hidemode 3"]

                          ;; OSC configuration
                          [(seesaw/label :text "Address:")]
                          [(seesaw/text :id :osc-address :text "/blt/{device-number}" :columns 32) "span, grow, wrap"]

                          [(seesaw/label :text "Message:")]
                          [(seesaw/text :id :osc-args :text "{track-artist} - {track-title} - {effective-tempo}" :columns 32) "span, grow, wrap"]

                          ;; Type, Send on, and Enabled on one row
                          [(seesaw/label :text "Type:") "skip 1, alignx trailing"]
                          [(seesaw/combobox :id :osc-arg-type :model ["String" "Float" "Int"] :selected-item "string" :listen [:action-performed cache-value])]
                          [(seesaw/label :text "Send on:") "alignx trailing"]
                          [(seesaw/combobox :id :osc-send-on
                                            :model ["Never" "Activation" "Deactivation" "Beat" "Tracked Update" "Setup" "Shutdown"]
                                            :selected-item "Beat")]
                          [(seesaw/label :id :enabled-label :text "Enabled:") "alignx trailing"]
                          [(seesaw/combobox :id :enabled :model ["Never" "On-Air" "Custom" "Always"]
                                            :listen [:item-state-changed cache-value]) "hidemode 1"]
                          [(seesaw/canvas :id :state :size [18 :by 18] :opaque? false
                                          :tip "Trigger state: Outer ring shows enabled, inner light when tripped.")
                           "wrap, hidemode 1"]
                          ;; Status label at bottom, right aligned
                          [(seesaw/label :id :status :text "Checking...")  "span, wrap"]]

                  :user-data (atom (initial-trigger-user-data)))
         export-action  (seesaw/action :handler (fn [_] (export-trigger panel))
                                       :name "Export Trigger")
         import-action  (seesaw/action :handler (fn [_] (import-trigger panel))
                                       :name "Import Trigger")
         delete-action  (seesaw/action :handler (fn [_] (delete-trigger true panel))
                                       :name "Delete Trigger")
         inspect-action (seesaw/action :handler (fn [_] (try
                                                          (inspector/inspect @(:locals @(seesaw/user-data panel))
                                                                             :window-name "Trigger Expression Locals")
                                                          (catch StackOverflowError _
                                                            (util/inspect-overflowed))
                                                          (catch Throwable t
                                                            (util/inspect-failed t))))
                                       :name "Inspect Expression Locals"
                                       :tip "Examine any values set as Trigger locals by its Expressions.")
         report-actions (fn []
                          [(seesaw/action
                            :handler (fn [_]
                                       (when (help/help-server)
                                         (clojure.java.browse/browse-url
                                          (show-util/expression-report-link
                                           (:show-file @(seesaw/user-data panel))
                                           ((requiring-resolve 'beat-link-trigger.editors/triggers-report-tag) panel)))))
                            :name "View Expressions in Report")])
         editor-actions (fn []
                          (for [[kind spec] editors/trigger-editors]
                            (let [update-fn (fn []
                                              (when (= kind :setup) ; Clean up then run the new setup function
                                                (run-trigger-function panel :shutdown nil true)
                                                (reset! (:locals @(seesaw/user-data panel)) {})
                                                (run-trigger-function panel :setup nil true))
                                              (update-gear-icon panel gear))]
                              (seesaw/action :handler (fn [_] (editors/show-trigger-editor kind panel update-fn))
                                             :name (str "Edit " (:title spec))
                                             :tip (:tip spec)
                                             :icon (prefs/gear-icon (not (missing-expression? panel kind)))))))
         sim-actions    (fn []
                          [(seesaw/action :name "Activation"
                                          :enabled? (simulate-enabled? panel :activation)
                                          :handler (fn [_]
                                                     (binding [util/*simulating* (util/data-for-simulation)]
                                                       (let [status (show-util/random-cdj-status)]
                                                         (report-activation panel status @(seesaw/user-data panel) false)
                                                         (when (= "Activation" (get-in (:value @(seesaw/user-data panel)) [:osc-send-on]))
                                                           (send-osc-if-configured! panel status)))))
                           )
                           (seesaw/action :name "Beat"
                                          :enabled? (simulate-enabled? panel :beat)
                                          :handler (fn [_]
                                                     (binding [util/*simulating* (util/data-for-simulation)]
                                                       (let [beat (show-util/random-beat)]
                                                         (run-trigger-function panel :beat beat true)
                                                         (when (and (= "Beat" (get-in (:value @(seesaw/user-data panel)) [:osc-send-on]))
                                                                    (enabled? panel))
                                                           (send-osc-if-configured! panel beat))))))
                           (seesaw/action :name "Tracked Update"
                                          :enabled? (simulate-enabled? panel :tracked)
                                          :handler (fn [_]
                                                     (binding [util/*simulating* (util/data-for-simulation)]
                                                       (let [status (show-util/random-cdj-status)]
                                                         (run-trigger-function panel :tracked status true)
                                                         (when (and (= "Tracked Update" (get-in (:value @(seesaw/user-data panel)) [:osc-send-on]))
                                                                    (enabled? panel))
                                                           (send-osc-if-configured! panel status))))))
                           (seesaw/action :name "Deactivation"
                                          :enabled? (simulate-enabled? panel :deactivation)
                                          :handler (fn [_]
                                                     (binding [util/*simulating* (util/data-for-simulation)]
                                                       (let [status (show-util/random-cdj-status)]
                                                         (report-deactivation panel status @(seesaw/user-data panel) false)
                                                         (when (= "Deactivation" (get-in (:value @(seesaw/user-data panel)) [:osc-send-on]))
                                                           (send-osc-if-configured! panel status))))))])
         popup-fn       (fn [_] (concat (report-actions) (editor-actions)
                                        [(seesaw/separator) (seesaw/menu :text "Simulate" :items (sim-actions))
                                         inspect-action (seesaw/separator) import-action export-action]
                                        (when (or (:show-file @(seesaw/user-data panel))
                                                  (> (count (get-triggers nil)) 1))
                                          [delete-action])))]

     ;; Create our contextual menu and make it available both as a right click on the whole row, and as a normal
     ;; or right click on the gear button.
     (seesaw/config! [panel gear] :popup popup-fn)
     (seesaw/listen gear
                    :mouse-pressed (fn [e]
                                     (let [popup (seesaw/popup :items (popup-fn e))]
                                       (util/show-popup-from-button gear popup e))))
     (prefs/register-gear-button gear)

     ;; Attach the custom paint function to render the graphical trigger state
     (seesaw/config! (seesaw/select panel [:#state]) :paint (partial paint-state panel))

     ;; Add listener to update the cached values when the comment changes, since the trigger cannot be
     ;; found from the DocumentEvent as it can for the other events.
     (let [comment (seesaw/select panel [:#comment])]
       (seesaw/listen comment :document (fn [_] (cache-value comment))))

     ;; Update the trigger state when the enabled state changes, and open an editor window if Custom is
     ;; chosen and the enabled filter expression is empty.
     (let [enabled-menu (seesaw/select panel [:#enabled])]
       (seesaw/listen enabled-menu
                      :action-performed (fn [_]
                                          (seesaw/repaint! (seesaw/select panel [:#state]))
                                          (when (and (= "Custom" (seesaw/selection enabled-menu))
                                                     (empty? (get-in @(seesaw/user-data panel) [:expressions :enabled])))
                                            (editors/show-trigger-editor :enabled panel #(update-gear-icon panel gear))))))

     (seesaw/listen (seesaw/select panel [:#players])
                    :item-state-changed (fn [_] ; Update player status when selection changes
                                          (seesaw/invoke-later ; Make sure menu value cache update has happened.
                                            (swap! (seesaw/user-data panel) dissoc :status) ; Clear cached status.
                                            (show-device-status panel))))
     ;; Removed legacy MIDI/Link UI wiring.

     ;; Attach listeners for OSC fields to cache values
     (doseq [id [:osc-host :osc-address :osc-args]]
       (when-let [tf (seesaw/select panel [(keyword (str "#" (name id)))])]
         (seesaw/listen tf :document (fn [_] (cache-value tf)))))
     (when-let [cb (seesaw/select panel [:#osc-arg-type])]
       (seesaw/listen cb :action-performed (fn [_] (cache-value cb))))
     (when-let [sp (seesaw/select panel [:#osc-port])]
       (seesaw/listen sp :state-changed (fn [_] (cache-value sp))))
     (when-let [sendOn (seesaw/select panel [:#osc-send-on])]
       (seesaw/listen sendOn :action-performed (fn [_] (cache-value sendOn))))

    ;; If this trigger belongs to a show, record that fact.
    (when show (swap! (seesaw/user-data panel) assoc :show-file (:file show)))

     (when (some? m) ; If there was a map passed to us to recreate our content, apply it now
       (load-trigger-from-map panel m gear))
     (swap! (seesaw/user-data panel) dissoc :creating)
     (show-device-status panel)
     (cache-value gear) ; Cache the initial values of the choice sections
     panel)))

(defonce ^{:private true
           :doc     "The menu action which adds a new Trigger to the end of the list of non-show triggers."}
  new-trigger-action
  (delay
   (seesaw/action :handler (fn [_]
                             (let [non-show      (get-triggers nil)
                                   last-nonshow  (last non-show)
                                   m             (when last-nonshow (assoc (format-trigger last-nonshow) :enabled "Never"))
                                   new-index     (inc (count non-show))
                                   new-row       (create-trigger-row m new-index)]
                               (seesaw/config! (seesaw/select @trigger-frame [:#triggers])
                                               :items (concat non-show [new-row]
                                                              (filter #(some? (:show-file @(seesaw/user-data %)))
                                                                      (get-triggers))))
                               (adjust-triggers)))
                  :name "New Trigger"
                  :key "menu T")))

(defn create-trigger-for-show
  "Creates a new trigger that belongs to the show that was opened from
  the specified file. If `m` is supplied, it is a map containing the
  contents with which the trigger should be recreated. If the show's
  user data reports that it has a custom hue, the trigger is marked to
  use that hue as well."
  ([show]
   (create-trigger-for-show show {}))
  ([show m]
   (let [triggers        (get-triggers)
         triggers-before (take-while #(not= (:file show) (:show-file @(seesaw/user-data %))) triggers)
         show-triggers   (get-triggers show)
         trigger-index   (+ (count triggers-before) (count show-triggers) 1)
         new-trigger     (create-trigger-row m trigger-index show)
         triggers-after  (drop (dec trigger-index) triggers)]
     (when-let [show-hue (:show-hue (show/user-data show))]
       (swap! (seesaw/user-data new-trigger) assoc :show-hue show-hue))
     (seesaw/config! (seesaw/select @trigger-frame [:#triggers])
                     :items (concat triggers-before show-triggers [new-trigger] triggers-after))
     (adjust-triggers))))

(defn recolor-show-triggers
  "Called when a Global Setup expression has been run for a show. If the
  show hue has changed, we need to re-color all its triggers."
  [show dark?]
  (let [show-hue (:show-hue (show/user-data show))
        changed? (atom false)]
    (doseq [trigger (get-triggers show)]
      (when (not= (:show-hue (seesaw/user-data trigger)) show-hue)
        (swap! (seesaw/user-data trigger) assoc :show-hue show-hue)
        (reset! changed? true)))
    (when @changed? (adjust-triggers dark?))))

;; Carabiner UI entry removed with Link support.

(defonce ^{:private true
           :doc "The menu action which opens the nREPL configuration window."}
  nrepl-action
  (delay (seesaw/action :handler (fn [_] (nrepl/show-window @trigger-frame))
                        :name "nREPL: Clojure IDE Connection")))

(defonce ^{:private true
           :doc "The menu action which opens the Load Track window."}
  load-track-action
  (delay (seesaw/action :handler (fn [_] (track-loader/show-dialog))
                        :name "Load Track on Player" :enabled? false)))

(defonce ^{:private true
           :doc "The menu action which opens the Load Settings window."}
  load-settings-action
  (delay (seesaw/action :handler (fn [_] (settings-loader/show-dialog))
                        :name "Load Settings on Player" :enabled? false)))

(defonce ^{:private true
           :doc     "The menu action which empties the Trigger list."}
  clear-triggers-action
  (delay
   (seesaw/action :handler (fn [_]
                             (try
                               (let [^java.awt.Window confirm
                                     (seesaw/dialog :content (str "Clear Triggers?\n"
                                                                  "You will be left with one default Trigger.")
                                                    :type :warning :option-type :yes-no)]
                                 (.pack confirm)
                                 (.setLocationRelativeTo confirm @trigger-frame)
                                 (when (= :success (seesaw/show! confirm))
                                   (delete-all-triggers true)
                                   (seesaw/config! (seesaw/select @trigger-frame [:#triggers])
                                                   :items (concat [(create-trigger-row)] (get-triggers)))
                                   (adjust-triggers)
                                   (seesaw/dispose! confirm))
                                 (seesaw/dispose! confirm))
                               (catch Exception e
                                 (timbre/error e "Problem clearing Trigger list."))))
                  :name "Clear Triggers")))

(defn- format-trigger
  "Organizes the portions of a trigger which are saved or exported."
  [trigger]
  (-> (seesaw/value trigger)
             (dissoc :status :channel-label :enabled-label :index :from-show)
             (merge (when-let [exprs (:expressions @(seesaw/user-data trigger))]
                      {:expressions exprs}))))

(defn- export-trigger
  "Saves a single trigger to a file for exchange or archival
  purposes."
  [trigger]
  (let [extension (util/extension-for-file-type :trigger-export)]
    (when-let [file (chooser/choose-file @trigger-frame :type "Export"
                                         :all-files? false
                                         :filters [["Trigger Export files" [extension]]])]
      (when-let [file (util/confirm-overwrite-file file extension @trigger-frame)]
        (try
          (spit file (with-out-str (fipp/pprint {:beat-link-trigger-export (util/get-version)
                                                 :item                     (format-trigger trigger)})))
          (catch Exception e
            (seesaw/alert (str "<html>Unable to Export.<br><br>" e)
                          :title "Problem Writing File" :type :error)))))))

(defn trigger-configuration-for-show
  "Returns the list of trigger configurations for all triggers belonging
  to the specified show, so they can be saved along with the show when
  that is being saved."
  [show]
  (vec (for [trigger (get-triggers show)]
         (format-trigger trigger))))

(defn- trigger-configuration
  "Returns the current Trigger window configuration, so it can be
  saved and recreated."
  []
  (trigger-configuration-for-show nil))

(defn- save-triggers-to-preferences
  "Saves the current Trigger window configuration to the application
  preferences."
  []
  (prefs/put-preferences (merge (prefs/get-preferences)
                                {:triggers         (trigger-configuration)
                                 :window-positions @util/window-positions}
                                (when-let [exprs (:expressions @trigger-prefs)]
                                  {:expressions exprs})
                                (select-keys @trigger-prefs [:tracks-using-playlists? :send-status?]))))

(defonce ^{:private true
           :doc "The menu action which saves the configuration to the preferences."}
  save-action
  (delay (seesaw/action :handler (fn [_] (save-triggers-to-preferences))
                        :name "Save"
                        :key "menu S")))

(defonce ^{:private true
           :doc "The menu action which saves the configuration to a user-specified file."}
  save-as-action
  (delay
   (seesaw/action :handler (fn [_]
                             (when (save-triggers-to-preferences)
                               (let [extension (util/extension-for-file-type :configuration)]
                                 (when-let [file (chooser/choose-file @trigger-frame :type :save
                                                                      :all-files? false
                                                                      :filters [["Beat Link Trigger configuration files"
                                                                                 [extension]]])]
                                   (when-let [file (util/confirm-overwrite-file file extension @trigger-frame)]
                                     (try
                                       (prefs/save-to-file file)
                                       (catch Exception e
                                         (seesaw/alert (str "<html>Unable to Save.<br><br>" e)
                                                       :title "Problem Writing File" :type :error))))))))
                  :name "Save to File")))

(declare recreate-trigger-rows)

(defn check-for-parse-error
  "Called after loading the triggers from a file or the preferences to
  see if there were problems parsing any of the custom expressions. If
  so, reports that to the user and clears the warning flags."
  ([]
   (check-for-parse-error nil))
  ([show]
   (let [failed (filter identity (for [trigger (get-triggers show)]
                                   (when (:expression-load-error @(seesaw/user-data trigger))
                                     (swap! (seesaw/user-data trigger) dissoc :expression-load-error)
                                     (editors/trigger-index trigger))))]
     (when (seq failed)
       (seesaw/alert (str "<html>Unable to use an expression for Trigger "
                          (clojure.string/join ", " failed) ".<br><br>"
                          "Check the log file for details.")
                     :title "Exception during Clojure evaluation" :type :error)))))

(defonce ^{:private true
           :doc "The menu action which loads the configuration from a user-specified file."}
  load-action
  (delay
   (seesaw/action :handler (fn [_]
                             (let [extension (util/extension-for-file-type :configuration)]
                               (when-let [file (chooser/choose-file
                                                @trigger-frame
                                                :all-files? false
                                                :filters [["Beat Link Trigger configuration files" [extension]]
                                                          (chooser/file-filter "All files" (constantly true))])]
                                 (try
                                   (when (delete-all-triggers false)
                                     (prefs/load-from-file file)
                                     (seesaw/config! (seesaw/select @trigger-frame [:#triggers])
                                                     :items (concat (recreate-trigger-rows) (get-triggers)))
                                     (adjust-triggers)
                                     (when (util/online?)
                                       (run-global-function :online)
                                       (settings/run-online-actions @trigger-frame expression-globals)))
                                   (catch Exception e
                                     (timbre/error e "Problem loading" file)
                                     (seesaw/alert (str "<html>Unable to Load.<br><br>" e)
                                                   :title "Problem Reading File" :type :error)))
                                 (check-for-parse-error))))
                  :name "Load from File"
                  :key "menu L")))



(defn- rebuild-all-device-status
  "Updates all player status descriptions to reflect the devices
  currently found on the network. Called when the set of available
  devices changes.

  Also potentially change whether the MetadataFinder is allowed to use
  dbserver queries for metadata when we are not using a real player
  number, based on whether we now see any players unable to handle
  that."
  []
  (doseq [trigger (get-triggers)]
    (show-device-status trigger))
  (try
    (when-not (real-player?)
      (.setPassive metadata-finder (and (> (.getDeviceNumber virtual-cdj) 4)
                                        (.isAnyDeviceLimitedToThreeDatabaseClients device-finder))))
    (catch Throwable t
      (timbre/error t "Problem setting metadata passive mode"))))

(defonce ^{:private true
           :doc "Responds to player status packets and updates the
  state of any triggers watching them."}
  status-listener
  (reify DeviceUpdateListener
    (received [_this status]
      (try
        (doseq [trigger (get-triggers)]
          (let [selection (get-in @(seesaw/user-data trigger) [:value :players])]
            (when (instance? CdjStatus status)
              (timbre/info "[Trigger] status" :device (.getDeviceNumber ^CdjStatus status)
                           :playing (.isPlaying ^CdjStatus status)
                           :selection selection
                           :matches (matching-player-number? status trigger selection)))
            (when (and (instance? CdjStatus status) (matching-player-number? status trigger selection))
              (let [^CdjStatus status status
                    p1 (.getPlayState1 status)
                    p2 (.getPlayState2 status)
                    effective-playing (or (.isPlaying status)
                                          (= p2 org.deepsymmetry.beatlink.CdjStatus$PlayState2/MOVING)
                                          (= p2 org.deepsymmetry.beatlink.CdjStatus$PlayState2/OPUS_MOVING)
                                          (= p1 org.deepsymmetry.beatlink.CdjStatus$PlayState1/PLAYING)
                                          (= p1 org.deepsymmetry.beatlink.CdjStatus$PlayState1/LOOPING)
                                          (= p1 org.deepsymmetry.beatlink.CdjStatus$PlayState1/CUE_PLAYING))]
                (when-not (neg? (:number selection))
                  (run-custom-enabled status trigger)) ; This was already done if Any Player is the selection
                (update-player-state trigger effective-playing (.isOnAir status) status)
                (seesaw/invoke-later
                 (let [status-label (seesaw/select trigger [:#status])
                       track-description (:track-description @(:locals @(seesaw/user-data trigger)))
                       metadata-summary (:metadata-summary @(:locals @(seesaw/user-data trigger)))]
                   (seesaw/config! status-label :foreground (:valid-status-color @theme-colors))
                   (seesaw/value! status-label (build-status-label status track-description metadata-summary))))))))
        (catch Exception e
          (timbre/error e "Problem responding to Player status packet."))))))

(defonce ^{:private true
           :doc     "Responds to beat packets and runs any registered beat
  expressions, and performs beat alignment for any tripped triggers
  that are assigned to Ableton Link."}
  beat-listener
  (reify BeatListener
    (newBeat [_this beat]
      (try
        (doseq [trigger (get-triggers)]
          (let [data      @(seesaw/user-data trigger)
                value     (:value data)
                selection (:players value)]
            (timbre/info "[Trigger] beat" :device (.getDeviceNumber beat)
                         :selection selection
                         :tracked (= (get-in data [:last-match 1]) (.getDeviceNumber beat)))
            (when (and (some? selection)
                       (or (= (:number selection) (.getDeviceNumber beat))
                           (and (zero? (:number selection))
                                (or (and (.isRunning virtual-cdj) (.isTempoMaster beat))
                                    (when-let [simulator (sim/for-player (.getDeviceNumber beat))]
                                      (:master simulator))))
                           (and (neg? (:number selection))  ; For Any Player, make sure beat's from the tracked player.
                                (= (get-in data [:last-match 1]) (.getDeviceNumber beat)))))
              (run-trigger-function trigger :beat beat false)
              (when (and (= "Beat" (get-in (:value @(seesaw/user-data trigger)) [:osc-send-on]))
                         (enabled? trigger))
                (send-osc-if-configured! trigger beat))
              (when (and (:tripped data) (= "Link" (:message value)) (carabiner/sync-triggers?))
                (carabiner/beat-at-time (long (/ (.getTimestamp beat) 1000))
                                        (when (:bar value) (.getBeatWithinBar beat)))))))
        (catch Exception e
          (timbre/error e "Problem responding to beat packet."))))))

(def ^Long offline-cooldown-ms
  "The amount of time to wait for things to stabilize after starting the
  process of going offline, before trying to go back online
  automatically."
  500)

(defonce ^{:private true
           :doc "Responds to the arrival or departure of DJ Link
  devices by updating our user interface appropriately. If we were
  online and lost the last device, drop offline and then switch to
  trying to go back online until the user cancels that, for reliable
  headless operation."}
  device-listener
  (reify DeviceAnnouncementListener
    (deviceFound [_this _announcement]
      (rebuild-all-device-status))
    (deviceLost [_this _announcement]
      (rebuild-all-device-status)
      (when (and (util/online?) (empty? (.getCurrentDevices device-finder)))
        ;; We are online but lost the last DJ Link device. Switch back to looking for the network.
        (future
          (seesaw/invoke-now  ; Go offline.
           (.setSelected (online-menu-item) false))
          (Thread/sleep offline-cooldown-ms)  ; Give things a chance to stabilize.
          (seesaw/invoke-now  ; Finally, start trying to go back online, unless/until the user decides to give up.
           (.setSelected (online-menu-item) true)))))))

(declare go-offline)

(defonce ^{:private true
           :doc "Used to detect the `VirtualCdj` shutting down
           unexpectedly due to network problems, so we can
           recognize that we are offline and try to recover."}
  vcdj-lifecycle-listener
  (reify LifecycleListener
    (started [_this _]) ; Nothing to do.
    (stopped [_this _]
      (future
        (go-offline true) ; Indicate this is a special case of going offline even though VirtualCdj is offline already.
        (seesaw/invoke-now
         ;; Update the Online menu state, which calls `go-offline` again but that is a no-op this time.
         (.setSelected (online-menu-item) false))
        (Thread/sleep offline-cooldown-ms)  ; Give things a chance to stabilize.
        (seesaw/invoke-now  ; Finally, start trying to go back online, unless/until the user decides to give up.
         (.setSelected (online-menu-item) true))))))

(defn- translate-enabled-values
  "Convert from the old true/false model of enabled stored in early
  preference file versions to the new choices so they load correctly."
  [trigger]
  (if-some [enabled (:enabled trigger)]
    (assoc trigger :enabled (case enabled
                              true "Always"
                              false "Never"
                              enabled))
    trigger))

(defn- translate-custom-enabled
  "Convert from the old format for storing a single custom expression
  into the new extensible approach, so we can read old preference file
  versions."
  [trigger]
  (merge (translate-enabled-values trigger)
         (when-let [expr (:custom-enabled trigger)]
           {:expressions {:enabled expr}})))

(defn- import-trigger
  "Replaces the content of a single trigger with a previously exported
  version, preserving the trigger's relationship to its parent show,
  if it has one."
  [trigger]
  (let [extension (util/extension-for-file-type :trigger-export)]
    (when-let [file (chooser/choose-file
                     @trigger-frame
                     :all-files? false
                     :filters [["Trigger Export files" [extension]]
                               (chooser/file-filter "All files" (constantly true))])]
      (try
        (cleanup-trigger true trigger)
        (let [m    (prefs/read-file :beat-link-trigger-export file)
              show (:show-file @(seesaw/user-data trigger))]
          (load-trigger-from-map trigger (translate-custom-enabled (:item m)))
          (when show (swap! (seesaw/user-data trigger) assoc :show-file show)))
        (catch Exception e
          (timbre/error e "Problem importing" file)
          (seesaw/alert (str "<html>Unable to Import.<br><br>" e)
                        :title "Problem Importing Trigger" :type :error)))
      (check-for-parse-error))))

(defn- recreate-trigger-rows
  "Reads the preferences and recreates any trigger rows that were
  specified in them. If none were found, returns a single, default
  trigger. Also updates the global setup and shutdown expressions,
  running them as needed, and sets the default track description."
  []
  (let [m (prefs/get-preferences)]
    (.doClick ^JRadioButtonMenuItem (seesaw/select @trigger-frame [(if (:tracks-using-playlists? m)
                                                                     :#track-position :#track-id)]))
    (.setSelected ^JMenuItem (seesaw/select @trigger-frame [:#send-status]) (true? (:send-status? m)))
    (when-let [exprs (:expressions m)]
      (swap! trigger-prefs assoc :expressions exprs)
      (doseq [[kind expr] (editors/sort-setup-to-front exprs)]
        (let [editor-info (get editors/global-trigger-editors kind)]
          (try
            (swap! trigger-prefs assoc-in [:expression-fns kind]
                   (if (= kind :shared)
                     (expressions/define-shared-functions expr (editors/triggers-editor-title kind nil true))
                     (expressions/build-user-expression
                      expr (:bindings editor-info)
                      (merge {:description (editors/triggers-editor-title kind nil true)
                              :fn-sym      (editors/triggers-editor-symbol kind nil true)}
                             (select-keys editor-info [:nil-status? :no-locals?])))))
            (catch Exception e
              (timbre/error e (str "Problem parsing " (:title editor-info)
                                   " when loading Triggers. Expression:\n" expr "\n"))
              (seesaw/alert (str "<html>Unable to use " (:title editor-info) ".<br><br>"
                                 "Check the log file for details.")
                            :title "Exception during Clojure evaluation" :type :error)))))
      (run-global-function :setup))
    (update-global-expression-icons)
    (let [triggers (:triggers m)]
      (if (seq triggers)
        (vec (map-indexed (fn [index trigger]
                            (create-trigger-row (translate-custom-enabled trigger) (inc index)))
                          triggers))
        [(create-trigger-row)]))))

(defn global-editor-update-fn
  "Returns the appropriate update function for a global expression
  editor of the specified kind."
  [kind]
  (fn []
    (when (= :setup kind)
      (run-global-function :shutdown)
      (reset! expression-globals {})
      (run-global-function :setup))
    (update-global-expression-icons)))

(defn build-global-editor-action
  "Creates an action which edits one of the global expressions."
  [kind]
  (seesaw/action :handler (fn [_] (editors/show-trigger-editor
                                   kind (seesaw/config @trigger-frame :content)
                                   (global-editor-update-fn kind)))
                 :name (str "Edit " (get-in editors/global-trigger-editors [kind :title]))
                 :tip (get-in editors/global-trigger-editors [kind :tip])
                 :icon (prefs/gear-icon (seq (get-in @trigger-prefs [:expressions kind])))))

(defn- show-player-status-handler
  "Try to show the player status window, giving the user appropriate
  feedback if the current environment is not appropriate, or even not
  ideal. A Seesaw event handler, but we ignore the event argument."
  [_]
  (if (.isRunning virtual-cdj)
    (players/show-window @trigger-frame expression-globals)
    (seesaw/alert "Must be Online to show Player Status window."
                  :title "Beat Link Trigger is Offline" :type :error)))

(defonce ^{:private true
           :doc "The menu action which opens the Player Status window."}
  player-status-action
  (delay (seesaw/action :handler show-player-status-handler
                        :name "Show Player Status"
                        :key "menu P"
                        :enabled? false)))

(defn show-player-status
  "Try to show the player status window, giving the user appropriate
  feedback if the current environment is not appropriate, or even not
  ideal. Ensures the use of the Event Dispatch Thread so that UI
  elements can be created safely, and does nothing if called before
  the Triggers window has been created."
  []
  (when @trigger-frame
    (seesaw/invoke-later (show-player-status-handler nil))))

(defonce ^{:private true
           :doc "The menu action which opens the Playlist Writer window."}
  playlist-writer-action
  (delay (seesaw/action :handler (fn [_]
                                   (if (.isRunning virtual-cdj)
                                     (writer/show-window @trigger-frame)
                                     (seesaw/alert "Must be Online to show Playlist Writer window."
                                                   :title "Beat Link Trigger is Offline" :type :error)))
                        :name "Write Playlist" :enabled? false)))

(defonce ^{:private true
           :doc "The menu action which allows creation of a metadata archive from rekordbox media."}
  archive-metadata-action
  (delay (seesaw/action :handler (fn [_] (track-loader/create-metadata-archive @trigger-frame))
                        :name "Archive Metadata" :enabled? true
                        :tip "Summarize a rekordbox usb to a file for working with an Opus Quad.")))

(defonce ^{:private true
           :doc "The action which opens the OBS overlay web server window."}
  overlay-server-action
  (delay (seesaw/action :handler (fn [_]
                                   (overlay/show-window @trigger-frame))
                        :name "OBS Overlay Web Server" :enabled? true)))

(defonce ^{:private true
           :doc "The menu action which opens a shallow playback simulator."}
  open-simulator-item
  (delay (let [item (seesaw/menu-item :visible? (not (util/online?)))]
           (seesaw/config! item :action (sim/build-simulator-action item))
           item)))

(defn- actively-send-status
  "Try to start sending status update packets if we are online and are
  using a valid player number (and are talking to actual CDJs, not an
  Opus Quad). If we are not using a valid player number, tell the user
  why this can't be done.

  When we are sending status update packets, we are also able to
  actively request metadata of all types from the dbserver instances
  running on the other players, so we can request things like CD-Text
  based information that Crate Digger can't obtain."
  []
  (when (util/online?)
    (if (.inOpusQuadCompatibilityMode virtual-cdj)
      (do
        (seesaw/alert @trigger-frame (str "<html>You requested Beat Link Trigger to use a real player number.<br>"
                                          "Features which require sending CDJ status packets, like tempo control,<br>"
                                          "cannot be used with the Opus Quad. This request is being canceled.")
                       :title "Cannot Pose as CDJ with Opus Quad" :type :error)
        (.setSelected ^JMenuItem (seesaw/select @trigger-frame [:#send-status]) false))
      (if (> (.getDeviceNumber virtual-cdj) 4)
        (let [players (count (util/visible-player-numbers))
              options (to-array ["Cancel" "Go Offline"])
              message (str "Beat Link Trigger is using device number " (.getDeviceNumber virtual-cdj)
                           ".\nTo act like a real player, it needs to use number 1, 2, 3, or 4.\n\n"
                           (if (< players 4)
                             (str "Since there are fewer than 4 CDJs on the network, all you need to do is\n"
                                  "go offline and then back online, and it will be able to use one of the\n"
                                  "unused device numbers, which will work great.\n\n")

                             (str "Please go offline, turn off one of the four CDJs currently on the network,\n"
                                  "then go back online, which will let us use that player's device number.\n\n")))
              choice (seesaw/invoke-now
                       (javax.swing.JOptionPane/showOptionDialog
                        nil message "Need to Change Device Number"
                        javax.swing.JOptionPane/YES_NO_OPTION javax.swing.JOptionPane/ERROR_MESSAGE nil
                        options (aget options (dec (count options)))))]
          (if (zero? choice)
            (.setSelected ^JMenuItem (seesaw/select @trigger-frame [:#send-status]) false) ; Cancel.
            (.setSelected (online-menu-item) false))) ; Go offline.
        (do (.setSendingStatus virtual-cdj true)      ; We can do it.
            (.setPassive metadata-finder false))))))

(defn- actively-request-metadata-from-cdj-3000s
  "Even if we are not using a real player number, if the only other
  players on the network are CDJ-3000s, we can still actively request
  metadata using the dbserver protocol."
  []
  (when (or (< (.getDeviceNumber virtual-cdj) 5)
            (not (.isAnyDeviceLimitedToThreeDatabaseClients device-finder)))
    (.setPassive metadata-finder false)))

(declare go-online)

(defn expression-report-link
  "Returns the URL that can be used to open the triggers expression report."
  ([]
   (expression-report-link nil))
  ([anchor]
   (let [port           (help/help-server)
         anchor-segment (when anchor (str "#" anchor))]
     (str "http://127.0.0.1:" port "/show/reports/expressions" anchor-segment))))

(defn- online-menu-name
  "Expands the content of the Online? menu option to show the current
  player number if we are online."
  []
  (str "Online?"
       (when (util/online?)
         (str "  [We are Player " (.getDeviceNumber virtual-cdj) "]"))))

(defonce ^{:doc "Controls whether buttons in the expressions report are allowed to
  affect the Triggers window."}
  report-actions-enabled?
  (atom false))

(defn- build-trigger-menubar
  "Creates the menu bar for the trigger window."
  []
  (let [inspect-action   (seesaw/action :handler (fn [_] (try
                                                           (inspector/inspect @expression-globals
                                                                              :window-name "Trigger Expression Globals")
                                                           (catch StackOverflowError _
                                                             (util/inspect-overflowed))
                                                           (catch Throwable t
                                                             (util/inspect-failed t))))
                                        :name "Inspect Expression Globals"
                                        :tip "Examine any values set as globals by any Trigger Expressions.")
        new-show-action  (seesaw/action :handler (fn [_] (show/new @trigger-frame))
                                        :name "New Show"
                                        :tip "Create an interface for conveniently assigning cues to tracks."
                                        :key "menu N")
        open-show-action (seesaw/action :handler (fn [_] (show/open @trigger-frame))
                                        :name "Open Show"
                                        :tip "Opens an already-created show interface."
                                        :key "menu O")
        ex-report-action (seesaw/action :handler (fn [_]
                                                   (when (help/help-server)
                                                     (clojure.java.browse/browse-url (expression-report-link))))
                                        :name "Expression Report"
                                        :tip "Open a web page describing all expressions in the show.")
        actions-item     (seesaw/checkbox-menu-item :text "Enable Report Actions"
                                                    :tip (str "Allow buttons in reports to affect the triggers: "
                                                              "use only on secure networks."))
        using-playlists? (:tracks-using-playlists? @trigger-prefs)
        online-item      (seesaw/checkbox-menu-item :text (online-menu-name) :id :online :selected? (util/online?))
        real-item        (seesaw/checkbox-menu-item :text "Use Real Player Number?" :id :send-status
                                                    :selected? (real-player?))
        bg               (seesaw/button-group)
        track-submenu    (seesaw/menu :text "Default Track Description"
                                      :items [(seesaw/radio-menu-item :text "recordbox id [player:slot]" :id :track-id
                                                                      :selected? (not using-playlists?) :group bg)
                                              (seesaw/radio-menu-item :text "playlist position" :id :track-position
                                                                      :selected? using-playlists? :group bg)])]
    (seesaw/listen bg :selection
                   (fn [_]
                     (when-let [s (seesaw/selection bg)]
                       (swap! trigger-prefs assoc :tracks-using-playlists? (= (seesaw/id-of s) :track-position)))))
    (seesaw/listen actions-item :item-state-changed
                   (fn [^java.awt.event.ItemEvent e]
                     (reset! report-actions-enabled? (= (.getStateChange e) java.awt.event.ItemEvent/SELECTED))))
    (seesaw/listen online-item :item-state-changed
                   (fn [^java.awt.event.ItemEvent e]
                     (if (= (.getStateChange e) java.awt.event.ItemEvent/SELECTED)
                       (go-online)
                       (go-offline))))
    (seesaw/listen real-item :item-state-changed
                   (fn [^java.awt.event.ItemEvent e]
                     (swap! trigger-prefs assoc :send-status? (= (.getStateChange e)
                                                                 java.awt.event.ItemEvent/SELECTED))
                     (if (real-player?)
                       (actively-send-status)
                       (do
                         (carabiner/cancel-full-sync)
                         (.setSendingStatus virtual-cdj false)))))
    (seesaw/menubar :items [(seesaw/menu :text "File"
                                         :items (concat [@save-action @save-as-action @load-action
                                                         (seesaw/separator) new-show-action open-show-action
                                                         (seesaw/separator) ex-report-action actions-item
                                                         (seesaw/separator) @playlist-writer-action
                                                         (seesaw/separator) @archive-metadata-action]
                                                        (menus/extra-file-actions quit settings)))
                            (seesaw/menu :text "Triggers"
                                         :items (concat [@new-trigger-action (seesaw/separator)]
                                                        (map build-global-editor-action (keys editors/global-trigger-editors))
                                                        [(seesaw/separator)
                                                         track-submenu inspect-action
                                                         (seesaw/separator) @clear-triggers-action])
                                         :id :triggers-menu)

                            (seesaw/menu :text "Network"
                                         :items [online-item real-item
                                                 (seesaw/separator)
                                                 @open-simulator-item
                                                 @player-status-action @load-track-action @load-settings-action
                                                 (seesaw/separator)
                                                 @overlay-server-action @nrepl-action]
                                         :id :network-menu)
                            (menus/build-help-menu)])))

(defn trigger-global-expressions
  "Returns the current state of the Triggers window global expressions
  source code."
  []
  (:expressions @trigger-prefs))

(defn update-global-expression-icons
  "Updates the icons next to expressions in the Trigger menu to reflect
  whether they have been assigned a non-empty value. If the user
  preferences have already been loaded, they can be passed as an
  argument to prevent redundant work."
  ([]
   (update-global-expression-icons (prefs/get-preferences)))
  ([preferences]
   (let [^JMenu menu (seesaw/select @trigger-frame [:#triggers-menu])
         exprs       {"Edit Shared Functions"           :shared
                      "Edit Global Setup Expression"    :setup
                      "Edit Came Online Expression"     :online
                      "Edit Going Offline Expression"   :offline
                      "Edit Global Shutdown Expression" :shutdown}]
     (doseq [i (range (.getItemCount menu))]
       (let [^JMenuItem item (.getItem menu i)]
         (when item
           (when-let [expr (get exprs (.getText item))]
             (.setIcon item (prefs/gear-icon (seq (get (trigger-global-expressions) expr)) preferences)))))))))

(defn- ui-theme-changed
  "Called whenever the user interface theme has been changed, or dark
  mode has been entered or exited. Updates the window's interface to
  be readable in the new theme."
  [_root dark? preferences]
  (adjust-triggers dark?)
  (update-global-expression-icons preferences))

(defn- create-trigger-window
  "Create and show the trigger window."
  []
  (try
    (println "[BLT] create-trigger-window: building frame")
    (let [root           (seesaw/frame :title "Beat Link Triggers" :on-close :nothing
                                       :menubar (build-trigger-menubar))
          triggers       (seesaw/vertical-panel :id :triggers)
          panel          (seesaw/scrollable triggers :user-data trigger-prefs)
          theme-callback (partial ui-theme-changed root)]
      (seesaw/config! root :content panel)
      (reset! trigger-frame root)
      (seesaw/config! triggers :items (recreate-trigger-rows))
      (adjust-triggers)
      ;; Pack to preferred size so the window isn't wider than needed on first run
      (seesaw/pack! root)
      (util/restore-window-position root :triggers nil)
      (println "[BLT] create-trigger-window: showing frame")
      (seesaw/show! root)
      (println "[BLT] create-trigger-window: frame shown")
      (check-for-parse-error)
      (prefs/register-ui-change-callback theme-callback)
      (seesaw/listen root
                     :window-closing
                     (fn [_]
                       (save-triggers-to-preferences)
                       (if (and (show/close-all-shows false)
                                (delete-all-triggers false))
                         (do
                           (prefs/unregister-ui-change-callback theme-callback)
                           (writer/close-window)
                           (when (beat-carabiner/active?)
                             (beat-carabiner/disconnect)
                             (Thread/sleep 250)) ; Give any spawned daemon time to exit gracefully.
                           (menus/respond-to-quit-request true) ; In case it came from the OS
                           (System/exit 0))
                         (menus/respond-to-quit-request false)))

                     #{:component-moved :component-resized}
                     (fn [_] (util/save-window-position root :triggers))))

    (catch Exception e
      (println "[BLT] create-trigger-window: exception" e)
      (timbre/error e "Problem creating Trigger window."))))

(defn- reflect-online-state
  "Updates the File and Network menus so they are appropriate for
  whether we are currently online or not. If online, we show the
  player number in the `Online?` option, and enable the options which
  require that state (but we hide the Simulator option). Otherwise we
  disable the online-dependent options and show the Simulator option."
  []
  (seesaw/invoke-soon
   (try
     (seesaw/config! [@playlist-writer-action @load-track-action @load-settings-action @player-status-action]
                     :enabled? (util/online?))
     (.setText (online-menu-item) (online-menu-name))
     (if (util/online?)
       (seesaw/hide! @open-simulator-item)
       (seesaw/show! @open-simulator-item))
     (catch Throwable t
       (timbre/error t "Problem updating interface to reflect online state")))))

(defn- start-other-finders
  "Starts up the full complement of metadata-related finders that we
  use. Also updates the Online menu item to show our player number."
  []
  (.start metadata-finder)
  (.start (CrateDigger/getInstance))
  (.start (SignatureFinder/getInstance))
  (.start (ArtFinder/getInstance))
  (.start (BeatGridFinder/getInstance))
  (.start (TimeFinder/getInstance))
  (.setFindDetails (WaveformFinder/getInstance) true)
  (.start (WaveformFinder/getInstance))
  (.start (AnalysisTagFinder/getInstance))
  (reflect-online-state))

(defn start
  "Create the Triggers window, and register all the notification
  handlers it needs in order to stay up to date with events on the
  MIDI and DJ Link networks. When we are supposed to go online, try
  doing so, and run any custom Came Online expressions. If the window
  already exists, just bring it to the front, to support returning to
  online operation. Returns truthy if the window was created for the
  first time."
  []
  (println "[BLT] triggers.start entered")
  (let [already-created @trigger-frame]
    (if @trigger-frame
      (do
        (rebuild-all-device-status)
        (seesaw/show! @trigger-frame))
      (do
        ;; MIDI environment notifications removed.

        ;; Open the trigger window
        (swap! theme-colors assoc :valid-status-color
               (if (prefs/dark-mode?) Color/CYAN Color/BLUE))
        (create-trigger-window)
        (println "[BLT] triggers.start: create-trigger-window returned")

        ;; Be able to react to players coming and going
        (.addDeviceAnnouncementListener device-finder device-listener)
        (.addUpdateListener virtual-cdj status-listener)
        (rebuild-all-device-status)  ; In case any came or went while we were setting up the listener
        (.addBeatListener (BeatFinder/getInstance) beat-listener)))  ; Allow triggers to respond to beats
    (when (util/online?)
      (try
        (.start (BeatFinder/getInstance))
        (catch java.net.BindException e
          (timbre/error e "Unable to start Beat Finder, is rekordbox or another instance running? Staying offline.")
          (seesaw/invoke-now
           (seesaw/alert @trigger-frame
                         (str "<html>Unable to listen for beat packets, socket is in use.<br>"
                              "Is rekordbox or another DJ Link program running?")
                         :title "Failed to Go Online" :type :error)
           (.stop virtual-cdj)
           (.setSelected (online-menu-item) false)))
        (catch Throwable t
          (timbre/error t "Problem starting Beat Finder, staying offline.")
          (seesaw/invoke-now
           (seesaw/alert @trigger-frame
                         (str "<html>Unable to listen for beat packets, check the log file for details.<br><br>" t)
                         :title "Problem Trying to Go Online" :type :error)
           (.stop virtual-cdj)
           (.setSelected (online-menu-item) false)))))
    (.setPassive metadata-finder true)  ; Start out conservatively
    (when (util/online?)
      (start-other-finders)
      (.addLifecycleListener virtual-cdj vcdj-lifecycle-listener))  ; React when VirtualCdj shuts down unexpectedly.
    (if (real-player?)
      (actively-send-status)
      (actively-request-metadata-from-cdj-3000s))
    (when (util/online?)
      (run-global-function :online)
      (settings/run-online-actions @trigger-frame expression-globals)
      (show/run-show-online-expressions))

    (not already-created)))  ; Indicate whether this was the first creation of the Triggers window

(defn go-offline
  "Transition to an offline state, running any custom Going Offline
  expression and then updating the UI appropriately. If `was-online?`
  is passed, it reports whether we had been online. In normal
  circumstances, this does not need to be passed, and our current
  online state is checked. However, in the special case of reacting to
  the `VirtualCdj` unexpectedly shutting itself down due to network
  problems, we want to be able to run expressions and do proper
  cleanup."
  ([]
   (go-offline (util/online?)))
  ([was-online?]
   (.removeLifecycleListener virtual-cdj vcdj-lifecycle-listener)  ; No longer care when it stops.
   (when was-online?  ; Don't do all this if we got here from a failed attempt to go online.
     (show/run-show-offline-expressions)
     (run-global-function :offline)
     (.stop (WaveformFinder/getInstance))
     (.stop (BeatGridFinder/getInstance))
     (.stop (ArtFinder/getInstance))
     (.stop metadata-finder)
     (.stop (BeatFinder/getInstance))
     (.stop (org.deepsymmetry.beatlink.dbserver.ConnectionManager/getInstance))
     (.stop virtual-cdj)
     (Thread/sleep 200))  ; Wait for straggling update packets
     (close-all-osc-clients)
   (reflect-online-state)
   (rebuild-all-device-status)))

(defn go-online
  "Try to transition to an online state, updating the UI appropriately."
  []
  (future
    (seesaw/invoke-now
     (seesaw/hide! @trigger-frame)
     (sim/close-all-simulators))
    ((resolve 'beat-link-trigger.core/try-going-online))
    (when-not (util/online?)
      (seesaw/invoke-now  ; We failed to go online, so update the menu to reflect that.
       (.setSelected (online-menu-item) false)))))
