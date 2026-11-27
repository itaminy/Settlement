(require '[clojure.java.io :as io]
         '[settlement.bandits :as bandits]
         '[settlement.building-ui :as building-ui]
         '[settlement.client :as client]
         '[settlement.gui :as gui]
         '[settlement.layout :as layout]
         '[settlement.rendering :as rendering]
         '[settlement.server :as server]
         '[settlement.transport :as transport]
         '[settlement.world :as world])
(import '[java.awt Color Dimension]
        '[java.awt.event MouseEvent]
        '[java.awt.image BufferedImage]
        '[java.net InetAddress ServerSocket Socket]
        '[java.util.concurrent TimeUnit]
        '[javax.imageio ImageIO]
        '[javax.swing JFrame JLabel JProgressBar SwingUtilities])

(def frame-dir (io/file (first *command-line-args*)))
(.mkdirs frame-dir)
(def player-id 7891)
(def fence [28 27])

(defn edt! [f]
  (let [out (promise)]
    (SwingUtilities/invokeAndWait #(deliver out (f)))
    @out))

(defn connect [port]
  (let [socket (doto (Socket. "127.0.0.1" port) (.setSoTimeout 5000))
        connection {:socket socket :reader (io/reader (.getInputStream socket) :encoding "UTF-8")
                    :writer (io/writer (.getOutputStream socket) :encoding "UTF-8")}]
    (assoc connection :queue (transport/create-queue #(client/request connection %) #(.close socket)))))

(defn build-demo-fence! [connection]
  (assert (= :ok (:status (client/request connection {:type :register :player-id player-id :name "Blue"
                                                      :client-kind :human}))))
  (world/change-player-items! player-id {:wood 2})
  (assert (= :ok (:status (client/request connection {:type :build :player-id player-id
                                                      :building-type :fence :position fence}))))
  (dosync (alter world/world assoc-in [:objects fence :hp] 20)))

(defn display! [connection]
  (let [window (JFrame. "Settlement - Bandits")
        terrain (gui/create-map-panel 29 26 player-id)
        health (doto (JLabel. "") (.setForeground (Color. 234 199 228)))
        ui (assoc (gui/create-resource-panel {}) :hp-label health)
        progress (doto (JProgressBar.) (.setVisible false))
        label (doto (JLabel. "") (.setForeground (Color. 240 230 193))
                (.setPreferredSize (Dimension. 600 40)))
        {:keys [panel]} (gui/create-game-content terrain ui progress label)
        builder (building-ui/install! terrain connection player-id label
                  (fn [state]
                    (gui/update-map terrain (:players state) (:objects state) (:bandits state))
                    (gui/update-player-panels! ui state player-id))
                  (constantly nil))
        timer (gui/start-map-updater terrain connection ui player-id)]
    (.add (:panel ui) health)
    (.setContentPane window panel)
    (.setSize window 784 539)
    (.setLocationRelativeTo window nil)
    (.setVisible window true)
    (.validate window)
    (gui/refresh! terrain connection ui player-id)
    {:window window :terrain terrain :panel panel :label label :timer timer :builder builder}))

(defn show-raider! []
  (loop [tries 100]
    (when-not (seq (:bandits @world/world))
      (assert (pos? tries))
      (Thread/sleep 10)
      (recur (dec tries))))
  (let [id (first (sort (for [[id npc] (:bandits @world/world) :when (= :blue (:watch-team npc))] id)))]
    (dosync
      (alter world/world update-in [:bandits id]
             assoc :route [[34 27] [29 27] [29 27]] :waypoint 1 :respawn-at (world/combat-time)))
    id))

(defn record! [{:keys [panel terrain label]} id]
  (let [start (System/nanoTime)
        frames (atom [])
        clicked (atom false)
        destroyed (atom false)
        width (edt! #(.getWidth panel))
        height (edt! #(.getHeight panel))]
    (dotimes [index 174]
      (let [remaining (- (+ start (* index 40000000)) (System/nanoTime))]
        (when (pos? remaining) (.sleep TimeUnit/NANOSECONDS remaining)))
      (let [image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)]
        (edt!
         (fn []
           (let [state (.getClientProperty terrain :settlement.gui/state)
                 hp (get-in state [:objects fence :hp])
                 npc (get-in state [:bandits id])
                 camera (.getClientProperty terrain :settlement.gui/camera)]
             (when (and (nil? hp) @destroyed (bandits/alive? npc)
                        (= 1 (bandits/distance [29 26] (bandits/position npc)))
                        (compare-and-set! clicked false true) camera)
               (.actionPerformed (.get (.getActionMap terrain) "attack-mode") nil)
               (let [[left top] (rendering/raster-origin camera)
                     sx (- (+ (* 64 (:x npc)) 32) left)
                     sy (- (+ (* 64 (:y npc)) 34) top)
                     event (MouseEvent. terrain MouseEvent/MOUSE_CLICKED 0 0 sx sy 1 false MouseEvent/BUTTON1)]
                 (.dispatchEvent terrain event)))
             (when (some? hp)
               (reset! destroyed true)
               (.setText label (str "Забор  " hp "/50 HP")))
             (when (and (nil? hp) @destroyed (not @clicked))
               (.setText label "Забор разрушен"))
             (let [graphics (.createGraphics image)]
               (try (.printAll panel graphics) (finally (.dispose graphics)))))))
        (swap! frames conj image)))
    (assert (nil? (get-in @world/world [:objects fence])) "The bandit did not destroy the fence")
    (doseq [[index image] (map-indexed vector @frames)]
      (ImageIO/write image "png" (io/file frame-dir (format "frame-%03d.png" index))))
    (println {:frames (count @frames) :stats (:bandit-stats (world/get-world))
              :swing-counterattack @clicked :bandit-hp (get-in @world/world [:bandits id :hp])})))

(defn benchmark! []
  (with-redefs [world/world (ref {:players {} :objects layout/initial-objects
                                 :bandits (bandits/new-roster 0) :match world/initial-match})]
    (let [measure (fn [index] (let [start (System/nanoTime)]
                                (world/tick-bandits! (+ 60000 (* index 50)))
                                (/ (- (System/nanoTime) start) 1e6)))]
      (doseq [i (range 30)] (measure i))
      (let [samples (vec (sort (map measure (range 30 430))))]
        (println {:tick-samples (count samples) :p50-ms (samples 199)
                  :p95-ms (samples 379) :p99-ms (samples 395) :max-ms (peek samples)})))))

(try
  (with-redefs [layout/spawn-cells (fn [] [[29 26]])]
    (with-open [listener (ServerSocket. 0 16 (InetAddress/getLoopbackAddress))]
      (let [acceptor (future (server/serve! listener))
            connection (connect (.getLocalPort listener))]
        (try
          (build-demo-fence! connection)
          (let [view (edt! #(display! connection))]
            (try (record! view (show-raider!))
                 (finally (edt! #(do (.stop (:timer view))
                                      ((:close! (:builder view)))
                                      (.dispose (:window view)))))))
          (finally (client/close-connection connection) (.close listener) (deref acceptor 3000 nil))))))
  (benchmark!)
  (finally (shutdown-agents)))
