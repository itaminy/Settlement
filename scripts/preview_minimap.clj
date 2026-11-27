(require '[clojure.java.io :as io]
         '[settlement.buildings :as buildings]
         '[settlement.gui :as gui]
         '[settlement.layout :as layout]
         '[settlement.minimap :as minimap])
(import '[java.awt Color Dimension]
        '[java.awt.image BufferedImage]
        '[javax.imageio ImageIO]
        '[javax.swing JFrame JLabel JProgressBar SwingUtilities])

(def output (io/file (or (first *command-line-args*) "docs/previews/minimap")))
(.mkdirs output)

(defn edt! [f]
  (let [value (volatile! nil)]
    (SwingUtilities/invokeAndWait #(vreset! value (f)))
    @value))

(def players {1 {:team :blue :x 24 :y 25 :name "Blue"}
              2 {:team :red :x 74 :y 26 :name "Prolog"}})
(def raiders {3 {:team :bandits :x 57 :y 20 :hp 100 :max-hp 100 :name "Bandit"}
              4 {:team :bandits :x 36 :y 37 :hp 100 :max-hp 100 :name "Bandit"}})
(defn structure [team id kind]
  (buildings/initialize-durability {:type :building :team team :owner-id id :kind kind}))

(def objects (assoc layout/initial-objects [29 24] (structure :blue 1 :house)
                                           [79 29] (structure :red 2 :watchtower)))

(defn display! []
  (let [frame (JFrame. "Settlement")
        map-panel (gui/create-map-panel 24 25 1)
        screen (gui/create-match-screen map-panel)
        health (doto (JLabel.) (.setForeground (Color. 234 199 228)))
        ui (assoc (gui/create-resource-panel {}) :hp-label health
                  :match-id (atom 0) :match-view (atom nil)
                  :match-screen screen :set-gameplay! (constantly nil))
        label (doto (JLabel. "B — строительство · F — атака · R — ремонт")
                (.setForeground (Color. 240 230 193)) (.setPreferredSize (Dimension. 600 40)))
        progress (doto (JProgressBar.) (.setVisible false))
        content (:panel (gui/create-game-content map-panel ui progress label))]
    (.add (:panel ui) health)
    (.setContentPane frame content)
    (.setSize frame 1080 960)
    (.setVisible frame true)
    {:frame frame :content content :map-panel map-panel :ui ui}))

(defn apply! [{:keys [map-panel ui]} state]
  (edt! #(#'gui/apply-world! map-panel ui state 1)))

(defn capture! [{:keys [content]} filename]
  (edt! (fn []
          (let [image (BufferedImage. (.getWidth content) (.getHeight content) BufferedImage/TYPE_INT_RGB)
                g (.createGraphics image)]
            (try (.printAll content g) (finally (.dispose g)))
            (ImageIO/write image "png" (io/file output filename))))))

(defn inspect-map [{:keys [map-panel]}]
  (edt! (fn [] {:camera (.getClientProperty map-panel :settlement.gui/camera)
                :marks (.getClientProperty map-panel :settlement.gui/minimap-marks)
                :visible (minimap/visible? map-panel)})))

(let [{:keys [frame content map-panel] :as window} (edt! display!)
      active {:match-id 0 :match {:phase :active :scores {:blue 15 :red 10}}
              :players players :objects objects :bandits raiders}]
  (try
    (apply! window active)
    (capture! window "large.png")
    (let [before (inspect-map window)
          downed (-> active
                     (assoc-in [:players 1 :respawn-at] 1)
                     (update :objects dissoc [29 24])
                     (assoc-in [:bandits 3 :hp] 0))]
      (assert (= 2 (count (:marks before))))
      (apply! window downed)
      (capture! window "downed.png")
      (assert (= 1 (count (:marks (inspect-map window)))))
      (edt! #(do (.setSize frame 640 430) (.validate frame)))
      (apply! window (-> downed
                         (assoc :players (assoc players 1 (assoc (players 1) :x 51 :y 25))
                                :bandits raiders)
                         (assoc-in [:objects [54 26]] (structure :blue 1 :fence))))
      (capture! window "compact.png")
      (assert (not= (:x (:camera before)) (:x (:camera (inspect-map window)))))
      (assert (= 2 (count (:marks (inspect-map window)))))
      (edt! #(.actionPerformed (.get (.getActionMap content) "minimap") nil))
      (assert (not (:visible (inspect-map window))))
      (capture! window "hidden.png")
      (edt! #(.actionPerformed (.get (.getActionMap content) "minimap") nil))
      (apply! window (assoc active :match {:phase :finished :winner :blue :scores {:blue 100 :red 10}}))
      (capture! window "finished.png")
      (assert (.isVisible (get-in window [:ui :match-screen :panel])))
      (apply! window (assoc active :match-id 1 :objects layout/initial-objects :bandits {}
                                   :match {:phase :active :scores {:blue 0 :red 0}}))
      (capture! window "restarted.png")
      (assert (not (.isVisible (get-in window [:ui :match-screen :panel]))))
      (assert (empty? (:marks (inspect-map window))))
      (println "Swing: six PNGs; resize, camera, disappear/reappear, toggle and restart verified"))
    (finally (edt! #(.dispose frame)))))
(shutdown-agents)
