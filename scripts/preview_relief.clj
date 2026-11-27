(require '[clojure.java.io :as io]
         '[settlement.gui :as gui]
         '[settlement.layout :as layout])
(import '[java.awt GraphicsEnvironment]
        '[java.awt.image BufferedImage]
        '[javax.imageio ImageIO]
        '[javax.swing JFrame JLabel JProgressBar SwingUtilities])

(def output (io/file "docs/previews/world_v6_4_1"))
(.mkdirs output)

(defn capture! [window [name [x y] scene mode actor fullscreen?]]
  (let [map-panel (gui/create-map-panel x y)
        ui (gui/create-resource-panel {})
        panel (:panel (gui/create-game-content map-panel ui (doto (JProgressBar.) (.setVisible false)) (JLabel.)))
        device (.getDefaultScreenDevice (GraphicsEnvironment/getLocalGraphicsEnvironment))
        players (if-let [[ax ay] actor]
                  (do
                    (assert (not (layout/initial-objects actor)) [name actor])
                    {1 {:x ax :y ay :team :blue :hp 100 :max-hp 100 :name "Blue"}})
                  {})]
    (gui/set-map-scenery! map-panel {:terrain-relief {:scene scene :view mode}})
    (gui/update-map map-panel players layout/initial-objects)
    (.setContentPane window panel)
    (if (#{"mountain-demo" "projection-off"} name)
      (.setSize window 1320 940)
      (.setSize window 1060 726))
    (.setVisible window true)
    (try
      (when fullscreen? (.setFullScreenWindow device window))
      (.validate window)
      (let [image (BufferedImage. (.getWidth panel) (.getHeight panel) BufferedImage/TYPE_INT_RGB)
            g (.createGraphics image)]
        (try (.printAll panel g) (finally (.dispose g)))
        (ImageIO/write image "png" (io/file output (str name ".png"))))
      (finally (when fullscreen? (.setFullScreenWindow device nil))))))

(SwingUtilities/invokeAndWait
 #(let [window (doto (JFrame. "Settlement | Relief and Lighting") (.setUndecorated true))]
    (try
      (doseq [spec [["mountain-demo" [17 83] :mountain :lit [13 88] false]
                    ["canyon-demo" [85 68] :canyon :lit [87 68] false]
                    ["bridge-demo" [87 68] :canyon :lit [87 68] false]
                    ["cave-demo" [82 66] :canyon :lit [83 68] false]
                    ["terraces-demo" [13 86] :mountain :lit [13 88] false]
                    ["fullscreen-demo" [86 69] :canyon :lit [87 68] true]
                    ["lighting-off" [85 68] :canyon :unlit [87 68] false]
                    ["projection-off" [17 83] :mountain :flat [13 88] false]]]
        (capture! window spec))
      (finally (.dispose window)))))
(shutdown-agents)
