(require '[clojure.java.io :as io]
         '[settlement.gui :as gui]
         '[settlement.layout :as layout])
(import '[java.awt GraphicsEnvironment]
        '[java.awt.image BufferedImage]
        '[javax.imageio ImageIO]
        '[javax.swing JFrame JLabel JProgressBar SwingUtilities])

(def output (io/file "docs/previews/world_v6_3_2"))
(def before? (= ["--before"] (vec *command-line-args*)))
(.mkdirs output)

(defn capture! [window [name [x y] width height fullscreen?]]
  (let [panel (gui/create-map-panel x y)
        ui (gui/create-resource-panel {})
        content (:panel (gui/create-game-content panel ui
                                                (doto (JProgressBar.) (.setVisible false)) (JLabel.)))
        device (.getDefaultScreenDevice (GraphicsEnvironment/getLocalGraphicsEnvironment))]
    (gui/update-map panel {} layout/initial-objects)
    (.setContentPane window content)
    (.setSize window width height)
    (.setVisible window true)
    (try
      (when fullscreen? (.setFullScreenWindow device window))
      (.validate window)
      (let [image (BufferedImage. (.getWidth content) (.getHeight content) BufferedImage/TYPE_INT_RGB)
            graphics (.createGraphics image)]
        (try (.printAll content graphics) (finally (.dispose graphics)))
        (ImageIO/write image "png" (io/file output (str name ".png"))))
      (finally (when fullscreen? (.setFullScreenWindow device nil))))))

(SwingUtilities/invokeAndWait
 #(let [window (doto (JFrame. "Settlement | Mountain Lake") (.setUndecorated true))
        views [["lake-after" [24 68] 896 640 false]
               ["shore-closeup" [22 69] 680 520 false]
               ["west-cliff-contact" [20 67] 700 520 false]
               ["east-cliff-contact" [28 68] 700 520 false]
               ["lake-fullscreen" [24 69] 896 640 true]
               ["mountain-overview" [18 74] 2272 2636 false]]]
    (try
      (doseq [view (if before?
                     [(assoc (first views) 0 "lake-before")]
                     views)]
        (capture! window view))
      (finally (.dispose window)))))
(shutdown-agents)
