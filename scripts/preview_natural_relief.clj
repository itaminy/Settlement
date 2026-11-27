(require '[clojure.java.io :as io] '[settlement.gui :as gui] '[settlement.layout :as layout]
         '[settlement.rendering :as rendering])
(import '[java.awt GraphicsEnvironment]
        '[java.awt.image BufferedImage]
        '[javax.imageio ImageIO]
        '[javax.swing JFrame JLabel JProgressBar SwingUtilities])

(def art? (boolean (some #{"--art"} *command-line-args*)))
(def materials? (boolean (some #{"--materials"} *command-line-args*)))
(def shapes? (boolean (some #{"--shapes"} *command-line-args*)))
(def walls? (boolean (some #{"--walls"} *command-line-args*)))
(def finish? (boolean (some #{"--finish"} *command-line-args*)))
(def integrated? (boolean (some #{"--integrated"} *command-line-args*)))
(def output (io/file (cond integrated? "docs/previews/world_v6_4_8"
                           finish? "docs/previews/world_v6_4_7"
                           walls? "docs/previews/world_v6_4_6"
                           shapes? "docs/previews/world_v6_4_5"
                           materials? "docs/previews/world_v6_4_4"
                           art? "docs/previews/world_v6_4_3"
                           :else "docs/previews/world_v6_4_2")))
(.mkdirs output)

(defn capture! [window [name [x y] scene mode actor width height]]
  (let [map-panel (gui/create-map-panel x y)
        ui (gui/create-resource-panel {})
        panel (:panel (gui/create-game-content map-panel ui (doto (JProgressBar.) (.setVisible false)) (JLabel.)))
        device (.getDefaultScreenDevice (GraphicsEnvironment/getLocalGraphicsEnvironment))
        fullscreen? (nil? width)
        revision (cond finish? :finish walls? :walls shapes? :shapes materials? :materials art? :art
                       (not= :prior mode) :natural)
        players (if-let [[ax ay] actor]
                  (do
                    (assert (not (layout/initial-objects actor)) [name actor])
                    {1 {:x ax :y ay :team :blue :hp 100 :max-hp 100 :name "Blue"}})
                  {})]
    (gui/set-map-scenery! map-panel
                          (if integrated?
                            rendering/game-scenery
                            {:terrain-relief {:scene scene :revision revision
                                              :view (if (= mode :unlit) :unlit :lit)}}))
    (gui/update-map map-panel players layout/initial-objects)
    (.setContentPane window panel)
    (.setSize window (or width 1200) (or height 800))
    (.setVisible window true)
    (try
      (when fullscreen? (.setFullScreenWindow device window))
      (.validate window)
      (let [image (BufferedImage. (.getWidth panel) (.getHeight panel) BufferedImage/TYPE_INT_RGB)
            g (.createGraphics image)]
        (try (.printAll panel g) (finally (.dispose g)))
        (ImageIO/write image "png" (io/file output (str name ".png"))))
      (finally (when fullscreen? (.setFullScreenWindow device nil))))))

(def specs
  [["canyon-overview" [86 81] :canyon :lit nil 1980 2320]
   ["deep-gorge" [87 67] :canyon :lit [87 68] 1060 726]
   ["gorge-floor" [87 70] :canyon :lit nil 920 665]
   ["bridge-north" [87 68] :canyon :lit [87 68] 1060 726]
   ["bridge-east" [95 81] :canyon :lit [95 81] 1060 726]
   ["cave" [82 65] :canyon :lit [83 68] 920 665]
   ["desert-camp" [81 69] :canyon :lit nil 920 665]
   ["mountain-overview" [18 74] :mountain :lit nil 2280 2600]
   ["foothills" [20 58] :mountain :lit [20 56] 1060 726]
   ["mountain-slope" [15 66] :mountain :lit nil 1060 726]
   ["highlands" [12 81] :mountain :lit nil 1060 726]
   ["summit" [17 87] :mountain :lit [17 88] 1060 726]
   ["mountain-lake" [24 68] :mountain :lit nil 1060 726]
   ["rock-occlusion" [13 86] :mountain :lit [13 88] 1060 726]
   ["prior-canyon" [87 68] :canyon :prior [87 68] 1060 726]
   ["prior-mountain" [17 87] :mountain :prior [17 88] 1060 726]
   ["unlit-canyon" [87 68] :canyon :unlit [87 68] 1060 726]
   ["unlit-mountain" [17 87] :mountain :unlit [17 88] 1060 726]
   ["fullscreen-canyon" [87 68] :canyon :lit [87 68] nil nil]
   ["fullscreen-mountain" [17 84] :mountain :lit [17 88] nil nil]])

(def art-specs
  [["fullscreen-mountain" [17 76] :mountain :lit [14 79] nil nil]
   ["mountain-stairs-camp" [15 69] :mountain :lit [15 68] 1060 726]
   ["fullscreen-canyon" [87 72] :canyon :lit [87 68] nil nil]
   ["gorge-bridge-cave" [85 67] :canyon :lit [87 68] 1060 726]])

(def material-specs
  [["snowy-mountain" [15 70] :mountain :lit [15 68] 1060 726]
   ["desert-rocks" [81 66] :canyon :lit nil 1060 726]])

(def shape-specs
  [["snowy-mountain" [15 70] :mountain :lit [15 68] 1060 726]
   ["desert-canyon" [83 66] :canyon :lit [87 68] 1060 726]])

(def integrated-specs
  [["fullscreen-mountain" [17 76] :mountain :lit [14 79] nil nil]
   ["fullscreen-canyon" [87 73] :canyon :lit [87 68] nil nil]
   ["mountain-stairs-camp" [15 69] :mountain :lit [15 68] 1060 726]
   ["biome-transition" [20 57] :mountain :lit [20 56] 1280 800]])

(when integrated?
  (assert (= :integrated (get-in rendering/game-scenery [:terrain-relief :revision]))))

(SwingUtilities/invokeAndWait
 #(let [window (doto (JFrame. "Settlement | Natural Terrain and Deep Shadows") (.setUndecorated true))]
    (try (doseq [spec (cond integrated? integrated-specs
                           (or finish? walls? shapes?) shape-specs
              materials? material-specs art? art-specs :else specs)
                 :when (or (not (some #{"--stairs-refresh"} *command-line-args*))
                           (#{"fullscreen-mountain" "mountain-stairs-camp"} (first spec)))]
           (capture! window spec))
         (finally (.dispose window)))))
(shutdown-agents)
