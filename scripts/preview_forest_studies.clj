(require '[clojure.java.io :as io]
         '[settlement.forest :as forest]
         '[settlement.gui :as gui]
         '[settlement.layout :as layout]
         '[settlement.rendering :as rendering])
(import '[java.awt.image BufferedImage]
        '[javax.imageio ImageIO]
        '[javax.swing JFrame JLabel JProgressBar SwingUtilities])

(let [polished? (some #{"--polish"} *command-line-args*)
      world? (some #{"--world"} *command-line-args*)
      selected (remove #{"--polish" "--world"} *command-line-args*)
      output (io/file (cond world? "docs/previews/world_v6_5_2"
                            polished? "docs/previews/world_v6_5_1"
                            :else "docs/previews/world_v6_5"))
      views (cond-> [[:leaf "deciduous-forest"] [:pine "conifer-forest"]]
              world? (into [[:mixed "mixed-forest" [73 7]] [:shore "forest-transition" [46 36]]]))]
  (when world? (assert (= :world (:forest-study rendering/game-scenery))))
  (.mkdirs output)
  (SwingUtilities/invokeAndWait
   #(let [window (doto (JFrame. (cond world? "Settlement | World V6.5.2"
                                    polished? "Settlement | World V6.5.1"
                                    :else "Settlement | World V6.5"))
                   (.setUndecorated true))]
      (try
        (doseq [[site name override] views
                :when (or (empty? selected) (some #{name} selected))
                :let [[x y :as position] (or override (forest/actors site))
                      [cx cy] (or override (forest/camera-centers site))
                      map-panel (gui/create-map-panel cx cy)
                      ui (gui/create-resource-panel {})
                      panel (:panel (gui/create-game-content
                                     map-panel ui (doto (JProgressBar.) (.setVisible false)) (JLabel.)))]]
          (assert (nil? (layout/initial-objects position)) position)
          (gui/set-map-scenery! map-panel
                                (if world?
                                  rendering/game-scenery
                                  (cond-> (assoc rendering/game-scenery :forest-study site)
                                    polished? (assoc :forest-revision :polished)
                                    (not polished?) (dissoc :forest-revision))))
          (gui/update-map map-panel {1 {:x x :y y :name "Blue" :team :blue :hp 100 :max-hp 100}}
                          layout/initial-objects)
          (.setContentPane window panel)
          (.setSize window 1200 780)
          (.setVisible window true)
          (.validate window)
          (let [image (BufferedImage. (.getWidth panel) (.getHeight panel) BufferedImage/TYPE_INT_RGB)
                g (.createGraphics image)]
            (try (.printAll panel g) (finally (.dispose g)))
            (ImageIO/write image "png" (io/file output (str name ".png")))))
        (finally (.dispose window))))))
(shutdown-agents)
