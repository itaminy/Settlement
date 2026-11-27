(require '[clojure.java.io :as io]
         '[settlement.gui :as gui]
         '[settlement.layout :as layout]
         '[settlement.rendering :as rendering])
(import '[java.awt.image BufferedImage]
        '[javax.imageio ImageIO]
        '[javax.swing JFrame JLabel JProgressBar SwingUtilities])

(let [natural? (some #{"--natural"} *command-line-args*)
      polish? (or natural? (some #{"--polish"} *command-line-args*))
      selected (remove #{"--polish" "--natural"} *command-line-args*)
      output (io/file (cond natural? "docs/previews/world_v6_6_2"
                            polish? "docs/previews/world_v6_6_1" :else "docs/previews/world_v6_6"))
      scenery (fn [site] (cond-> (assoc rendering/game-scenery :water-study site)
                          polish? (assoc :water-revision (if natural? :natural :polished))))
      scenes [[:river "river-bridge" [49 36] [49 36]]
              [:lake "large-lake" [49 71.5] nil]
              [:fall "mountain-waterfall" (cond natural? [31.6 67.4] polish? [31.8 67.8] :else [33 68]) nil]
              [:oasis "desert-oasis" [90 79] nil]]
      scenes (filter (fn [[_ name]] (or (empty? selected) (some #{name} selected))) scenes)
      prepared (when polish?
                 (into {}
                       (for [[site _ [x y]] scenes]
                         [site (rendering/prepare-viewport
                                nil layout/initial-objects
                                (rendering/camera {:x x :y y} 1440 780) (scenery site))])))]
  (.mkdirs output)
  (SwingUtilities/invokeAndWait
   #(let [window (doto (JFrame. "Settlement | World V6.6") (.setUndecorated true))]
      (try
        (doseq [[site name [x y] actor] scenes]
          (let [map-panel (gui/create-map-panel x y)
                panel (:panel (gui/create-game-content
                               map-panel (gui/create-resource-panel {})
                               (doto (JProgressBar.) (.setVisible false)) (JLabel.)))
                players (if-let [[ax ay] actor]
                          (do (assert (nil? (layout/initial-objects actor)) actor)
                              {1 {:x ax :y ay :team :blue :name "Blue" :hp 100 :max-hp 100}})
                          {})]
            (gui/set-map-scenery! map-panel (scenery site))
            (when-let [cache (get prepared site)]
              (gui/prime-map-viewport! map-panel cache))
            (gui/update-map map-panel players layout/initial-objects)
            (.setContentPane window panel)
            (.setSize window (if (= site :lake) 1440 1200) (if (= site :lake) 840 780))
            (.setVisible window true)
            (.validate window)
            (let [image (BufferedImage. (.getWidth panel) (.getHeight panel) BufferedImage/TYPE_INT_RGB)
                  g (.createGraphics image)]
              (try (.printAll panel g) (finally (.dispose g)))
              (ImageIO/write image "png" (io/file output (str name ".png"))))))
        (finally (.dispose window))))))
(shutdown-agents)
