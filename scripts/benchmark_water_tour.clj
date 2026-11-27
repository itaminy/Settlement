(require '[settlement.layout :as layout]
         '[settlement.rendering :as rendering])
(import '[javax.swing SwingUtilities])

(let [scenes [[:river 49 36] [:lake 49 71.5] [:fall 31.6 67.4] [:oasis 90 79]]
      objects layout/initial-objects
      scenery #(assoc rendering/game-scenery :water-study % :water-revision :natural)
      camera #(rendering/camera {:x %1 :y %2} 1440 810)
      millis #(/ (- (System/nanoTime) %) 1e6)
      start (System/nanoTime)
      prepared (into {} (for [[site x y] scenes]
                          [site (rendering/prepare-viewport nil objects (camera x y) (scenery site))]))
      preload-ms (millis start)]
  (SwingUtilities/invokeAndWait
   #(let [frame (volatile! nil)
          results
          (mapv (fn [[site x y]]
                  (let [view (volatile! (prepared site))
                        samples (mapv
                                 (fn [i]
                                   (let [cam (camera (+ x (* i 0.12)) y)
                                         start (System/nanoTime)]
                                     (vswap! view rendering/prepare-viewport objects cam (scenery site))
                                     (vswap! frame rendering/render-viewport {} cam {} @view i)
                                     (millis start)))
                                 (range 46))
                        pan (vec (sort (rest samples)))]
                    {:site site :switch-ms (first samples) :pan-p95-ms (pan 42)
                     :pan-max-ms (peek pan)}))
                scenes)]
      (println {:preload-ms preload-ms
                :total-first-ms (+ preload-ms (:switch-ms (first results)))
                :scenes results}))))
(shutdown-agents)
