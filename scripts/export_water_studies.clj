(require '[clojure.string :as str]
         '[settlement.config :as config]
         '[settlement.landscape :as landscape]
         '[settlement.layout :as layout])

;; Pixel art bounds are deliberately larger than the viewport along flowing water.
(def world? (some #{"--world"} *command-line-args*))
(def sites (if world? [[:world 0 0 config/world-width config/world-height]]
               [[:river 39 25 22 22] [:lake 37 60 25 22] [:oasis 81 71 19 18]]))
(when world?
  (println (str "seed," config/world-seed))
  (doseq [y (range config/world-height)]
    (println (str "flow," y "," (landscape/river-center (+ y 0.5))))))
(doseq [[site ox oy width height] sites
        y (range oy (+ oy height)) x (range ox (+ ox width))
        :let [p [x y]
              mask (get landscape/shore-masks p [1 1 1 1])
              object (layout/initial-objects p)]]
  (println (str/join "," [(name site) x y (if (landscape/wet-cells p) 1 0)
                         (apply str mask) (name (landscape/biome p))
                         (if (layout/road-cells p) 1 0)
                         (if (or (= :tree object) (= :palm (layout/decorations p))) 1 0)])))
(doseq [[[_ row] {:keys [west east]}] (sort-by (comp second key) landscape/bridges)
        :when (or world? (= 36 row))]
  (println (str/join "," ["bridge" west east row])))
(shutdown-agents)
