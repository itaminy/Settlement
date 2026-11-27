(require '[clojure.string :as str]
         '[settlement.landscape :as landscape]
         '[settlement.layout :as layout])

(doseq [[site biome [cx cy]] [[:leaf :deciduous [13 37]] [:pine :conifer [38 38]]]
        y (range (- cy 9) (+ cy 10))
        x (range (- cx 12) (+ cx 13))
        :let [p [x y]]]
  (println (str/join "," [(name site) x y (if (= biome (landscape/biome p)) 1 0)
                         (if (layout/road-cells p) 1 0)
                         (name (get layout/initial-objects p :clear))
                         (name (get layout/decorations p :clear))])))
(shutdown-agents)
