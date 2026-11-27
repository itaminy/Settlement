(require '[clojure.string :as str]
         '[settlement.config :as config]
         '[settlement.desert-canyon :as canyon]
         '[settlement.forest-ground :as forest]
         '[settlement.landscape :as landscape]
         '[settlement.layout :as layout]
         '[settlement.massive-mountain :as mountain])

(println (str "seed," config/world-seed))
(doseq [[x y :as p] (landscape/cells)
        :let [wet (landscape/wet-cells p)
              road (layout/road-cells p)
              object (layout/initial-objects p)
              detail (layout/decorations p)
              land (case (landscape/biome p)
                     :plains (if (forest/site p) :woods :plains)
                     :swamp :swamp
                     :desert :desert
                     :protected)
              protected? (or wet (layout/house-positions p)
                             (mountain/platforms p) (mountain/blocks p)
                             (canyon/routes p) (canyon/walls p) (canyon/pits p))]]
  (println (str/join "," [x y (name (if protected? :protected land))
                         (if road 1 0) (if object 1 0) (if detail 1 0)])))
(shutdown-agents)
