(require '[clojure.string :as str] '[settlement.landscape :as l] '[settlement.layout :as layout])
(let [items (merge layout/decorations layout/initial-objects)
      rows (for [[x y :as p] (sort (keys items))]
             (str x "," y "," (name (items p))))]
  (spit (or (first *command-line-args*) ".relief-v642-world.csv")
        (str (str/join "\n" rows) "\n")))
