(require '[clojure.set :as set]
         '[settlement.config :as config]
         '[settlement.landscape :as landscape]
         '[settlement.layout :as layout]
         '[settlement.visual-v5 :as v5]
         '[settlement.visual-v6 :as v6]
         '[settlement.massive-mountain :as mountain]
         '[settlement.desert-canyon :as canyon]
         '[settlement.mountain-lake :as lake])

(defn stock [objects [ox oy]]
  (into {} (for [[[x y] kind] objects
                 :when (and (#{:tree :stone} kind) (<= (+ (abs (- x ox)) (abs (- y oy))) 7))]
             [[(- x ox) (- y oy)] kind])))

(defn visible-road-network [roads crossings]
  (binding [layout/road-cells roads landscape/bridge-cells crossings]
    (loop [frontier #{layout/spawn-position} visited frontier]
      (if (empty? frontier) visited
        (let [next (into #{} (for [p frontier n (layout/neighbors p)
                                   :when (and (not (visited n)) (layout/road-link? p n))]
                               n))]
          (recur next (into visited next)))))))

(defn verify [seed]
  (let [start (System/nanoTime)
        {:keys [objects roads wet-cells bridges bridge-cells climate]} (layout/generate-world seed)
        ms (/ (- (System/nanoTime) start) 1e6)
        reached (layout/reachable-cells objects layout/spawn-position)
        dry (set/difference (set (landscape/cells)) (set (keys objects)))
        blue (stock objects layout/spawn-position)
        red (stock objects landscape/red-spawn)]
    (assert (= reached dry) [seed :connected-ground])
    (assert (every? reached roads) [seed :road-collisions])
    (assert (= roads (layout/reachable-cells (zipmap (remove roads (landscape/cells)) (repeat :blocked))
                                            layout/spawn-position)) [seed :connected-roads])
    (assert (= roads (visible-road-network roads bridge-cells)) [seed :connected-rendered-roads])
    (assert (every? roads (concat landscape/camps (vals layout/zone-anchors))) [seed :road-destinations])
    (assert (every? (fn [[p k]] (or (not (#{:tree :stone} k)) (some reached (layout/neighbors p)))) objects)
            [seed :resource-access])
    (assert (every? #(= :water (objects %)) (remove bridge-cells wet-cells)) [seed :water-collisions])
    (assert (every? (fn [[[_ y] {:keys [west east]}]]
                      (and (not (wet-cells [west y])) (not (wet-cells [east y]))
                           (every? roads (for [x (range west (inc east))] [x y]))))
                    bridges) [seed :bridges])
    (when mountain/enabled?
      (assert (every? reached mountain/platforms) [seed :mountain-platforms])
      (assert (every? roads mountain/trail) [seed :summit-road])
      (assert (every? #(= :terrain-blocked (objects %)) (remove wet-cells mountain/blocks))
              [seed :mountain-wall-collisions]))
    (when lake/enabled?
      (let [water? #(= :water (objects %))
            local-water (filter #(and (lake/local-cell? %) (water? %)) wet-cells)]
        (assert (seq local-water) [seed :mountain-lake])
        (assert (every? #(lake/water-at? water? % 32 32) local-water) [seed :mountain-lake-centers])))
    (when canyon/enabled?
      (assert (every? reached canyon/routes) [seed :canyon-routes (remove reached canyon/routes)])
      (assert (every? roads canyon/routes) [seed :canyon-road-links])
      (assert (every? reached canyon/bridges) [seed :canyon-bridges])
      (assert (every? reached (:camps canyon/model)) [seed :canyon-camps])
      (assert (every? #(= :terrain-blocked (objects %)) (remove wet-cells canyon/blocks))
              [seed :canyon-wall-and-pit-collisions]))
    (assert (= blue red) [seed :symmetric-resources])
    (assert (= #{:tree :stone} (set (vals blue))) [seed :starting-materials])
    (assert (= 7 (count (set (map landscape/classify (vals climate))))) [seed :biomes])
    (when v5/enabled?
      (assert (some :marsh? (vals bridges)) [seed :marsh-crossing])
      (assert (every? #(<= 0.0 (:vegetation %) 1.0) (vals climate)) [seed :vegetation]))

    (assert (every? (fn [[ox oy]]
                     (some (fn [[x y]]
                             (not-any? #(or (objects %) (roads %) (layout/start-area? %))
                                       [[x y] [(inc x) y] [x (inc y)] [(inc x) (inc y)]]))
                           (for [y (range (- oy 7) (+ oy 7)) x (range (- ox 7) (+ ox 7))] [x y])))
                   [layout/spawn-position landscape/red-spawn]) [seed :building-sites])
    {:ms ms :fingerprint (hash objects) :bridges (count bridges) :roads (count roads)
     :local-stock (frequencies (vals blue))}))

(let [samples (if v6/enabled? 30 20)
      seeds (take samples (distinct (cons config/world-seed (range))))
      results (mapv verify seeds)
      times (vec (sort (map :ms results)))]
  (assert (= (count results) (count (set (map :fingerprint results)))))
  (assert (= (:fingerprint (first results)) (:fingerprint (verify config/world-seed))))
  (println {:seeds (count seeds) :distinct (count (set (map :fingerprint results)))
            :generation-median-ms (nth times (quot samples 2)) :generation-p95-ms (nth times (dec (long (Math/ceil (* 0.95 samples)))))
            :generation-max-ms (last times) :default (first results)}))
