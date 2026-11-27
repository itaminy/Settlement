(ns settlement.layout-test
  (:require [clojure.test :refer :all]
            [clojure.set :as set]
            [settlement.layout :as layout]
            [settlement.landscape :as landscape]
            [settlement.massive-mountain :as mountain]
            [settlement.desert-canyon :as canyon]
            [settlement.auth :as auth]
            [settlement.buildings :as buildings]
            [settlement.config :as config]
            [settlement.world :as world]
            [settlement.rendering :as rendering]
            [settlement.test-support :as support]))

(use-fixtures :each
  (fn [f]
    (with-redefs [world/world (ref {:players {} :resources {:wood 0 :stone 0} :objects layout/initial-objects})
                  auth/state (ref (auth/new-state))
                  world/gathering (ref {}) world/now-ns #(deref support/time-ns)
                  world/schedule-resource-restore (fn [& _] nil)] (f))))

(defn- all-cells []
  (for [y (range config/world-height) x (range config/world-width)] [x y]))

(defn- flood [cells origin]
  (loop [visited #{origin} frontier #{origin}]
    (let [next-cells (set (for [p frontier n (layout/neighbors p)
                                :when (and (cells n) (not (visited n)))] n))]
      (if (empty? next-cells) visited (recur (into visited next-cells) next-cells)))))

(deftest reproducible-zoned-generation-test
  (is (= layout/initial-objects (layout/generate-objects)))
  (is (= layout/decorations (layout/generate-decorations)))
  (is (= 3 (count layout/house-positions)))
  (is (every? config/in-bounds? (concat (keys layout/initial-objects) (keys layout/decorations))))
  (is (< (count layout/decorations) (quot (* config/world-width config/world-height) 9))))

(deftest central-start-area-is-clear-test
  (let [start (filter layout/start-area? (all-cells))]
    (is (not-any? #(contains? layout/initial-objects %) start))
    (is (not-any? #(contains? layout/decorations %) start))
    (world/add-player 1 "A") (world/add-player 2 "B")
    (let [players (:players (world/get-world))]
      (is (= layout/spawn-position ((juxt :x :y) (players 1))))
      (is (every? layout/start-area? (map (juxt :x :y) (vals players))))
      (is (= 2 (count (set (map (juxt :x :y) (vals players)))))))))

(deftest roads-connect-zones-and-house-entrances-test
  (let [roads (set/difference layout/road-cells layout/house-positions)
        reachable (flood roads layout/spawn-position)]
    (is (= roads reachable))
    (is (every? reachable (vals layout/zone-anchors)))
    (is (every? reachable (for [[x y] layout/house-positions] [x (inc y)])))
    (is (not-any? #(contains? layout/initial-objects %) roads))))

(deftest village-has-space-around-buildings-test
  (doseq [[p kind] layout/initial-objects :when (#{:tree :stone} kind)]
    (is (not (layout/house-clearance? p)))
    (is (not (apply layout/dirt? p))))
  (is (every? #(= :house (layout/initial-objects %)) layout/house-positions))
  (is (not-any? layout/house-clearance? (keys (remove (comp #{:lantern} val) layout/decorations)))))

(deftest resources-and-free-space-remain-accessible-test
  (let [reachable (layout/reachable-cells layout/initial-objects layout/spawn-position)]
    (is (= reachable (set/difference (set (all-cells)) (set (keys layout/initial-objects)))))
    (doseq [[p kind] layout/initial-objects :when (#{:tree :stone} kind)]
      (is (some reachable (layout/neighbors p))))))

(deftest hydrology-is-authoritative-and-crossings-stay-open-test
  (let [state (world/get-world)
        reachable (buildings/spawn-accessible-cells (:objects state))]
    (is (<= 6 (count landscape/bridges) 10))
    (is (every? #(not (world/free-cell? state %)) (keys landscape/water-blocks)))
    (doseq [[[_ y] {:keys [west east]}] landscape/bridges
            x (range west (inc east))]
      (is (world/free-cell? state [x y]))
      (is (buildings/protected-cells [x y]))
      (is (reachable [x y])))
    (is (every? reachable buildings/protected-cells))
    (is (buildings/access-safe? state (:objects state)))
    (doseq [[anchor [kind :as spec]] landscape/landmarks
            :when (= kind (get-in state [:objects anchor]))
            position (landscape/footprint anchor spec)]
      (is (not (world/free-cell? state position))))))

(deftest server-movement-and-construction-at-a-river-crossing-test
  (let [[[_ y] {:keys [west east]}] (first landscape/bridges)
        [bank water] (first
                       (for [water (sort (keys landscape/water-blocks))
                             bank (layout/neighbors water)
                             :when (not (layout/initial-objects bank))]
                         [bank water]))
        set-player! (fn [[x y]]
                      (dosync (alter world/world assoc-in [:players 1]
                                     {:x x :y y :team :blue :inventory {:items {:wood 2}}})))
        direction ({[1 0] :right [-1 0] :left [0 1] :down [0 -1] :up}
                   (mapv - water bank))]
    (set-player! bank)
    (is (= :object-blocked (:code (world/move-player-result 1 direction))))
    (set-player! [west y])
    (is (= :protected-area (:code (world/build! 1 :fence [(inc west) y]))))
    (dotimes [_ (- east west)]
      (is (= :ok (:status (world/move-player-result 1 :right)))))
    (is (= [east y] ((juxt :x :y) (get-in (world/get-world) [:players 1]))))))

(deftest role-based-spawns-have-matching-local-resources-test
  (let [human (auth/register! :human 72 "Human" :human)
        bot (auth/register! :bot 73 "Bot" :prolog)
        {{blue 72 red 73} :players} (world/get-world)
        stock (fn [[x y]]
                (into {} (for [[[px py] kind] layout/starter-resources
                               :when (<= (+ (abs (- px x)) (abs (- py y))) 8)]
                           [[(- px x) (- py y)] kind])))]
    (is (= [:ok :ok] (mapv :status [human bot])))
    (is (= [:blue :red] [(:team blue) (:team red)]))
    (is (= layout/spawn-position ((juxt :x :y) blue)))
    (is (= landscape/red-spawn ((juxt :x :y) red)))
    (is (= (stock layout/spawn-position) (stock landscape/red-spawn)))
    (is (= #{:tree :stone} (set (vals (stock layout/spawn-position)))))))

(deftest decor-follows-the-environment-test
  (doseq [[[x y :as p] kind] layout/decorations]
    (is (not (contains? layout/initial-objects p)))
    (is (or (= :lantern kind) (not (layout/dirt? x y))))
    (cond
      (mountain/props p) (is (and (mountain/platforms p) (not (mountain/route-cells p))))
      (canyon/props p) (is (and (canyon/platforms p) (not (canyon/routes p))))
      :else
      (case kind
      (:mushroom-red :mushroom-green :mushroom-group)
      (is (some (fn [[[tx ty] object]]
                  (and (= object :tree) (<= (abs (- tx x)) 1) (<= (abs (- ty y)) 1))) layout/initial-objects))
      :sign (do (is (layout/village? p)) (is (some #(apply layout/road? %) (layout/neighbors p))))
      (:stump :fallen-tree :fern :forest-fern :forest-floor) (is (layout/forest? p))
      (:shore-rocks :reeds :rushes :bank-stones) (is (some landscape/wet-cells (layout/neighbors p)))
      (:dry-bush :bones :dune-grass :saguaro :dune-field :scrub-cluster :dry-channel :sand-boulders :relict-well :ruin-court) (is (= :desert (landscape/biome p)))
      (:white-flowers :blue-flowers :yellow-flowers) (is (= :plains (landscape/biome p)))
      (:bush :berry-bush) (is (#{:conifer :deciduous :plains} (landscape/biome p)))
      :tall-grass (is (#{:plains :swamp} (landscape/biome p)))
      :palm (is (= :desert (landscape/biome p)))
      (:dead-tree :marsh-bank :marsh-fern :rooted-snags :drowned-log :marsh-hamlet)
      (is (= :swamp (landscape/biome p)))
      :frost-pine (is (= :snow (landscape/biome p)))
      :talus-fan (is (#{:mountains :snow} (landscape/biome p)))
      :abandoned-camp (is (or (layout/forest? p) (= :desert (landscape/biome p))))
      :lantern (is (or (layout/house-clearance? p)
                       (some (fn [[cx cy]] (<= (+ (abs (- x cx)) (abs (- y cy))) 2)) landscape/camps)))
      (:raider-tent :crate :barrel :ruined-wall)
      (is (some (fn [[cx cy]] (<= (+ (abs (- x cx)) (abs (- y cy))) 3)) landscape/camps))))))

(deftest chimney-and-broken-sprites-are-excluded-test
  ;; logs.png — отдельная труба; файл сохраняется, но в декор не попадает.
  (is (not-any? #{:logs :flowers} (vals layout/decorations)))
  (is (every? (fn [^String path] (.startsWith path "images/nature_v3/"))
              (for [[position kind] layout/initial-objects :when (= kind :tree)]
                (rendering/sprite-paths (rendering/sprite-kind position kind))))))

(deftest real-map-collision-and-timed-harvest-test
  (world/add-player 1 "A")
  ;; Находим доступный ресурс и соседнюю свободную клетку на настоящей карте.
  (let [[target kind] (first (filter #(#{:tree :stone} (val %)) (sort-by key layout/initial-objects)))
        position (first (remove #(contains? layout/initial-objects %) (layout/neighbors target)))
        [x y] position [tx ty] target
        direction (cond (< x tx) :right (> x tx) :left (< y ty) :down :else :up)]
    (dosync (alter world/world update-in [:players 1] assoc :x x :y y))
    (is (= :object-blocked (:code (world/move-player-result 1 direction))))
    (let [result (support/gather! 1) resource (:resource result)]
      (is (= :ok (:status result)))
      (is (nil? (get-in (world/get-world) [:objects (:position result)])))
      (is (= 1 (get-in (world/get-world) [:players 1 :inventory :items resource]))))))


(deftest massive-mountain-has-a-protected-continuous-ascent
  (when mountain/enabled?
    (let [objects layout/initial-objects
          reachable (layout/reachable-cells objects layout/spawn-position)]
      (is (= 4 (count mountain/camps)))
      (is (every? reachable (conj (vec mountain/camps) mountain/entrance mountain/summit)))
      (is (not-any? objects mountain/platforms))
      (is (every? buildings/protected-cells mountain/platforms))
      (is (every? #(= :terrain-blocked (objects %)) (remove landscape/wet-cells mountain/blocks)))
      (is (every? (fn [[a b]] (and (some #{b} (layout/neighbors a)) (layout/road-link? a b)))
                  (partition 2 1 mountain/trail))))))
