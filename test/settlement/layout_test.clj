(ns settlement.layout-test
  (:require [clojure.test :refer :all]
            [clojure.set :as set]
            [settlement.layout :as layout]
            [settlement.config :as config]
            [settlement.world :as world]
            [settlement.rendering :as rendering]
            [settlement.test-support :as support]))

(use-fixtures :each
  (fn [f]
    (with-redefs [world/world (ref {:players {} :resources {:wood 0 :stone 0} :objects layout/initial-objects})
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
  (is (< (count layout/decorations) 200)))

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
  (is (not-any? layout/house-clearance? (keys layout/decorations))))

(deftest resources-and-free-space-remain-accessible-test
  (let [reachable (layout/reachable-cells layout/initial-objects layout/spawn-position)]
    (is (= reachable (set/difference (set (all-cells)) (set (keys layout/initial-objects)))))
    (doseq [[p kind] layout/initial-objects :when (#{:tree :stone} kind)]
      (is (some reachable (layout/neighbors p))))))

(deftest decor-follows-the-environment-test
  (doseq [[[x y :as p] kind] layout/decorations]
    (is (not (contains? layout/initial-objects p)))
    (is (not (layout/dirt? x y)))
    (case kind
      (:mushroom-red :mushroom-green)
      (is (some (fn [[[tx ty] object]]
                  (and (= object :tree) (<= (abs (- tx x)) 1) (<= (abs (- ty y)) 1))) layout/initial-objects))
      :bench (do (is (layout/village? p)) (is (some #(apply layout/road? %) (layout/neighbors p))))
      :stump (is (layout/forest? p))
      :bush (is (or (layout/clearing? p) (some #(apply layout/road? %) (layout/neighbors p)))))))

(deftest chimney-and-broken-sprites-are-excluded-test
  ;; logs.png — отдельная труба; файл сохраняется, но в декор не попадает.
  (is (not-any? #{:logs :flowers} (vals layout/decorations)))
  (is (= #{:tree :tree-small :tree-tall}
         (set (for [[p kind] layout/initial-objects :when (= kind :tree)] (rendering/sprite-kind p kind))))))

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
