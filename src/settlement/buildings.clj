(ns settlement.buildings
  (:require [settlement.config :as config]
            [settlement.layout :as layout]
            [settlement.inventory :as inventory]))

(def catalog
  {:fence {:name "Деревянный забор" :size [1 1] :cost {:wood 2}}
   :house {:name "Деревянный дом" :size [2 2] :cost {:wood 15 :stone 10}}})

(defn building? [object] (and (map? object) (= :building (:type object))))
(defn cells [[x y] object]
  (let [[width height] (if (building? object) (get-in catalog [(:kind object) :size]) [1 1])]
    (for [dy (range height) dx (range width)] [(+ x dx) (+ y dy)])))

(defn occupied-cells [objects]
  (into #{} (mapcat (fn [[position object]] (cells position object)) objects)))

(defn occupied? [objects position]
  (or (contains? objects position)
      (boolean (some (fn [[anchor object]]
                       (and (building? object) (some #{position} (cells anchor object)))) objects))))

(def protected-cells
  (into (set (concat layout/road-cells (vals layout/zone-anchors)
                     (for [[x y] layout/house-positions] [x (inc y)])))
        (for [y (range config/world-height) x (range config/world-width)
              :when (layout/start-area? [x y])] [x y])))

(defn- reachable
  ([blocked] (reachable blocked layout/spawn-position))
  ([blocked origin]
   (if (or (nil? origin) (blocked origin)) #{}
    (loop [visited #{origin} frontier #{origin}]
      (let [next-cells (set (for [p frontier n (layout/neighbors p)
                                  :when (and (not (blocked n)) (not (visited n)))] n))]
        (if (empty? next-cells) visited (recur (into visited next-cells) next-cells)))))))

(defn spawn-accessible-cells [objects]
  (let [blocked (occupied-cells objects)
        origin (if (blocked layout/spawn-position)
                 (first (remove blocked (layout/spawn-cells))) layout/spawn-position)]
    (reachable blocked origin)))

(defn access-safe? [state objects]
  (let [visited (reachable (occupied-cells objects))]
    (and (every? visited protected-cells)
         ;; Персонажи — цели доступности, а не временные стены при BFS.
         (every? #(visited [(:x %) (:y %)]) (vals (:players state)))
         (every? (fn [[position object]]
                   (or (not (#{:tree :stone} object))
                       (some visited (layout/neighbors position)))) objects))))

(defn validate [state player-id kind position]
  (let [player (get-in state [:players player-id])
        spec (catalog kind)
        fail (fn [code] {:status :error :code code})]
    (cond
      (nil? player) (fail :player-not-found)
      (nil? spec) (fail :unknown-building)
      (not (and (vector? position) (= 2 (count position)) (every? integer? position))) (fail :invalid-position)
      (not (config/in-bounds? position)) (fail :out-of-bounds)
      :else
      (let [entry {:type :building :kind kind :owner-id player-id}
            footprint (vec (cells position entry))
            player-positions (set (map (juxt :x :y) (vals (:players state))))
            items (:items (inventory/player-inventory player))]
        (cond
          (not-every? config/in-bounds? footprint) (fail :out-of-bounds)
          (some #(occupied? (:objects state) %) footprint) (fail :object-blocked)
          (some player-positions footprint) (fail :player-blocked)
          (some protected-cells footprint) (fail :protected-area)
          (> (apply min (map (fn [[x y]] (+ (abs (- x (:x player))) (abs (- y (:y player))))) footprint)) 2)
          (fail :too-far)
          (some (fn [[item amount]] (< (get items item 0) amount)) (:cost spec)) (fail :insufficient-items)
          (not (access-safe? state (assoc (:objects state) position entry))) (fail :blocks-access)
          :else {:status :ok :entry entry :cells footprint :cost (:cost spec)})))))
