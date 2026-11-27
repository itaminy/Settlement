(ns settlement.buildings
  (:require [settlement.config :as config]
            [settlement.layout :as layout]
            [settlement.massive-mountain :as mountain]
            [settlement.desert-canyon :as canyon]
            [settlement.inventory :as inventory]))

(def catalog
  {:fence {:name "Деревянный забор" :size [1 1] :cost {:wood 2} :max-hp 50 :points 0}
   :house {:name "Деревянный дом" :size [2 2] :cost {:wood 15 :stone 10} :max-hp 200 :points 15}
   :watchtower {:name "Сторожевая башня" :size [2 1] :cost {:wood 10 :stone 8} :max-hp 150 :points 10}
   :sawmill {:name "Лесопилка" :size [2 2] :cost {:wood 20 :stone 10} :max-hp 240 :points 20}
   :mine {:name "Шахта" :size [2 1] :cost {:wood 10 :stone 20} :max-hp 260 :points 20}})

(defn building? [object] (and (map? object) (= :building (:type object))))

(defn initialize-durability [object]
  (if-let [max-hp (and (building? object) (get-in catalog [(:kind object) :max-hp]))]
    (let [max-hp (or (:max-hp object) max-hp)]
      (cond-> object
        (nil? (:building-id object)) (assoc :building-id (random-uuid))
        (nil? (:hp object)) (assoc :hp max-hp)
        (nil? (:max-hp object)) (assoc :max-hp max-hp)))
    object))

(defn cells [[x y] object]
  (let [[width height] (if (building? object) (get-in catalog [(:kind object) :size]) [1 1])]
    (for [dy (range height) dx (range width)] [(+ x dx) (+ y dy)])))

(defn cell-coordinate? [position]
  (and (vector? position) (= 2 (count position)) (every? integer? position)))

(defn target-at [objects position]
  (when (and (cell-coordinate? position) (config/in-bounds? position))
    (let [[target other] (for [[anchor object] objects
                               :when (and (building? object) (some #{position} (cells anchor object)))]
                           {:anchor anchor :building object})]
      (when-not other target))))

(defn- distance-to-cells [{:keys [x y]} footprint]
  (reduce min (map (fn [[cx cy]] (+ (abs (- cx x)) (abs (- cy y)))) footprint)))

(defn distance-to-building [player {:keys [anchor building]}]
  (distance-to-cells player (cells anchor building)))

(defn adjacent-to-building? [player target]
  (= 1 (distance-to-building player target)))

(defn occupied-cells [objects]
  (into #{} (mapcat (fn [[position object]] (cells position object)) objects)))

(defn occupied? [objects position]
  (or (contains? objects position)
      (boolean (some (fn [[anchor object]]
                       (and (building? object) (some #{position} (cells anchor object)))) objects))))

(def protected-cells
  (into (set (concat layout/road-cells (filter layout/road-cells (vals layout/zone-anchors))
                     mountain/platforms (remove layout/initial-objects canyon/platforms)
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
      (:respawn-at player) (fail :player-downed)
      (nil? spec) (fail :unknown-building)
      (not (cell-coordinate? position)) (fail :invalid-position)
      (not (config/in-bounds? position)) (fail :out-of-bounds)
      :else
      (let [entry {:type :building :kind kind :owner-id player-id :team (:team player)}
            footprint (vec (cells position entry))
            player-positions (into #{} (map (juxt :x :y)) (remove :respawn-at (vals (:players state))))
            bandit-positions (into #{} (map (juxt :x :y)) (filter #(pos? (:hp %)) (vals (:bandits state))))
            items (:items (inventory/player-inventory player))]
        (cond
          (not-every? config/in-bounds? footprint) (fail :out-of-bounds)
          (some #(occupied? (:objects state) %) footprint) (fail :object-blocked)
          (some player-positions footprint) (fail :player-blocked)
          (some bandit-positions footprint) (fail :object-blocked)
          (some protected-cells footprint) (fail :protected-area)
          (> (distance-to-cells player footprint) 2)
          (fail :too-far)
          (some (fn [[item amount]] (< (get items item 0) amount)) (:cost spec)) (fail :insufficient-items)
          (not (access-safe? state (assoc (:objects state) position entry))) (fail :blocks-access)
          :else {:status :ok :entry entry :cells footprint :cost (:cost spec)})))))
