(ns settlement.layout
  (:require [settlement.config :as config]
            [clojure.set :as set])
  (:import [java.util Random Collections ArrayList]))

(def spawn-position [23 25])
(def house-positions #{[31 23] [36 25] [31 29]})
(def groves [[9 10 8 8] [39 9 8 7] [8 39 9 8] [40 40 9 8] [7 22 6 7] [26 16 7 5]])
(def clearings [[23 25 8 6] [22 9 5 4] [8 28 5 4] [24 42 6 4]])
(def rocky-areas [[17 8 5 4] [43 23 6 7] [18 38 5 4]])
(def zone-anchors {:start [23 25] :village [33 27] :north-forest [10 13]
                   :east-forest [39 10] :west-meadow [8 28] :south-meadow [24 42]
                   :southwest-forest [9 39] :southeast-forest [40 40]
                   :east-rocks [42 25] :north-rocks [17 8] :south-rocks [18 38]})

(defn- ellipse [[x y] [cx cy rx ry]]
  (+ (Math/pow (/ (- x cx) (double rx)) 2) (Math/pow (/ (- y cy) (double ry)) 2)))
(defn- organic-inside? [[x y :as p] shape]
  (< (ellipse p shape) (+ 1.0 (* 0.13 (Math/sin (+ (* x 0.71) (* y 0.37))))
                              (* 0.09 (Math/cos (- (* x 0.43) (* y 0.61)))))))
(defn start-area? [p] (organic-inside? p [23 25 3.5 2.5]))
(defn house-clearance? [[x y]]
  (boolean (some (fn [[hx hy]] (and (<= (abs (- x hx)) 2) (<= (abs (- y hy)) 2))) house-positions)))
(defn clearing? [p] (boolean (some #(organic-inside? p %) clearings)))
(defn forest? [p] (boolean (some #(organic-inside? p %) groves)))
(defn rocky? [p] (boolean (some #(organic-inside? p %) rocky-areas)))
(defn village? [p] (organic-inside? p [33 26 8 7]))
(defn neighbors [[x y]]
  (filter config/in-bounds? [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]))
(defn- near [p radius]
  (let [[x y] p]
    (for [dy (range (- radius) (inc radius)) dx (range (- radius) (inc radius))
          :when (not= [dx dy] [0 0]) :let [p [(+ x dx) (+ y dy)]] :when (config/in-bounds? p)] p)))

(def path-routes
  [[[23 25] [26 25] [28 24] [31 24]]
   [[31 24] [33 24] [34 25] [36 26]]
   [[31 24] [33 27] [34 28] [34 30] [31 30]]
   [[23 25] [20 22] [18 19] [16 17] [14 14] [10 13]]
   [[33 27] [37 28] [39 26] [42 25]]
   [[23 25] [22 29] [24 33] [23 36] [24 40] [24 42]]
   [[23 25] [25 21] [26 18] [24 15] [24 12] [22 9]]
   [[22 9] [26 8] [30 10] [34 8] [39 10]]
   [[22 9] [19 10] [17 8]]
   [[23 36] [20 36] [18 38]]
   [[23 25] [19 26] [15 28] [12 27] [8 28]]
   [[12 27] [10 30] [12 33] [10 36] [9 39]]
   [[24 40] [28 42] [31 41] [35 41] [40 40]]])

(defn- curve [route]
  (let [points (vec (concat [(first route)] route [(last route)]))]
    (for [i (range 1 (- (count points) 2)) step (range 21)
          :let [t (/ step 20.0) t2 (* t t) t3 (* t2 t)]]
      (mapv (fn [axis]
              (let [a (get-in points [(dec i) axis]) b (get-in points [i axis])
                    c (get-in points [(inc i) axis]) d (get-in points [(+ i 2) axis])]
                (long (Math/round (* 0.5 (+ (* 2 b) (* (- c a) t)
                    (* (+ (* 2 a) (* -5 b) (* 4 c) (- d)) t2)
                    (* (+ (- a) (* 3 b) (* -3 c) d) t3))))))) [0 1]))))

(def road-cells
  (set (filter config/in-bounds?
         (mapcat (fn [route]
                   (let [samples (curve route)]
                     (concat samples (for [[[x1 y1] [x2 y2]] (partition 2 1 samples)
                                           :when (and (not= x1 x2) (not= y1 y2))] [x2 y1])))) path-routes))))
(defn road? [x y] (contains? road-cells [x y]))
(defn courtyard? [p]
  (boolean (some (fn [[x y]] (organic-inside? p [x (inc y) 2.5 1.8])) house-positions)))
(defn dirt? [x y] (or (road? x y) (courtyard? [x y])))
(defn grass-variant [p] (cond (forest? p) 0 (clearing? p) 1 :else 2))

(defn reachable-cells [objects origin]
  (loop [visited #{origin} frontier #{origin}]
    (let [next-cells (set (for [p frontier n (neighbors p)
                                :when (and (not (contains? objects n)) (not (contains? visited n)))] n))]
      (if (empty? next-cells) visited (recur (into visited next-cells) next-cells)))))

(defn- carve-connection [objects origin destination?]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY origin) parents {origin nil}]
    (let [p (peek queue)]
      (if (destination? p)
        (loop [objects objects p p]
          (if p (recur (if (= :house (objects p)) objects (dissoc objects p)) (parents p)) objects))
        (let [next-cells (remove #(or (contains? parents %) (= :house (objects %))) (neighbors p))]
          (recur (into (pop queue) next-cells) (reduce #(assoc %1 %2 p) parents next-cells)))))))

(defn- connect-free-space [objects]
  (loop [objects objects]
    (let [reachable (reachable-cells objects spawn-position)
          missing (first (for [y (range config/world-height) x (range config/world-width)
                               :let [p [x y]] :when (and (not (contains? objects p)) (not (reachable p)))] p))]
      (if missing (recur (carve-connection objects missing reachable)) objects))))

(defn- shuffled-cells [random]
  (let [cells (ArrayList. ^java.util.Collection
               (vec (for [y (range config/world-height) x (range config/world-width)] [x y])))]
    (Collections/shuffle cells random) cells))

(defn generate-objects []
  (let [random (Random. 592321)
        houses (zipmap house-positions (repeat :house))
        candidates (shuffled-cells random)
        objects
        (reduce
         (fn [objects [x y :as p]]
           (if (or (start-area? p) (house-clearance? p) (village? p) (clearing? p)
                   (dirt? x y) (some #(apply road? %) (neighbors p))) objects
             (let [r (.nextDouble random)
                   tree-chance (cond (rocky? p) 0.04 (forest? p) 0.75 :else 0.13)
                   stone-chance (if (rocky? p) 0.58 0.025)
                   kind (cond (< r stone-chance) :stone (< r (+ stone-chance tree-chance)) :tree)]
               (cond
                 (and (= kind :tree) (not-any? #(= :tree (objects %)) (near p 1))) (assoc objects p :tree)
                 (and (= kind :stone) (< (count (filter #(= :stone (objects %)) (neighbors p))) 2)) (assoc objects p :stone)
                 :else objects)))) houses candidates)
        ;; Каждый добываемый объект должен иметь хотя бы одну свободную соседнюю клетку.
        accessible (reduce (fn [objects [p kind]]
                             (if (and (#{:tree :stone} kind) (every? #(contains? objects %) (neighbors p)))
                               (if-let [neighbor (first (filter #(#{:tree :stone} (objects %)) (neighbors p)))]
                                 (dissoc objects neighbor) objects) objects))
                           objects (sort-by key objects))]
    (connect-free-space accessible)))

(def initial-objects (generate-objects))

(defn spawn-cells []
  (let [cells (for [y (range config/world-height) x (range config/world-width)] [x y])]
    (concat (sort-by (fn [[x y :as p]] [(+ (abs (- x (first spawn-position))) (abs (- y (second spawn-position)))) y x])
                     (filter start-area? cells))
            (remove start-area? cells))))

(defn generate-decorations []
  (let [random (Random. 73191)]
    (reduce
     (fn [decorations [x y :as p]]
       (if (or (contains? initial-objects p) (start-area? p) (house-clearance? p)
               (dirt? x y) (some #(contains? decorations %) (near p 1))) decorations
         (let [r (.nextDouble random)
               trees (some #(= :tree (initial-objects %)) (near p 1))
               near-path (some #(apply road? %) (neighbors p))
               kind (cond
                      (and near-path (village? p) (< r 0.14)) :bench
                      (and trees (< r 0.22)) (if (< (.nextDouble random) 0.5) :mushroom-red :mushroom-green)
                      (and (forest? p) (< r 0.055)) :stump
                      (and (or (clearing? p) near-path) (< r 0.09)) :bush)]
           (if kind (assoc decorations p kind) decorations)))) {} (shuffled-cells random))))

(def decorations (generate-decorations))
