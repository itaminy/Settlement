(ns settlement.layout
  (:require [settlement.config :as config])
  (:import [java.util Random Collections ArrayList]))

;; Сохраняем знакомую стартовую область при расширении мира.
(def starting-region-size 12)
(def starting-objects
  {[3 4] :tree [5 5] :tree [2 1] :tree [5 1] :tree
   [9 1] :tree [1 3] :tree [5 3] :tree [9 3] :tree
   [7 4] :tree [10 5] :tree [1 7] :tree [3 7] :tree
   [7 7] :tree [10 7] :tree [2 9] :tree [5 9] :tree
   [9 9] :tree [7 10] :tree
   [7 2] :stone [1 1] :stone [10 2] :stone [3 2] :stone
   [2 5] :stone [8 5] :stone [5 7] :stone [11 8] :stone
   [1 10] :stone [4 10] :stone [9 11] :stone [11 10] :stone
   [8 6] :house [6 8] :house [3 10] :house})

(defn road? [x y]
  (and (config/in-bounds? [x y])
       (or (= y 6) (and (= x 4) (<= 3 y))
           (and (= y 30) (<= 4 x)) (and (= x 28) (<= 6 y)))))

(defn- shuffled [positions random]
  (let [items (ArrayList. ^java.util.Collection (vec positions))]
    (Collections/shuffle items random)
    (vec items)))

(defn generate-objects []
  (let [random (Random. 591832)]
    (reduce
     (fn [objects [bx by]]
       ;; Свободные границы блоков 3×3 образуют связанные проходы.
       (let [positions (shuffled
                        (for [dy [1 2] dx [1 2]
                              :let [x (+ bx dx) y (+ by dy)]
                              :when (and (config/in-bounds? [x y])
                                         (or (>= x starting-region-size) (>= y starting-region-size))
                                         (not (road? x y)))] [x y]) random)
             kinds (concat (when (< (.nextDouble random) 0.92) [:tree])
                           (when (< (.nextDouble random) 0.72) [:stone]))]
         (into objects (map vector positions kinds))))
     starting-objects
     (for [y (range 0 config/world-height 3) x (range 0 config/world-width 3)] [x y]))))

(def initial-objects (generate-objects))

(defn generate-decorations []
  (let [random (Random. 73109)
        kinds [:bush :mushroom-red :mushroom-green :logs :stump :bench]]
    (into {}
          (for [by (range 0 config/world-height 3) bx (range 0 config/world-width 3)
                :let [position (first (shuffled
                                       (for [dy (range 3) dx (range 3)
                                             :let [p [(+ bx dx) (+ by dy)]]
                                             :when (and (config/in-bounds? p)
                                                        (not= p [0 0])
                                                        (not (contains? initial-objects p))
                                                        (not (apply road? p)))] p) random))]
                :when position]
            [position (nth kinds (.nextInt random (count kinds)))]))))

(def decorations (generate-decorations))
