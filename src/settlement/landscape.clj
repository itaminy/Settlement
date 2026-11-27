(ns settlement.landscape
  (:require [settlement.config :as config]
            [settlement.visual-v5 :as v5]
            [settlement.visual-v6 :as v6]
            [settlement.massive-mountain :as mountain]))

(defn cells []
  (for [y (range config/world-height) x (range config/world-width)] [x y]))

(def ^:dynamic *seed* config/world-seed)
(def red-spawn [(- config/world-width 24) 25])
(def crossings [14 25 36 48 58 79 83 92])
(def waterfall-rows [9 43 65 88])
(def camps [[18 18] [81 18] [18 32] [81 32]])

(defn- value [salt x y]
  (/ (double (bit-and 65535 (hash [*seed* salt x y]))) 65535.0))

(defn- blend [a b t] (+ a (* (- b a) t)))
(defn- ease [t] (* t t (- 3.0 (* 2.0 t))))

(defn- noise [salt scale x y]
  (let [u (/ (double x) scale) v (/ (double y) scale)
        i (long (Math/floor u)) j (long (Math/floor v))
        fx (ease (- u i)) fy (ease (- v j))]
    (blend (blend (value salt i j) (value salt (inc i) j) fx)
           (blend (value salt i (inc j)) (value salt (inc i) (inc j)) fx) fy)))

(defn- field [salt x y]
  (+ (* 0.67 (noise salt 22 x y)) (* 0.25 (noise salt 9 x y))
     (* 0.08 (noise salt 4 x y))))

(defn variation [salt [x y]] (field salt x y))

(defn- basin [x y cx cy rx ry]
  (max 0.0 (- 1.0 (+ (Math/pow (/ (- x cx) (double rx)) 2)
                      (Math/pow (/ (- y cy) (double ry)) 2)))))

(defn climate [[x y]]
  (let [ridge (basin x y 23 75 35 34)
        heat (basin x y 85 82 37 33)
        marsh (basin x y 68 55 17 15)
        forest (max (basin x y 12 17 28 30) (basin x y 86 16 24 25)
                    (basin x y 34 36 22 22) (basin x y 93 38 20 20))
        height (+ (* 0.7 ridge) (* 0.33 (field :height x y)))
        moisture (+ (* 0.51 forest) (* 0.63 marsh) (* 0.33 (field :moisture x y)) (- (* 0.4 heat)))
        temperature (+ 0.32 (* 0.59 heat) (* 0.29 (field :temperature x y))
                       (- (* 0.53 (basin x y 23 89 32 18))))]
    {:height height :moisture moisture :temperature temperature :marsh marsh
     :vegetation (max 0.0 (min 1.0 (+ (* 0.64 moisture) (* 0.36 (field :groves x y))
                                      (- (* 0.21 height)) (- (* 0.37 heat)))))}))

(defn classify [{:keys [height moisture temperature marsh]}]
  (cond
    (and (> height 0.35) (< temperature 0.19)) :snow
    (> height 0.51) :mountains
    (> temperature 0.65) :desert
    (and (> marsh 0.27) (> moisture 0.52)) :swamp
    (> moisture 0.48) :conifer
    (> moisture 0.31) :deciduous
    :else :plains))

(defn river-center [y]
  (+ 50.0 (* 2.6 (Math/sin (* 0.18 y))) (* 1.2 (Math/cos (* 0.29 y)))
     (* 2.0 (- (noise :river 18 50 y) 0.5))))

(defn- river-width [y]
  (let [distance (reduce min (map #(abs (- y (+ % 0.5))) crossings))]
    (+ 0.92 (* 0.59 (min 1.0 (/ distance 4.0)))
       (* 0.66 (noise :channel 11 50 y)))))

(def lake-sites
  [[49 70 7.5 5.8] [91 55 5.2 4.2] [9 48 4.2 3.0] [24 69 4.5 3.1]
   [70 47 3.6 2.7] [90 79 3.4 2.6] [75 89 3.8 2.7]])

(def oasis-sites [[90 79] [75 89]])

(defn tributaries []
  (mapv (fn [[source-x source-y join-y]]
          (let [join-x (river-center join-y)]
            (mapv (fn [part]
                    (let [t (/ part 64.0)
                          x (blend source-x join-x t)
                          y (blend source-y join-y t)
                          bend (* (Math/sin (* Math/PI t))
                                  (- (* 8 (noise :tributary 9 x y))
                                     (* 5 (:height (climate [x y])))))]
                      [x (+ y bend)]))
                  (range 65))))
        [[35 49 54] [24 69 69] [70 47 51] [66 73 74]]))

(defn- water-model [seed]
  (binding [*seed* seed]
    (let [tributary-cells
          (set (for [route (tributaries) [x y] route
                     dx [-1 0 1] dy [-1 0 1]
                     :when (< (+ (abs dx) (abs dy)) 2)]
                 [(long (Math/round (+ (* 2 x) dx))) (long (Math/round (+ (* 2 y) dy)))]))
          lake? (fn [x y]
                  (some (fn [[cx cy rx ry]]
                          (let [u (/ (- x cx (* 2.4 (- (value :lake cx cy) 0.5))) rx)
                                v (/ (- y cy) ry)
                                angle (Math/atan2 v u)
                                shape (+ (* u u) (* v v))]
                            (< shape (+ 0.91 (* 0.19 (Math/sin (+ (* 3 angle) (* cx 0.71))))
                                        (* 0.11 (Math/cos (+ (* 5 angle) cy)))
                                        (* 0.22 (- (noise :shore 4 x y) 0.5))))))
                        lake-sites))
          marsh-pools [[63 51 1.65] [65 54 1.3] [68 55 1.65] [71 52 1.25]
                       [71 59 1.7] [66 61 1.4] [73 56 1.25]]
          marsh? (fn [x y]
                   (some (fn [[cx cy radius]]
                           (< (+ (Math/pow (- x cx) 2) (* 1.25 (Math/pow (- y cy) 2)))
                              (* radius radius (+ 0.77 (* 0.46 (noise :pool 3 x y))))))
                         marsh-pools))
          wet? (fn [x y]
                 (or (< (abs (- x (river-center y))) (river-width y))
                     (tributary-cells [(Math/round (double (* 2 x))) (Math/round (double (* 2 y)))])
                     (and (lake? x y)
                          (not (and v5/enabled?
                                    (< (+ (Math/pow (- x 92) 2) (* 1.8 (Math/pow (- y 55) 2))) 1.9))))
                     (marsh? x y)))
          indices (vec (cells))
          center-water (into #{} (filter (fn [[x y]] (wet? (+ x 0.5) (+ y 0.5)))) indices)
          corners (into {} (for [y (range (inc config/world-height)) x (range (inc config/world-width))]
                             [[x y] (if (wet? x y) 0 1)]))
          masks (into {} (for [[x y :as p] indices]
                           [p (mapv corners [p [(inc x) y] [(inc x) (inc y)] [x (inc y)]])]))
          wet (into center-water (keep (fn [[p mask]] (when (<= (reduce + mask) 2) p))) masks)
          bridges (into {}
                        (for [y crossings
                              :let [x (long (Math/floor (river-center (+ y 0.5))))
                                    west (first (remove #(wet [% y]) (iterate dec x)))
                                    east (first (remove #(wet [% y]) (iterate inc x)))]]
                          [[x y] {:west west :east east}]))
          bridges (if v5/enabled?
                    (into bridges
                          (keep (fn [[x y]]
                                  (when (wet [x y])
                                    (let [west (first (remove #(wet [% y]) (iterate dec x)))
                                          east (first (remove #(wet [% y]) (iterate inc x)))]
                                      (when (<= 2 (- east west) 5)
                                        [[x y] {:west west :east east :marsh? true}])))))
                          [[65 54] [71 59]])
                    bridges)
          bridge-cells (into #{} (mapcat (fn [[[_ y] {:keys [west east]}]]
                                          (map #(vector % y) (range west (inc east))))) bridges)
          water-blocks (into {} (map #(vector % :water)) (remove bridge-cells wet))]
      {:shore-masks masks :wet-cells wet :bridges bridges :bridge-cells bridge-cells
       :water-blocks water-blocks})))

(defn generate [seed]
  (binding [*seed* seed]
    (assoc (water-model seed) :seed seed
           :climate (into {} (map (juxt identity climate)) (cells)))))

(def active (generate config/world-seed))
(def ^:dynamic *landscape* active)

(defn biome [[x y :as cell]]
  (classify (or ((:climate *landscape*) cell) (climate [x y]))))

(def shore-kinds
  {[1 1 0 0] "straight_n" [0 1 1 0] "straight_e"
   [0 0 1 1] "straight_s" [1 0 0 1] "straight_w"
   [1 0 0 0] "outer_nw" [0 1 0 0] "outer_ne"
   [0 0 1 0] "outer_se" [0 0 0 1] "outer_sw"
   [0 1 1 1] "inner_nw" [1 0 1 1] "inner_ne"
   [1 1 0 1] "inner_se" [1 1 1 0] "inner_sw"
   [1 0 1 0] "diagonal_nw_se" [0 1 0 1] "diagonal_ne_sw"})

(def ^:dynamic shore-masks (:shore-masks active))
(def ^:dynamic wet-cells (:wet-cells active))
(def ^:dynamic bridges (:bridges active))
(def ^:dynamic bridge-cells (:bridge-cells active))
(def ^:dynamic water-blocks (:water-blocks active))

(defn terrain [p]
  (case (biome p)
    :snow :snow
    :mountains :rocky_ground
    :desert :desert
    :swamp :swamp
    :grass))

(defn shore-ground [p]
  (case (biome p) :mountains "rocky_ground" :desert "sand" "grass"))


;; These landmarks occupy their ground footprints. Upper facades and peaks
;; may overhang them, just as the canopies of resource trees do.
(def ^:private legacy-landmarks
  {[12 60] [:massif 4 2] [16 61] [:escarpment 3 2] [13 62] [:escarpment 4 2]
   [17 63] [:massif 4 2] [20 67] [:massif 3 2] [16 73] [:escarpment 3 2]
   [29 63] [:escarpment 2 2] [28 73] [:cave 2 1] [23 74] [:crystals 1 1]
   [10 70] [:iron-ore 1 1] [34 68] [:gold-ore 1 1]
   [13 85] [:snow-ridge 4 2] [17 86] [:snow-ridge 3 2] [20 88] [:snow-ridge 4 2]
   [32 84] [:snow-ridge 3 2] [28 91] [:escarpment 2 2]
   [83 75] [:sand-ridge 3 2] [91 82] [:sand-ridge 3 2] [73 88] [:sand-boulders 2 1]
   [84 94] [:sand-boulders 2 1] [86 66] [:palm 1 1] [92 71] [:palm 1 1]
   [79 81] [:cactus 1 1] [89 76] [:flowering-cactus 1 1] [94 93] [:cactus 1 1]
   [18 30] [:sawmill 2 2] [29 31] [:watchtower 2 1] [28 61] [:mine 2 1]
   [68 29] [:sawmill 2 2] [72 31] [:watchtower 2 1]
   [79 31] [:well 1 1] [19 27] [:well 1 1] [80 20] [:tent 2 1]
   [43 77] [:tent 2 1] [65 85] [:tent 2 1]
   [35 29] [:gate 1 1] [22 28] [:blue-flag 1 1] [77 28] [:red-flag 1 1]
   [31 26] [:barrel 1 1] [36 27] [:crate 1 1] [72 28] [:chest 1 1]
   [20 32] [:log-bundle 1 1] [28 60] [:stone-pile 1 1] [78 30] [:campfire 1 1]
   [17 17] [:raider-tent 1 1] [19 17] [:ember-pit 1 1]
   [82 17] [:raider-tent 1 1] [80 17] [:ember-pit 1 1]
   [17 33] [:raider-tent 1 1] [19 33] [:ember-pit 1 1]
   [82 33] [:raider-tent 1 1] [80 33] [:ember-pit 1 1]
   [82 84] [:ruined-wall 2 1] [92 78] [:palm 1 1] [76 86] [:palm 1 1]
   [35 65] [:cairn 1 1]})

(def ^:private landmarks-v5
  (cond-> legacy-landmarks
    v5/enabled?
    (merge {[5 61] [:escarpment 4 2] [7 64] [:massif 5 2]
            [6 67] [:escarpment 4 2] [11 66] [:massif 4 2]
            [22 65] [:escarpment 3 2] [11 79] [:snow-ridge 4 2]
            [8 83] [:snow-ridge 4 2] [13 88] [:escarpment 4 2]
            [24 87] [:snow-ridge 4 2] [18 93] [:snow-ridge 5 2]
            [82 72] [:sand-ridge 4 2] [80 76] [:sand-ridge 3 2]
            [85 77] [:sand-ridge 4 2] [86 86] [:sand-ridge 4 2]
            [89 89] [:sand-ridge 5 2]})))

(def landmarks
  (if v6/enabled?
    (cond-> (into v6/ridges (remove (comp #{:massif :escarpment :snow-ridge :sand-ridge} first val)) landmarks-v5)
      mountain/enabled? (->> (remove (fn [[[x y] _]] (and (< x 35) (<= 55 y 93)))) (into {})))
    landmarks-v5))

(defn footprint [[x y] [_ width height]]
  (for [dy (range height) dx (range width)] [(+ x dx) (+ y dy)]))

(defn place-landmarks [reserved]
  (reduce (fn [objects [anchor [kind :as spec]]]
            (let [positions (footprint anchor spec)]
              (if (every? #(and (config/in-bounds? %) (not (reserved %))
                                (not (wet-cells %)) (not (objects %))) positions)
                (assoc (into objects (map #(vector % :scenery-footprint)) positions) anchor kind)
                objects)))
          {} (sort-by key landmarks)))
