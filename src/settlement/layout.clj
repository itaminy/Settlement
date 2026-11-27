(ns settlement.layout
  (:require [settlement.config :as config]
            [settlement.landscape :as landscape]
            [settlement.visual-v5 :as v5]
            [settlement.visual-v6 :as v6]
            [settlement.massive-mountain :as mountain]
            [settlement.desert-canyon :as canyon]
            [clojure.set :as set])
  (:import [java.util Random Collections ArrayList]))

(def spawn-position [23 25])
(def house-positions #{[31 23] [36 25] [31 29]})
(def groves [[9 10 8 8] [39 9 8 7] [8 39 9 8] [40 40 9 8] [7 22 6 7] [26 16 7 5]])
(def clearings [[23 25 8 6] [22 9 5 4] [8 28 5 4] [24 42 6 4]])
(def rocky-areas [[17 8 5 4] [43 23 6 7] [18 38 5 4]])
(def zone-anchors (cond-> {:start [23 25] :village [33 27] :north-forest [10 13]
                   :east-forest [39 10] :west-meadow [8 28] :south-meadow [24 42]
                   :southwest-forest [9 39] :southeast-forest [40 40]
                   :east-rocks [42 25] :north-rocks [17 8] :south-rocks [18 38]
                   :red-start landscape/red-spawn :highlands [26 62] :snow [25 84]
                   :desert [76 83] :wetlands [67 58] :lake-view [39 74]}
                    mountain/enabled? (assoc :highlands mountain/entrance :snow mountain/summit)))

(defn- ellipse [[x y] [cx cy rx ry]]
  (+ (Math/pow (/ (- x cx) (double rx)) 2) (Math/pow (/ (- y cy) (double ry)) 2)))
(defn- organic-inside? [[x y :as p] shape]
  (< (ellipse p shape) (+ 1.0 (* 0.13 (Math/sin (+ (* x 0.71) (* y 0.37))))
                              (* 0.09 (Math/cos (- (* x 0.43) (* y 0.61)))))))
(defn start-area? [p]
  (boolean (some (fn [[x y]] (organic-inside? p [x y 3.5 2.5]))
                 [spawn-position landscape/red-spawn])))
(defn house-clearance? [[x y]]
  (boolean (some (fn [[hx hy]] (and (<= (abs (- x hx)) 2) (<= (abs (- y hy)) 2))) house-positions)))
(defn clearing? [p] (boolean (some #(organic-inside? p %) clearings)))
(defn forest? [p] (boolean (or (#{:conifer :deciduous} (landscape/biome p))
                              (some #(organic-inside? p %) groves))))
(defn rocky? [p] (boolean (or (#{:mountains :snow} (landscape/biome p))
                             (some #(organic-inside? p %) rocky-areas))))
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
   [[24 40] [28 42] [31 41] [35 41] [40 40]]
   [[42 25] [49 25] [56 25] [66 25] landscape/red-spawn]
   [[24 42] [24 51] [26 58] [26 62]]
   [[26 58] [36 58] [44 58] [51 58] [59 58] [67 58]]
   [[67 58] [67 47] [69 37] [73 30] landscape/red-spawn]
   [[26 62] [28 69] [26 77] [25 84]]
   [[28 69] [35 68] [40 68]]
   [[25 84] [36 83] [44 83] [52 83] [62 83] [76 83]]
   [[76 83] [74 73] [70 66] [67 58]]])

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

(defn- flood [allowed origin]
  (loop [frontier #{origin} seen #{origin}]
    (if (empty? frontier) seen
      (let [next (into #{} (comp (mapcat neighbors) (filter allowed) (remove seen)) frontier)]
        (recur next (into seen next))))))

(defn- connect-road [land? destination? start]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start) parents {start nil}]
    (when-let [cell (peek queue)]
      (if (destination? cell)
        (take-while some? (iterate parents cell))
        (let [next (filter #(and (land? %) (not (contains? parents %))) (neighbors cell))]
          (recur (into (pop queue) next) (reduce #(assoc %1 %2 cell) parents next)))))))

(defn- simplify-roads [roads terminals]
  (let [without-leaves (fn [network]
                         (loop [roads network]
                           (let [leaves (remove #(or (terminals %)
                                                    (<= 2 (count (filter roads (neighbors %))))) roads)]
                             (if (seq leaves) (recur (reduce disj roads leaves)) roads))))
        roads (without-leaves roads)
        trimmed (reduce (fn [network [x y :as p]]
                          (let [square [p [(inc x) y] [x (inc y)] [(inc x) (inc y)]]]
                            (or (when (every? network square)
                                  (some (fn [cell]
                                          (let [without (disj network cell)]
                                            (when (= without (flood without spawn-position)) without)))
                                        (remove terminals square)))
                                network)))
                        roads (sort roads))]
    (without-leaves trimmed)))

(defn- generate-base-roads []
  (let [houses (into house-positions (map (fn [[x y]] [(inc x) y])) house-positions)
        land? #(not (or (landscape/water-blocks %) (houses %) (mountain/blocks %)))
        dry (flood land? spawn-position)
        terminals (into #{} (filter dry)
                        (concat landscape/bridge-cells landscape/camps mountain/trail (vals zone-anchors)
                                (map (fn [[x y]] [x (inc y)]) house-positions)
                                (cond-> [[22 9] [88 75] [17 65]] v5/enabled? (into [[79 94] [73 53]]))))
        desired (into terminals (filter dry)
                      (concat landscape/bridge-cells landscape/camps
                              (vals zone-anchors)
                              (mapcat (fn [route]
                                        (let [samples (curve route)]
                                          (concat samples (for [[[x1 y1] [x2 y2]] (partition 2 1 samples)
                                                                :when (and (not= x1 x2) (not= y1 y2))]
                                                            [x2 y1]))))
                                      (cond-> (if mountain/enabled?
                                                 (into (vec (remove (fn [route] (#{[26 62] [28 69] [25 84]} (first route))) path-routes))
                                                       [[[24 51] [22 54] [20 55]]
                                                        [[39 69] [39 75] [42 82] [52 83] [76 83]]])
                                                 path-routes)
                                        v5/enabled? (into [[[76 83] [79 80] [83 79] [88 75]]
                                                           [[76 83] [74 86] [74 91] [79 94]]
                                                           [[26 62] [22 64] [17 65]]
                                                           [[67 58] [68 56] [71 54] [73 53]]])))))]
    (loop [roads desired]
      (let [network (flood roads spawn-position)]
        (if-let [missing (first (sort (remove network roads)))]
          (recur (into roads (connect-road land? network missing)))
          (simplify-roads roads terminals))))))

(defn- add-canyon-roads [base]
  (if-not canyon/enabled?
    base
    (let [land? (fn [p]
                  (and (not (or (canyon/blocks p) (landscape/water-blocks p)))
                       (or (base p) (canyon/within? p))))
          reachable (flood land? spawn-position)
          desired (into (into #{} (filter reachable) base) (filter reachable canyon/routes))]
      (loop [roads desired]
        (let [linked (flood roads spawn-position)]
          (if-let [p (first (sort (remove linked roads)))]
            (recur (into roads (connect-road land? linked p)))
            roads))))))

(def ^:dynamic base-road-cells (generate-base-roads))
(defn generate-roads [] (add-canyon-roads (generate-base-roads)))
(def ^:dynamic road-cells (add-canyon-roads base-road-cells))
(defn road? [x y] (contains? road-cells [x y]))
(defn road-link? [[ax ay :as a] [bx by :as b]]
  (and (road-cells a) (road-cells b)
       (or (and (mountain/route-cells a) (mountain/route-cells b))
           (and (canyon/within? a) (canyon/within? b))
           (not (and (= ay by)
                 (not (or (landscape/bridge-cells a) (landscape/bridge-cells b)))
                 (road-cells [ax (inc ay)])
                 (road-cells [bx (inc by)]))))))

(defn road-material [p]
  (cond
    (or (village? p) (start-area? p)) :stone
    (landscape/bridge-cells p) :dirt
    (mountain/route-cells p) :mountain
    :else (case (landscape/biome p)
            :desert :sand (:mountains :snow) :mountain
            (:conifer :deciduous) :forest :dirt)))
(defn courtyard? [p]
  (boolean (some (fn [[x y]] (organic-inside? p [x (inc y) 2.5 1.8])) house-positions)))
(defn dirt? [x y] (or (road? x y) (courtyard? [x y])))
(defn grass-variant [p] (cond (forest? p) 0 (clearing? p) 1 :else 2))

(defn reachable-cells [objects origin]
  (loop [visited #{origin} frontier #{origin}]
    (let [next-cells (set (for [p frontier n (neighbors p)
                                :when (and (not (contains? objects n)) (not (contains? visited n)))] n))]
      (if (empty? next-cells) visited (recur (into visited next-cells) next-cells)))))

(defn- removable? [kind] (contains? #{nil :tree :stone} kind))

(defn- carve-connection [objects origin destination?]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY origin) parents {origin nil}]
    (let [p (peek queue)]
      (if (destination? p)
        (loop [objects objects p p]
          (if p (recur (dissoc objects p) (parents p)) objects))
        (let [next-cells (remove #(or (contains? parents %) (not (removable? (objects %)))) (neighbors p))]
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

(defn generate-starters []
  (into {} (for [[dx dy kind] [[-5 -2 :tree] [-4 -3 :tree] [-1 -5 :stone]
                               [2 -5 :stone] [4 -3 :tree] [3 -4 :stone]
                               [4 3 :tree] [4 5 :tree] [-2 5 :stone] [-5 3 :tree]]
                 :let [positions (for [[x y] [spawn-position landscape/red-spawn]]
                                   [(+ x dx) (+ y dy)])]
                 :when (not-any? #(or (road-cells %) (house-clearance? %)) positions)
                 position positions]
             [position kind])))

(def ^:dynamic starter-resources (generate-starters))

(defn- starter-buffer? [[x y]]
  (some (fn [[cx cy]] (<= (+ (abs (- x cx)) (abs (- y cy))) 7))
        [spawn-position landscape/red-spawn]))

(defn- balance-starting-stock [objects]
  (reduce-kv
   (fn [result [x y :as blue] kind]
     (let [[bx by] spawn-position
           [rx ry] landscape/red-spawn
           red [(+ rx (- x bx)) (+ ry (- y by))]]
       (if (and (starter-resources red)
                (not= (= kind (objects blue)) (= kind (objects red))))
         (cond-> result (= kind (objects blue)) (dissoc blue)
                        (= kind (objects red)) (dissoc red))
         result)))
   objects starter-resources))

(defn- generate-base-objects []
  (let [random (Random. landscape/*seed*)
        reserved (into (into road-cells (keys starter-resources))
                       (concat (vals zone-anchors) mountain/platforms mountain/blocks
                               (filter start-area? (landscape/cells))))
        houses (into (zipmap house-positions (repeat :house))
                     (for [[x y] house-positions] [[(inc x) y] :scenery-footprint]))
        static (merge (zipmap mountain/blocks (repeat :terrain-blocked))
                      landscape/water-blocks
                      (landscape/place-landmarks reserved)
                      houses)
        connected (reachable-cells static spawn-position)
        static (into static (for [p (landscape/cells) :when (not (or (static p) (connected p)))]
                             [p :terrain-blocked]))
        candidates (shuffled-cells random)
        objects
        (reduce
         (fn [objects [x y :as p]]
           (if (or (objects p) (mountain/platforms p) (starter-buffer? p) (start-area? p) (house-clearance? p) (village? p) (clearing? p)
                   (dirt? x y) (some #(apply road? %) (neighbors p))) objects
             (let [r (.nextDouble random)
                   biome (landscape/biome p)
                   grove (landscape/variation :groves p)
                   density (get-in landscape/*landscape* [:climate p :vegetation] 0.5)
                   grove (if v5/enabled? (+ (* 0.72 grove) (* 0.28 density) 0.045) grove)
                   scree (landscape/variation :scree p)
                   tree-chance (cond (forest? p) (cond (> grove 0.57) 0.86 (> grove 0.43) (if v6/enabled? 0.57 0.42) :else 0.07)
                                     (#{:desert :mountains :snow} biome) 0
                                     (rocky? p) 0.04 :else 0.035)
                   stone-chance (if (rocky? p) (if (> scree 0.55) 0.32 0.065) 0.009)
                   kind (cond (< r stone-chance) :stone (< r (+ stone-chance tree-chance)) :tree)]
               (cond
                 (and (= kind :tree) (not-any? #(= :tree (objects %)) (if (and v6/enabled? (> grove 0.57)) (neighbors p) (near p 1)))) (assoc objects p :tree)
                 (and (= kind :stone) (< (count (filter #(= :stone (objects %)) (neighbors p))) 2)) (assoc objects p :stone)
                 :else objects)))) (merge static (select-keys starter-resources connected)) candidates)
        ;; Каждый добываемый объект должен иметь хотя бы одну свободную соседнюю клетку.
        accessible (reduce (fn [objects [p kind]]
                             (if (and (#{:tree :stone} kind) (every? #(contains? objects %) (neighbors p)))
                               (if-let [neighbor (first (filter #(#{:tree :stone} (objects %)) (neighbors p)))]
                                 (dissoc objects neighbor) (dissoc objects p)) objects))
                           objects (sort-by key objects))]
    (let [connected (connect-free-space accessible)]
      (if v6/enabled? (balance-starting-stock connected) connected))))

(defn- add-canyon-objects [base]
  (if-not canyon/enabled?
    base
    (let [terrain (into {}
                        (remove (fn [[p kind]]
                                  (and (canyon/within? p)
                                       (not= kind :water)
                                       (or (canyon/platforms p) (road-cells p)
                                           (not (canyon/oasis-margin? p))))))
                        base)
          terrain (into terrain (for [p canyon/blocks :when (not (landscape/water-blocks p))]
                                  [p :terrain-blocked]))
          reached (reachable-cells terrain spawn-position)]
      (into terrain
            (for [p (landscape/cells)
                  :when (and (canyon/within? p) (not (or (terrain p) (reached p))))]
              [p :terrain-blocked])))))

(defn- legacy-objects []
  (binding [road-cells base-road-cells starter-resources (binding [road-cells base-road-cells] (generate-starters))]
    (generate-base-objects)))

(defn generate-objects [] (add-canyon-objects (legacy-objects)))
(def ^:private baseline-objects (legacy-objects))
(def ^:dynamic initial-objects (add-canyon-objects baseline-objects))

(defn generate-world [seed]
  (let [{:keys [bridges bridge-cells wet-cells water-blocks shore-masks] :as model}
        (landscape/generate seed)]
    (binding [landscape/*seed* seed landscape/*landscape* model landscape/bridges bridges
              landscape/bridge-cells bridge-cells landscape/wet-cells wet-cells
              landscape/water-blocks water-blocks landscape/shore-masks shore-masks]
      (binding [base-road-cells (generate-base-roads)]
        (binding [road-cells (add-canyon-roads base-road-cells)]
          (assoc model :roads road-cells :objects (generate-objects)))))))

(defn spawn-cells []
  (sort-by (fn [[x y]] [(+ (abs (- x (first spawn-position))) (abs (- y (second spawn-position)))) y x])
           (landscape/cells)))

(def red-spawn-cells
  (delay (let [[cx cy] landscape/red-spawn]
           (sort-by (fn [[x y]] [(+ (abs (- x cx)) (abs (- y cy))) y x]) (landscape/cells)))))

(defn- biome-prop [p biome bank? roll]
  (let [patch (landscape/variation :microhabitat p)
        cluster (> patch 0.50)]
    (cond
      (and (= biome :desert) cluster (< roll 0.46))
      (cond (< roll 0.10) :saguaro (< roll 0.20) :scrub-cluster
            (< roll 0.33) :dune-field (< roll 0.43) :dry-channel :else :sand-boulders)
      (and (= biome :swamp) bank? (< roll 0.49))
      (cond (< roll 0.13) :rooted-snags (< roll 0.22) :drowned-log
            (< roll 0.35) :marsh-bank :else :rushes)
      (and (= biome :swamp) cluster (< roll 0.26))
      (if (< roll 0.08) :marsh-fern :marsh-bank)
      (and (#{:mountains :snow} biome) (< roll (if cluster 0.31 0.09))) :talus-fan
      (and (forest? p) cluster (< roll 0.16)) :forest-floor)))

(defn- generate-natural-decorations []
  (let [random (Random. (bit-xor 73191 landscape/*seed*))]
    (reduce
     (fn [decorations [x y :as p]]
       (if (or (contains? initial-objects p) (start-area? p) (house-clearance? p)
               (dirt? x y) (some #(contains? decorations %) (near p 1))) decorations
         (let [r (.nextDouble random)
               trees (some #(= :tree (initial-objects %)) (near p 1))
               near-path (some #(apply road? %) (neighbors p))
               biome (landscape/biome p)
               bank? (some landscape/wet-cells (neighbors p))
               reed-bed? (> (landscape/variation :reed-bed p) 0.49)
               kind (or (when v5/enabled? (biome-prop p biome bank? r))
                        (cond
                          (and bank? (= biome :swamp) (< r 0.08)) :dead-tree
                          (and bank? reed-bed? (< r 0.72))
                          (cond (< r 0.29) :rushes (< r 0.53) :reeds (< r 0.64) :bank-stones :else :shore-rocks)
                          (and bank? (< r 0.14)) :bank-stones
                          (and near-path (village? p) (< r 0.10)) :sign
                          (and trees (< r 0.17)) (if (< r 0.07) :mushroom-group :mushroom-red)
                          (and (forest? p) (< r 0.18)) :fallen-tree
                          (and (forest? p) (< r 0.21)) :stump
                          (and (forest? p) (< r 0.28)) (if (even? x) :forest-fern :fern)
                          (and (#{:conifer :deciduous :plains} biome) (< r 0.10)) :berry-bush
                          (and (#{:conifer :deciduous :plains} biome) (< r 0.16)) :bush
                          (and (= :plains biome) (< r 0.24))
                          (nth [:white-flowers :yellow-flowers :blue-flowers :tall-grass] (.nextInt random 4))
                          (and (= :swamp biome) (< r 0.24)) :tall-grass
                          (and (= :desert biome) (< r 0.13)) (cond (< r 0.04) :dune-grass (< r 0.09) :dry-bush :else :bones)))]
           (if kind (assoc decorations p kind) decorations)))) {} (shuffled-cells random))))

(defn- atmosphere []
  (into {}
        (for [[x y kind] (concat (for [[x y] landscape/oasis-sites
                                     [dx dy] [[-4 2] [-5 1] [-4 0] [2 -4] [3 -4] [3 3] [4 3]]]
                                 [(+ x dx) (+ y dy) :palm])
                               (for [[x y] landscape/camps
                                     [dx dy kind] [[1 0 :lantern] [-2 1 :raider-tent]
                                                   [-1 -2 :crate] [1 -2 :barrel] [2 1 :ruined-wall]]]
                                 [(+ x dx) (+ y dy) kind])
                               (for [[x y] house-positions] [(+ x 2) (inc y) :lantern]))
              :let [p [x y]]
              :when (not (or (initial-objects p) (road-cells p)))]
          [p kind])))

(defn- places-of-interest []
  (into {}
        (for [[origin props] [[[87 83] [:ruin-court :scrub-cluster :relict-well :dry-channel]]
                             [[79 94] [:abandoned-camp :relict-well :saguaro]]
                             [[39 12] [:abandoned-camp :forest-floor :forest-floor]]
                             [[91 79] [:palm :palm :palm :palm]]
                             [[75 89] [:palm :palm :palm :palm]]]
              :let [cells (sort-by (fn [[x y :as p]] [(ellipse p [(first origin) (second origin) 4 3]) x y])
                                   (near origin 4))]
              [p kind] (map vector (remove #(or (initial-objects %) (road-cells %)
                                                (courtyard? %) (start-area? %) (house-clearance? %))
                                           cells) props)]
          [p kind])))

(defn- alpine-trees [decorations]
  (into {}
        (comp (filter (fn [[p kind]] (and (= :talus-fan kind) (= :snow (landscape/biome p)))))
              (take 7)
              (map (fn [[p _]] [p :frost-pine])))
        (sort-by (fn [[[x y :as p]]] [(+ (abs (- x 22)) (abs (- y 88))) p]) decorations)))

(defn- marsh-hamlet [decorations]
  (when-let [anchor
             (first (for [y [55 54 56 52 53 57 58] x [62 63 64 65 66 67 68 69]
                          :let [p [x y]]
                          :when (and (= :swamp (landscape/biome p))
                                     (every? (fn [cell] (not (or (initial-objects cell)
                                                                   (dirt? (first cell) (second cell))
                                                                   (landscape/wet-cells cell))))
                                             (for [dx [-1 0 1] dy [-1 0]]
                                               [(+ x dx) (+ y dy)])))]
                      p))]
    (let [covered (filter #(<= (ellipse % [(first anchor) (second anchor) 2 2]) 1)
                          (keys decorations))]
      (assoc (apply dissoc decorations covered) anchor :marsh-hamlet))))

(defn generate-decorations []
  (let [decorations (binding [initial-objects baseline-objects road-cells base-road-cells]
                      (merge (generate-natural-decorations) (atmosphere)
                             (when v5/enabled? (places-of-interest))))]
    (let [decorations (if v6/enabled?
                        (let [marsh (or (marsh-hamlet decorations) decorations)]
                          (into marsh (alpine-trees marsh)))
                        decorations)]
      (cond-> decorations
        mountain/enabled? (as-> result (into (apply dissoc result mountain/platforms) mountain/props))
        canyon/enabled? (as-> result
                            (into (into {} (remove (fn [[p _]] (or (canyon/blocks p) (canyon/platforms p)
                                                                  (canyon/near-bridge? p) (road-cells p)))) result)
                                  (remove (fn [[p _]] (or (initial-objects p) (road-cells p))) canyon/props)))))))

(def decorations (generate-decorations))
