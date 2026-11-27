(ns settlement.forest-ground
  "Seed-aware cached woodland material, masked by the unchanged world layout."
  (:require [clojure.java.io :as io]
            [settlement.config :as config]
            [settlement.desert-canyon :as canyon]
            [settlement.landscape :as landscape]
            [settlement.layout :as layout]
            [settlement.massive-mountain :as mountain]
            [settlement.mountain-lake :as lake])
  (:import [java.awt Graphics2D RenderingHints]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]))

(defn- in-region? [[x y] {:keys [origin size]}]
  (when origin
    (let [[ox oy] origin [w h] size]
      (and (<= ox x) (< x (+ ox w)) (<= oy y) (< y (+ oy h))))))

(defn- protected? [p]
  (or (in-region? p mountain/model)
      (in-region? p canyon/model)
      (and lake/enabled? (lake/affected-cell? p))
      (landscape/wet-cells p)
      (landscape/bridge-cells p)
      (layout/house-positions p)))

(def ^:private site-map
  (delay
    (into {}
          (for [x (range config/world-width) y (range config/world-height)
                :let [p [x y]]
                :when (not (protected? p))
                :let [site (case (landscape/biome p)
                             :deciduous :leaf
                             :conifer :pine
                             :plains (when (layout/forest? p) :leaf)
                             nil)]
                :when site]
            [p site]))))

(defn site [p] (get @site-map p))

(defn crown-site [p]
  (when-let [base (site p)]
    ;; A few trees from the adjacent stand occur on each side of a mixed edge.
    (let [other (if (= :leaf base) :pine :leaf)]
      (if (and (< (mod (hash [p :mixed]) 8) 3)
               (some #(= other (site %)) (layout/neighbors p)))
        other
        base))))

(def ^:private corners
  (delay
    (into {}
          (for [x (range (inc config/world-width))
                y (range (inc config/world-height))
                :let [cells [[(dec x) (dec y)] [x (dec y)] [(dec x) y] [x y]]
                      counts (frequencies (keep site cells))
                      road? (some layout/road-cells cells)
                      reserve? (some #(and (config/in-bounds? %) (protected? %)) cells)
                      strength (cond reserve? 0.0 road? 0.22 :else 1.0)]]
            [[x y] [(* strength 0.25 (get counts :leaf 0))
                    (* strength 0.25 (get counts :pine 0))]]))))

(def ^:private clearing-weight
  (memoize
   (fn [x y]
     (let [cells [[(dec x) (dec y)] [x (dec y)] [(dec x) y] [x y]]]
       (- 1.0 (* 0.1125 (count (filter layout/clearing? cells))))))))

(def ^:private material
  (memoize (fn [kind]
             (let [^BufferedImage im (ImageIO/read
                                      (io/resource (str "images/forest_v6_5_2/" (name kind) "-ground.png")))
                   w (.getWidth im) h (.getHeight im)]
               (.getRGB im 0 0 w h nil 0 w)))))

(def ^:private cells-per-chunk 5)
(def ^:private sample-size 32)
(def ^:private material-size 512)

(defn- mirror ^long [^long p]
  (let [v (bit-and p (dec (* 2 material-size)))]
    (if (< v material-size) v (- (* 2 material-size) v 1))))

(defn- mix-channel ^long [^long leaf ^long pine ^long shift ^double mix]
  (let [a (bit-and 255 (unsigned-bit-shift-right leaf shift))
        b (bit-and 255 (unsigned-bit-shift-right pine shift))]
    (bit-shift-left (long (+ a (* mix (- b a)))) shift)))

(defn- woodland-edge? [[x y :as position]]
  (and (config/in-bounds? position)
       (not (protected? position))
       (not (layout/road-cells position))
       (some site (for [dy [-1 0 1] dx [-1 0 1]] [(+ x dx) (+ y dy)]))))

(defn- paint-cell! [^ints pixels stride offset x y blend-clearings?]
  (let [[a e] (@corners [x y]) [b f] (@corners [(inc x) y])
        [c g] (@corners [x (inc y)]) [d h] (@corners [(inc x) (inc y)])
        ^ints leaf (material :leaf) ^ints pine (material :pine)
        ox (* x sample-size) oy (* y sample-size)
        edge? (if blend-clearings?
                (fn [p] (or (protected? p) (layout/road-cells p)))
                (complement site))
        west? (edge? [(dec x) y]) east? (edge? [(inc x) y])
        north? (edge? [x (dec y)]) south? (edge? [x (inc y)])
        road-scale (if (layout/road-cells [x y]) 0.0
                       (if (and (not blend-clearings?) (layout/clearing? [x y])) 0.55 1.0))
        [nw ne sw se] (if blend-clearings?
                        [(clearing-weight x y) (clearing-weight (inc x) y)
                         (clearing-weight x (inc y)) (clearing-weight (inc x) (inc y))]
                        [1.0 1.0 1.0 1.0])]
    (dotimes [v sample-size]
      (let [t (/ (+ v 0.5) sample-size)
            l (+ (double a) (* t (- (double c) (double a))))
            r (+ (double b) (* t (- (double d) (double b))))
            p (+ (double e) (* t (- (double g) (double e))))
            q (+ (double f) (* t (- (double h) (double f))))
            wl (+ (double nw) (* t (- (double sw) (double nw))))
            wr (+ (double ne) (* t (- (double se) (double ne))))
            feather-y (min 1.0 (if north? (* 3.0 t) 1.0)
                           (if south? (* 3.0 (- 1.0 t)) 1.0))
            row (+ (long offset) (* v (long stride)))
            texture-row (* material-size (mirror (+ oy v)))]
        (dotimes [u sample-size]
          (let [s (/ (+ u 0.5) sample-size)
                foliage (+ l (* s (- r l)))
                needles (+ p (* s (- q p)))
                total (+ foliage needles)
                uncovered (- 1.0 total)
                coverage (if blend-clearings?
                           (- 1.0 (* uncovered uncovered uncovered uncovered))
                           total)
                feather-x (min 1.0 (if west? (* 3.0 s) 1.0)
                               (if east? (* 3.0 (- 1.0 s)) 1.0))
                weight (* road-scale (+ wl (* s (- wr wl)))
                          feather-x feather-y coverage coverage (- 3.0 (* 2.0 coverage)))
                alpha (long (* 247.0 weight))]
            (when (pos? alpha)
              (let [i (+ texture-row (mirror (+ ox u)))
                    leaf-rgb (aget leaf i)
                    pine-rgb (aget pine i)
                    mix (/ needles total)]
                (aset-int pixels (+ row u)
                          (unchecked-int
                           (bit-or (bit-shift-left alpha 24)
                                   (mix-channel leaf-rgb pine-rgb 16 mix)
                                   (mix-channel leaf-rgb pine-rgb 8 mix)
                                   (mix-channel leaf-rgb pine-rgb 0 mix))))))))))))

(def ^:private ground-chunk
  (memoize
   (fn [cx cy blend-clearings?]
     (let [x0 (* cx cells-per-chunk) y0 (* cy cells-per-chunk)
           cells (for [dy (range cells-per-chunk) dx (range cells-per-chunk)
                       :let [p [(+ x0 dx) (+ y0 dy)]]
                       :when (or (site p) (and blend-clearings? (woodland-edge? p)))]
                   [dx dy])]
       (when (seq cells)
         (let [n (* cells-per-chunk sample-size)
               image (BufferedImage. n n BufferedImage/TYPE_INT_ARGB_PRE)
               pixels (int-array (* n n))]
           (doseq [[dx dy] cells]
             (paint-cell! pixels n (+ (* dy sample-size n) (* dx sample-size))
                          (+ x0 dx) (+ y0 dy) blend-clearings?))
           (.setRGB image 0 0 n n pixels 0 n)
           image))))))

(defn draw! [^Graphics2D g {:keys [x y width height]} blend-clearings?]
  (let [span (* config/tile-size cells-per-chunk)
        old (.getRenderingHint g RenderingHints/KEY_INTERPOLATION)]
    (try
      (.setRenderingHint g RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
      (doseq [cy (range (max 0 (quot (long y) span)) (inc (quot (dec (long (Math/ceil (+ y height)))) span)))
              cx (range (max 0 (quot (long x) span)) (inc (quot (dec (long (Math/ceil (+ x width)))) span)))
              :let [^BufferedImage image (ground-chunk cx cy blend-clearings?)]
              :when image]
        (.drawImage g image (int (* cx span)) (int (* cy span)) (int span) (int span) nil))
      (finally
        (.setRenderingHint g RenderingHints/KEY_INTERPOLATION
                           (or old RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR))))))
