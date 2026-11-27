(ns settlement.road-art
  (:require [settlement.config :as config]
            [settlement.landscape :as landscape]
            [settlement.layout :as layout]
            [settlement.visual-v6 :as v6])
  (:import [java.awt BasicStroke Color Graphics2D]
           [java.awt.geom Path2D$Double]
           [java.awt.image BufferedImage]))

;; Geometry uses half-resolution world pixels. Adjacent tiles clip the same
;; curves and use the same coordinate-based texture, without drawing tile caps.
(def ^:private size (quot config/tile-size 2))
(def ^:private materials
  {:stone [13 [126 116 91] [105 98 78]]
   :dirt [12 [134 106 67] [112 87 55]]
   :trade [17 [146 115 77] [123 102 70]]
   :forest [6 [113 93 55] [96 79 46]]
   :mountain [7 [122 111 94] [92 85 76]]
   :sand [7 [174 125 76] [160 115 68]]})

(defn- mid [a b] (mapv #(/ (+ %1 %2) 2.0) a b))
(defn- color [[r g b]] (Color. (int r) (int g) (int b)))
(defn- center [[x y :as p]]
  (let [jitter (fn [salt] (- (mod (hash [p salt]) 9) 4))]
    [(+ (* x size) 16 (if (landscape/bridge-cells p) 0 (jitter :x)))
     (+ (* y size) (if (landscape/bridge-cells p) 13 (+ 16 (jitter :y))))]))

(defn- point! [^Path2D$Double path [x y]] (.moveTo path (double x) (double y)))
(defn- line! [^Path2D$Double path [x y]] (.lineTo path (double x) (double y)))

(defn- links [p]
  (filterv #(layout/road-link? p %) (layout/neighbors p)))

(defn- road-center [p]
  (let [[x y :as own] (center p)
        nearby (links p)]
    (if (and (= 2 (count nearby)) (not (landscape/bridge-cells p)))
      (letfn [(along [previous cell depth]
                (let [options (links cell)]
                  (if (or (zero? depth) (not= 2 (count options)) (landscape/bridge-cells cell))
                    (center cell)
                    (recur cell (first (remove #{previous} options)) (dec depth)))))]
        (let [[tx ty] (mid (along p (first nearby) 1) (along p (second nearby) 1))]
          [(+ x (max -11 (min 11 (* 0.68 (- tx x)))))
           (+ y (max -11 (min 11 (* 0.68 (- ty y)))))]))
      own)))

(def ^:private layouts (memoize road-center))

(def ^:private segments
  (memoize
   (fn [p]
     (when (layout/road-cells p)
       (let [origin (layouts p)
             links (links p)
             ends (mapv #(mid origin (layouts %)) links)
             [x y] p
             key (if (and v6/enabled? (<= 41 x 77) (<= 24 y 26)
                          (= :dirt (layout/road-material p)))
                   :trade (layout/road-material p))
             [width base shade] (materials key)
             path (Path2D$Double.)]
         (if (= 2 (count ends))
           (let [[a [bx by]] ends [ox oy] origin]
             (point! path a)
             (.quadTo path (double ox) (double oy) (double bx) (double by)))
           (doseq [end ends]
             (point! path origin)
             (line! path end)))
         {:path path :size (+ width (* 0.85 (- (mod (hash p) 5) 2)))
          :key key :base base :shade shade :center origin})))))

(defn- stroke! [^Graphics2D g {:keys [path size]} amount hue]
  (.setStroke g (BasicStroke. (float (+ size amount)) BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
  (.setColor g (color hue))
  (.draw g ^Path2D$Double path))

(defn- nearest-materials [paths gx gy]
  (reduce
   (fn [[a da b db :as best] {:keys [center] :as item}]
     (let [[x y] center dx (- gx x) dy (- gy y) distance (+ (* dx dx) (* dy dy))]
       (cond
         (< distance da) [item distance a da]
         (< distance db) [a da item distance]
         :else best)))
   [nil Double/POSITIVE_INFINITY nil Double/POSITIVE_INFINITY] paths))

(defn- raster [[x y :as p]]
  (when-not (landscape/wet-cells p)
    (let [paths (keep segments (for [j (range (dec y) (+ y 2)) i (range (dec x) (+ x 2))] [i j]))]
      (when (seq paths)
        (let [mask (BufferedImage. size size BufferedImage/TYPE_INT_ARGB)
              ^Graphics2D g (.createGraphics mask)
              ox (* x size) oy (* y size)
              output (BufferedImage. (* size 2) (* size 2) BufferedImage/TYPE_INT_ARGB)]
          (try
            (.translate g (int (- ox)) (int (- oy)))
            (doseq [s paths] (stroke! g s 6 [40 40 40]))
            (doseq [s paths] (stroke! g s 0 [80 80 80]))
            (when v6/enabled?
              (doseq [{:keys [key] :as s} paths :when (= key :trade)]
                (stroke! g s -5 [60 60 60])
                (stroke! g s -8 [80 80 80])))
            (finally (.dispose g)))
          (doseq [v (range size) u (range size)
                  :let [pixel (.getRGB mask u v)]
                  :when (neg? pixel)]
            (let [gx (+ u ox) gy (+ v oy)
                  grain (mod (hash [gx gy :road]) 31)
                  [m near-distance neighbor other-distance] (nearest-materials paths gx gy)
                  edge? (= 40 (bit-and pixel 255))
                  palette (if edge? :shade :base)
                  colors (palette m)
                  nearby (when neighbor (palette neighbor))
                  mix (if (and nearby (not= (:key m) (:key neighbor)))
                        (max 0.0 (- 0.5 (/ (- other-distance near-distance) 380.0))) 0.0)
                  seam? (or (zero? (mod gy 5)) (zero? (mod (+ gx (* 4 (quot gy 5))) 9)))
                  stone? (and (= :stone (:key m)) (not edge?))
                  wheel-rut? (or (= 60 (bit-and pixel 255))
                                 (and (= :dirt (:key m)) (not edge?)
                                      (< (abs (- (mod (+ gx (* 2 gy)) 23) 11)) 2)))
                  tone (cond (and stone? seam?) -13 (< grain 4) -14 (> grain 26) 15
                             wheel-rut? -8 :else 0)
                  alpha (if edge?
                          (if v6/enabled? (+ 53 (* 4 grain)) (if (< grain 4) 92 151))
                          255)
                  rgba (unchecked-int
                        (reduce (fn [c [shift v next]]
                                  (bit-or c (bit-shift-left (+ (int (+ v (* mix (- next v)))) tone) shift)))
                                (bit-shift-left alpha 24) (map vector [16 8 0] colors (or nearby colors))))]
              (doseq [dv [0 1] du [0 1]]
                (.setRGB output (+ (* 2 u) du) (+ (* 2 v) dv) rgba))))
          output)))))

(def image (memoize raster))
