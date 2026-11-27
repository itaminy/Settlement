(ns settlement.mountain-lake
  "Pixel shoreline of the existing mountain lake and its outlet, for rendering only."
  (:require [settlement.config :as config]
            [settlement.landscape :as landscape]
            [settlement.massive-mountain :as mountain])
  (:import [java.awt.image BufferedImage]))

(def enabled? (contains? #{nil "6.3.2" "6.4" "6.4.8" "6.5.2" "7.0"} mountain/version))
;; Includes the foothill lake and the reach where its outlet leaves the mountain.
(def bounds [17 62 37 76])
(def ^:private tile config/tile-size)

(defn local-cell? [[x y]]
  (let [[left top right bottom] bounds]
    (and enabled? (<= left x (dec right)) (<= top y (dec bottom)))))

(defn- smooth [t]
  (let [t (max 0.0 (min 1.0 t))]
    (* t t (- 3.0 (* 2.0 t)))))

(defn blend-strength
  "Fade into the existing tributary outside the ridge, not across the basin."
  [world-x]
  (- 1.0 (smooth (/ (- world-x (* 33 tile)) (* 4.0 tile)))))

(defn- wet-field [wet? x y u v]
  (let [edge-weight (fn [distance] (* 0.5 (- 1.0 (smooth (/ distance 40.0)))))
        west (edge-weight u) east (edge-weight (- tile u))
        north (edge-weight v) south (edge-weight (- tile v))]
    (reduce + 0.0 (for [[dx a] [[-1 west] [0 (- 1 west east)] [1 east]]
                         [dy b] [[-1 north] [0 (- 1 north south)] [1 south]]
                         :when (and (pos? a) (pos? b) (wet? [(+ x dx) (+ y dy)]))]
                     (* a b)))))

(defn- threshold [px py]
  (+ 0.78
     (* 0.081 (Math/sin (+ (* px 0.037) (* py 0.041))))
     (* 0.025 (Math/sin (- (* px 0.133) (* py 0.091))))))

(defn- protect-center? [wet? x y]
  ;; A narrow, isolated wet tile can lose its center when surrounded by dry cells.
  (and (wet? [x y]) (< (wet-field wet? x y 33 33) 0.89)))

(defn- basin-field [wet? x y u v protect?]
  (let [field (wet-field wet? x y u v)]
    (if protect?
      (max field (smooth (/ (- 30.0 (Math/hypot (- u 32.0) (- v 32.0))) 21.0)))
      field)))

(defn water-at?
  "Test the displayed water footprint on the same two-pixel grid as the renderer."
  [wet? [x y :as cell] u v]
  (and (wet? cell)
       (>= (basin-field wet? x y (inc u) (inc v) (protect-center? wet? x y))
           (threshold (+ (* x tile) u) (+ (* y tile) v)))))

(defn- argb [alpha [r g b] tone]
  (unchecked-int
   (bit-or (bit-shift-left (int alpha) 24)
           (bit-shift-left (int (max 0 (min 255 (+ r tone)))) 16)
           (bit-shift-left (int (max 0 (min 255 (+ g tone)))) 8)
           (int (max 0 (min 255 (+ b tone)))))))

(defn- wall-contact [wet? rock? px py]
  (let [x (- px 22) y (- py 30)]
    (wet-field #(and (rock? %) (not (wet? %)))
               (quot x tile) (quot y tile) (mod x tile) (mod y tile))))

(defn- shore-gravel [px py grain shadow]
  (let [column (quot px 14)
        row (quot py 12)
        shape (mod (bit-xor (* 193 column) (* 47 row)) 43)
        x (- (mod px 14) (+ 4 (mod shape 6)))
        y (- (mod py 12) (+ 3 (mod shape 4)))
        stone? (and (< shape 24) (< (+ (abs x) (* 2 (abs y))) (+ 3 (mod shape 4))))
        palette (cond
                  (and stone? (neg? y)) [144 137 120]
                  stone? [68 72 67]
                  :else [104 102 90])]
    [palette (+ (* 1.4 grain) (* 0.44 shadow))]))

(defn- water-color [depth wall-tone px py grain]
  (let [wave-y (+ py (long (* 3.0 (Math/sin (+ (* px 0.023) (* (quot py 21) 1.7))))))
        band (quot wave-y 21)
        phase (mod (bit-xor (* band 59) (* (quot px 137) 17)) 47)
        glint (and (< (mod wave-y 21) 2)
                   (< (mod (+ px phase) 47) (+ 10 (mod phase 13))))
        shallow? (< depth 0.58)
        palette (cond
                  (< depth 0.17) [133 165 161]
                  (< depth 0.38) [108 151 153]
                  shallow? [88 134 148]
                  :else [74 115 137])]
    [palette (+ (* 0.65 grain) wall-tone (if glint (if shallow? 14 10) 0))]))

(defn- render-fragment [[x y :as cell]]
  (let [wet? landscape/water-blocks
        rock? mountain/blocks
        wet-cell? (wet? cell)
        protect? (protect-center? wet? x y)
        near-wall? (some (fn [p] (and (rock? p) (not (wet? p))))
                         (for [dx [-1 0] dy [-1 0]] [(+ x dx) (+ y dy)]))
        image (BufferedImage. tile tile BufferedImage/TYPE_INT_ARGB)
        cut (when wet-cell? (BufferedImage. tile tile BufferedImage/TYPE_INT_ARGB))]
    (doseq [v (range 0 tile 2) u (range 0 tile 2)
            :let [px (+ (* tile x) u) py (+ (* tile y) v)
                  field (basin-field wet? x y (inc u) (inc v) protect?)
                  cutoff (threshold px py)
                  depth (/ (- field cutoff) (- 1.0 cutoff))
                  water? (and wet-cell? (>= depth 0.0))
                  grain (- (mod (bit-xor (* 97 (quot px 4)) (* 193 (quot py 4))
                                          (* 17 (quot (+ px py) 6))) 13) 6)
                  shadow (if (and near-wall? (< depth 0.75))
                           (int (* -34.0 (wall-contact wet? rock? px py)
                                   (- 1.0 (smooth (/ depth 0.75)))))
                           0)
                  [palette tone] (if water?
                                   (water-color depth shadow px py grain)
                                   (shore-gravel px py grain shadow))
                  strength (blend-strength px)
                  opacity (* strength (if wet-cell? 255 (max 0 (min 105 (* 360 (- field 0.25))))))
                  pixel (argb opacity palette tone)
                  mask (when water? (argb (* 255 strength) [0 0 0] 0))]
            :when (pos? opacity)
            dy [0 1] dx [0 1]]
      (.setRGB image (+ u dx) (+ v dy) pixel)
      (when mask (.setRGB ^BufferedImage cut (+ u dx) (+ v dy) mask)))
    {:image image :cut cut}))

(defn repaint-ground? [cell]
  (and (local-cell? cell)
       (or (landscape/water-blocks cell)
           (not= [1 1 1 1] (landscape/shore-masks cell)))))

(defn affected-cell? [[x y :as cell]]
  (and (local-cell? cell)
       (or (repaint-ground? cell)
           (some landscape/water-blocks [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]))))

(def fragment
  ;; A single field anchors adjacent tiles to the same coastline and water texture.
  (memoize (fn [cell] (when (affected-cell? cell) (render-fragment cell)))))
