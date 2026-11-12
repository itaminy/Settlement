(ns settlement.config)

(def world-width 50)
(def world-height 50)
(def tile-size 64)
(def viewport-width 896)
(def viewport-height 768)

(defn in-bounds? [[x y]]
  (and (<= 0 x) (< x world-width) (<= 0 y) (< y world-height)))
