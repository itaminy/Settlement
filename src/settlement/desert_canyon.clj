(ns settlement.desert-canyon
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [settlement.massive-mountain :as mountain]))

(def enabled? (contains? #{nil "6.4" "6.4.8" "6.5.2" "7.0"} mountain/version))
(def model (when enabled? (edn/read-string (slurp (io/resource "desert_canyon.edn")))))
(def trail (:trail model))
(def routes (into (set trail) cat (:branches model)))
(def bridges (set (:bridges model)))
(def walls (set (:walls model)))
(def pits (set (:pits model)))
(def blocks (into walls pits))
(def platforms (set (:platforms model)))
(def props (into {} (:props model)))
(def image-root "images/visual_v6_4/")
(def art (delay (when enabled?
                  (edn/read-string (slurp (io/resource "desert_canyon_art.edn"))))))

(defn within? [[x y]]
  (and enabled? (<= 73 x 98) (<= 64 y 98)))

(defn near-bridge? [[x y]]
  (some bridges [[x y] [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]))

(defn oasis-margin? [[x y]]
  (or (< (+ (Math/pow (/ (- x 90) 5.8) 2) (Math/pow (/ (- y 79) 4.5) 2)) 1)
      (< (+ (Math/pow (/ (- x 75) 6.1) 2) (Math/pow (/ (- y 89) 5.0) 2)) 1)))
