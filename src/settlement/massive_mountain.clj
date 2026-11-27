(ns settlement.massive-mountain
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def version (System/getProperty "settlement.world.version"))
(when (#{"6.2" "6.3"} version)
  (throw (ex-info "Archived world version. Use backup/world-v7.0-full." {:version version})))

(def art-version (case version
                   (nil "6.3.1" "6.3.2" "6.4" "6.4.8" "6.5.2" "7.0") "6_3_1"
                   nil))
(def redesigned? (some? art-version))
(def image-root (str "images/visual_v" (or art-version "6_1") "/"))
(def art (when redesigned?
           (edn/read-string (slurp (io/resource (str "massive_mountain_art_v" art-version ".edn"))))))

(def enabled? (not (#{"4.1" "5" "6"} version)))
(def model (when enabled? (edn/read-string (slurp (io/resource "massive_mountain.edn")))))
(def trail (:trail model))
(def route-cells (set trail))
(def platforms (set (:platforms model)))
(def blocks (set (:blocks model)))
(def entrance (first trail))
(def summit (peek trail))
(def camps (:camps model))
(def props (:props model))
