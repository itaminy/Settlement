(ns settlement.world-terrain
  "Opt-in V7 materials and plant variants over authoritative world coordinates."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [settlement.config :as config]
            [settlement.landscape :as landscape]
            [settlement.layout :as layout]
            [settlement.relief :as relief])
  (:import [java.awt AlphaComposite Graphics2D RenderingHints]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]))

(def ^:private atlas
  (delay
    (edn/read-string (slurp (io/resource "world_terrain.edn")))))

(def ^:private image
  (memoize
   (fn [asset]
     (let [^BufferedImage source (ImageIO/read (io/resource (str "images/world_v7_ground/" asset ".png")))
           target (BufferedImage. (.getWidth source) (.getHeight source) BufferedImage/TYPE_INT_ARGB_PRE)
           ^Graphics2D g (.createGraphics target)]
       (try
         (.setComposite g AlphaComposite/Src)
         (.drawImage g source 0 0 nil)
         (finally (.dispose g)))
       target))))

(defn draw! [^Graphics2D graphics camera scenery]
  (when (and (:world-art scenery) (= config/world-seed (:seed @atlas)))
    (doseq [{:keys [asset x y width height] :as part} (:chunks @atlas)
            :when (relief/intersects? camera part)]
      (.drawImage graphics ^BufferedImage (image (name asset))
                  (int x) (int y) (int width) (int height) nil))))

(def ^:private variants
  #{"palm" "cactus" "flowering-cactus" "dry-bush"
    "tall-grass" "fern" "reeds" "bush" "yellow-flowers"})

(def ^:private scaled
  (memoize
   (fn [asset width height]
     (let [target (BufferedImage. width height BufferedImage/TYPE_INT_ARGB_PRE)
           ^Graphics2D g (.createGraphics target)]
       (try
         (.setComposite g AlphaComposite/Src)
         (.setRenderingHint g RenderingHints/KEY_INTERPOLATION
                            RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
         (.drawImage g ^BufferedImage (image asset) 0 0 width height nil)
         (finally (.dispose g)))
       target))))

(defn decorate [scenery {:keys [kind position width height] :as placement}]
  (if (and (:world-art scenery) position kind (variants (name kind))
           (#{:plains :swamp :desert} (landscape/biome position)))
    (let [index (mod (hash [config/world-seed position kind]) 3)
          asset (str (name kind) "-" index)]
      (assoc placement :image (scaled asset width height)))
    placement))

(def ^:private pads
  (delay
    (vec
     (for [[x y :as p] (landscape/cells)
           :when (and (= :swamp (landscape/biome p))
                      (= [0 0 0 0] (landscape/shore-masks p))
                      (landscape/water-blocks p)
                      (not (layout/road-cells p))
                      (< (mod (hash [config/world-seed p :lily]) 12) 5))
           :let [index (mod (hash p) 3)]]
       {:x (+ (* x config/tile-size) (mod (hash [p :x]) 12))
        :y (+ (* y config/tile-size) (mod (hash [p :y]) 18))
        :width 42 :height 34 :image (image (str "lily-" index))}))))

(defn draw-water-plants! [^Graphics2D graphics camera scenery]
  (when (:world-art scenery)
    (doseq [{:keys [image x y] :as pad} @pads :when (relief/intersects? camera pad)]
      (.drawImage graphics ^BufferedImage image (int x) (int y) nil))))
