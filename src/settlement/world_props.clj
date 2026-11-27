(ns settlement.world-props
  "World V7 visual material registry. The source placement stays authoritative."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [settlement.relief :as relief])
  (:import [java.awt AlphaComposite Graphics2D RenderingHints]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]))

(def ^:private root "images/world_v7_props/")

(defn asset-path [asset] (str root (name asset) ".png"))

(def ^:private sprite
  (memoize
   (fn [asset width height]
     (let [^BufferedImage source (ImageIO/read (io/resource (asset-path asset)))]
       (if (= [width height] [(.getWidth source) (.getHeight source)])
         source
         (let [target (BufferedImage. width height BufferedImage/TYPE_INT_ARGB_PRE)
               ^Graphics2D g (.createGraphics target)]
           (try
             (.setComposite g AlphaComposite/Src)
             (.setRenderingHint g RenderingHints/KEY_INTERPOLATION
                                RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
             (.drawImage g source 0 0 width height nil)
             (finally (.dispose g)))
           target))))))

(def ^:private fence-detail
  (memoize
   (fn [^BufferedImage source]
     ;; Each source image already encodes its neighbours. Preserve that exact shape.
     (let [w (.getWidth source) h (.getHeight source)
           image (BufferedImage. w h BufferedImage/TYPE_INT_ARGB_PRE)
           ^ints pixels (.getRGB source 0 0 w h nil 0 w)]
       (dotimes [v h]
         (dotimes [u w]
           (let [i (+ u (* v w))
                 argb (aget pixels i)
                 r (bit-and 255 (unsigned-bit-shift-right argb 16))
                 g (bit-and 255 (unsigned-bit-shift-right argb 8))
                 b (bit-and 255 argb)]
             (when (and (not (zero? (unsigned-bit-shift-right argb 24))) (> r 60) (> r b))
               (let [grain (case (mod (+ (quot u 4) (quot v 2)) 7) 0 7 3 -8 0)
                     tone (+ grain (- 7 (quot (+ u v) 10)))
                     c #(max 0 (min 255 (+ % tone)))]
                 (aset-int pixels i (unchecked-int
                                      (bit-or (bit-and argb 0xff000000)
                                              (bit-shift-left (c r) 16)
                                              (bit-shift-left (c g) 8)
                                              (c b)))))))))
       (.setRGB image 0 0 w h pixels 0 w)
       image))))

(def ^:private materials
  #{:house :sawmill :watchtower :mine :gate :wooden-fence :tent :sign :well
    :shore-rocks :campfire :crate :barrel :chest :log-bundle :stone-pile :stump
    :fallen-tree :stone :stone-alt :large-boulder :mossy-rock :gold-ore :iron-ore
    :crystals})

(def ^:private variants
  {:raider-tent 4 :cairn 4 :abandoned-camp 5 :relict-well 4 :ember-pit 4 :lantern 4})

(defn- material-asset [objects {:keys [kind asset position]}]
  (cond
    (= kind :built-house) (case (:team (get objects position))
                            :blue :blue-house :red :red-house :house)
    (and asset (#{:mountain-camp :canyon-camp} kind))
    (keyword (str (name kind) "-" (name asset)))
    (contains? materials kind) kind
    (and position (contains? variants kind))
    (keyword (str (name kind) "-" (mod (hash position) (variants kind))))))

(defn decorate [scenery objects {:keys [kind image width height] :as placement}]
  (if (and (:world-art scenery) image)
    (if (= kind :fence)
      (assoc placement :image (fence-detail image))
      (if-let [asset (material-asset objects placement)]
        (assoc placement :image (sprite asset width height))
        placement))
    placement))

(def ^:private stairs
  (delay (edn/read-string (slurp (io/resource (str root "stairs.edn"))))))

(defn draw-stairs! [^Graphics2D graphics camera scenery]
  (when (:world-art scenery)
    (doseq [{:keys [asset x y width height] :as stair} @stairs
            :when (relief/intersects? camera stair)]
      (.drawImage graphics ^BufferedImage (sprite asset width height) (int x) (int y) nil))))
