(ns settlement.forest
  "Opt-in local forest art. Sprite positions and world objects stay authoritative."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [settlement.forest-ground :as ground]
            [settlement.relief :as relief])
  (:import [java.awt AlphaComposite Graphics2D]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]))

(def studies
  (delay (update-vals (edn/read-string (slurp (io/resource "forest_studies.edn")))
                      #(update % :cells set))))

(defn options [scenery]
  (when-let [site (:forest-study scenery)]
    [site (:forest-revision scenery)]))

(def camera-centers {:leaf [13 37] :pine [38 37]})
(def actors {:leaf [13 38] :pine [37 37]})

(def world-options
  (when (contains? #{nil "6.5.2" "7.0"} (System/getProperty "settlement.world.version"))
    {:forest-study :world :forest-revision :polished}))

(def ^:private image
  (memoize (fn [revision asset]
             (let [suffix (str "/" (name asset) ".png")
                   ^BufferedImage source (ImageIO/read
                                         (or (when (= :polished revision)
                                               (io/resource (str "images/forest_v6_5_1" suffix)))
                                             (io/resource (str "images/forest_v6_5" suffix))))
                   target (BufferedImage. (.getWidth source) (.getHeight source)
                                          BufferedImage/TYPE_INT_ARGB_PRE)
                   graphics (.createGraphics target)]
               (try
                 (.setComposite graphics AlphaComposite/Src)
                 (.drawImage graphics source 0 0 nil)
                 (finally (.dispose graphics)))
               target))))

(defn draw-ground! [^Graphics2D g camera scenery]
  (when-let [[site revision] (options scenery)]
    (if (= :world site)
      (ground/draw! g camera (boolean (:world-art scenery)))
      (doseq [{:keys [asset x y width height] :as part} (:chunks (@studies site))
            :when (relief/intersects? camera part)]
        (.drawImage g ^BufferedImage (image revision asset) (int x) (int y) (int width) (int height) nil)))))

(def ^:private scaled-prop
  (memoize (fn [asset width height]
             (let [target (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
                   g (.createGraphics target)]
               (try
                 (.setRenderingHint g java.awt.RenderingHints/KEY_INTERPOLATION
                                    java.awt.RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
                 (.drawImage g ^BufferedImage (image nil asset) 0 0 width height nil)
                 (finally (.dispose g)))
               target))))

(def wind-frame
  (memoize
   (fn [^BufferedImage image wind cut]
     (let [w (.getWidth image)
           h (.getHeight image)
           frame (BufferedImage. (+ w 4) h BufferedImage/TYPE_INT_ARGB_PRE)
           g (.createGraphics frame)
           dx (+ 2 wind)]
       (try
         (.setComposite g AlphaComposite/Src)
         (.drawImage g image dx 0 (+ dx w) cut 0 0 w cut nil)
         (.drawImage g image 2 cut (+ 2 w) h 0 cut w h nil)
         (finally (.dispose g)))
       frame))))

(def ^:private trees #{:tree :tree-tall :tree-small :broadleaf-tree :autumn-tree})
(def ^:private props {:forest-fern :fern :fern :fern :stump :stump :fallen-tree :fallen-tree
                     :bush :bush :berry-bush :berry-bush})

(defn- tree-asset [site revision position]
  (let [variant (mod (hash [site position]) 6)
        variation (mod (hash [position :crown]) 12)]
    (cond
      (not= :polished revision) (str (name site) "-" variant)
      (= site :leaf) (cond
                       (< variation 2) "leaf-young"
                       (< variation 8) (str "leaf-ragged-" (mod variation 3))
                       :else (str "leaf-" variant))
      (< variation 2) "spruce-young"
      (< variation 4) "pine-young"
      :else (str "pine-" variant))))

(defn decorate [scenery {:keys [kind position x width height foot-y] :as layer}]
  (let [[mode revision] (options scenery)
        site (when (and mode position (or (trees kind) (props kind)))
               (if (= :world mode)
                 (ground/crown-site position)
                 (when (contains? (:cells (@studies mode)) position) mode)))]
    (if site
      (cond
        (trees kind)
        (let [asset (tree-asset site revision position)
              ^BufferedImage sprite (image revision asset)
              w (.getWidth sprite)
              h (.getHeight sprite)
              sx (- (+ x (quot width 2) (- (mod (hash position) 21) 10)) (quot w 2))
              fy (+ foot-y (- (mod (hash (reverse position)) 15) 7))
              sy (- fy (quot (* h 122) 128))]
          (assoc layer :image sprite :x sx :y sy :width w :height h :foot-y fy
                 :forest-tree? true :natural? true :ground-shadow? (= :world mode)
                 :forest-shadow {:image (image revision (str asset "-shadow"))
                                 :x (- sx 8) :y (- fy 10)}))

        (props kind)
        (assoc layer :image (scaled-prop (str (name site) "-" (name (props kind))) width height))

        :else layer)
      layer)))
