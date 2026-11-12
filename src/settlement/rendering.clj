(ns settlement.rendering
  (:require [clojure.java.io :as io]
            [settlement.config :as config]
            [settlement.layout :as layout])
  (:import [javax.imageio ImageIO]
           [java.awt Color Font RenderingHints]
           [java.awt.image BufferedImage]))

(def map-size config/world-width)
(def tile-size config/tile-size)
(def world-pixel-width (* config/world-width tile-size))
(def world-pixel-height (* config/world-height tile-size))

(defn camera [player width height]
  (let [width (max 1 (min width world-pixel-width))
        height (max 1 (min height world-pixel-height))
        center-x (+ (* (:x player 0) tile-size) (quot tile-size 2))
        center-y (+ (* (:y player 0) tile-size) 37)
        clamp (fn [value maximum] (int (max 0 (min maximum value))))]
    {:x (clamp (- center-x (quot width 2)) (- world-pixel-width width))
     :y (clamp (- center-y (quot height 2)) (- world-pixel-height height))
     :width width :height height}))

(defn visible-cells [{:keys [x y width height]} margin]
  (for [cy (range (max 0 (- (quot y tile-size) margin))
                  (min config/world-height (+ 1 margin (quot (+ y height -1) tile-size))))
        cx (range (max 0 (- (quot x tile-size) margin))
                  (min config/world-width (+ 1 margin (quot (+ x width -1) tile-size))))]
    [cx cy]))

(defn intersects? [camera {:keys [x y width height]}]
  (and (< x (+ (:x camera) (:width camera)))
       (< y (+ (:y camera) (:height camera)))
       (> (+ x width) (:x camera)) (> (+ y height) (:y camera))))

(def sprite-files
  {:tree "pine_tree.png" :tree-alt "pine_tree_alt.png"
   :stone "rock_1.png" :stone-alt "rock_2.png" :house "house.png"
   :bush "bush.png" :flowers "flowers.png" :mushroom-red "mushroom_red.png"
   :mushroom-green "mushroom_green.png" :logs "logs.png" :stump "stump.png" :bench "bench.png"})

(defn load-image [path]
  (or (ImageIO/read (io/file path))
      (throw (ex-info "Cannot load image" {:path path}))))

(defn scale-image [^BufferedImage source max-width max-height]
  (let [scale (double (min (/ max-width (.getWidth source)) (/ max-height (.getHeight source))))
        width (max 1 (int (Math/round (* scale (.getWidth source)))))
        height (max 1 (int (Math/round (* scale (.getHeight source)))))
        result (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphics (.createGraphics result)]
    (try
      (.setRenderingHint graphics RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
      (.drawImage graphics source 0 0 width height nil)
      (finally (.dispose graphics)))
    result))

(def sprite-sizes
  {:tree [100 112] :tree-alt [100 112] :stone [54 36] :stone-alt [54 36]
   :house [144 128] :bush [36 28] :flowers [38 28]
   :mushroom-red [22 22] :mushroom-green [22 22]
   :logs [30 52] :stump [38 24] :bench [62 30]})

(def sprites
  (into {} (for [[kind filename] sprite-files
                 :let [[width height] (sprite-sizes kind)]]
             [kind (scale-image (load-image (str "resources/images/new/" filename)) width height)])))

(def decorations layout/decorations)
(def road? layout/road?)

(def terrain-tiles
  (delay
    (let [grass (scale-image (load-image "resources/images/grass.png") tile-size tile-size)
          dirt (scale-image (load-image "resources/images/dirt.png") tile-size tile-size)]
      (assoc
       (into {} (for [[index shade] [[0 (Color. 42 78 23 18)]
                                     [1 (Color. 183 199 100 14)]
                                     [2 (Color. 0 0 0 0)]]]
                  (let [image (BufferedImage. tile-size tile-size BufferedImage/TYPE_INT_ARGB)
                        g (.createGraphics image)]
                    (try (.drawImage g grass 0 0 nil)
                         (.setColor g shade)
                         (.fillRect g 0 0 tile-size tile-size)
                         (finally (.dispose g)))
                    [index image])))
       :dirt dirt))))

(defn sprite-kind [[x y] object]
  (let [alternate? (odd? (+ x y))]
    (case object
      ;; Временно используем одну сосну: исходный alt PNG — чёрный силуэт.
      :tree :tree
      :stone (if alternate? :stone-alt :stone)
      object)))

(defn sprite-placement [[x y] kind]
  (let [image ^BufferedImage (sprites kind)
        width (.getWidth image) height (.getHeight image)
        foot-x (+ (* x tile-size) (quot tile-size 2))
        foot-y (+ (* y tile-size) (- tile-size 8))]
    {:position [x y] :kind kind :image image :x (max 0 (min (- world-pixel-width width) (- foot-x (quot width 2))))
     :y (max 0 (min (- world-pixel-height height) (- foot-y height)))
     :width width :height height :foot-y foot-y}))

(defn scene-sprites [objects camera]
  (->> (for [position (visible-cells camera 2)
             :let [object (get objects position)
                   kind (if object (sprite-kind position object) (get decorations position))]
             :when kind
             :let [placement (sprite-placement position kind)]
             :when (intersects? camera placement)]
         placement)
       (sort-by (juxt :foot-y :x :kind))))

(def player-colors [(Color. 52 151 234) (Color. 242 155 45)
                    (Color. 171 100 220) (Color. 224 91 111)])

(defn player-markers [players]
  (mapcat
   (fn [[[x y] occupants]]
     (let [occupants (sort-by key occupants)
           count-players (count occupants)]
       (map-indexed
        (fn [index [id _]]
          {:id id
           :x (+ (* x tile-size) (quot tile-size 2)
                 (int (* 34 (- index (/ (dec count-players) 2.0)))))
           :y (+ (* y tile-size) 37)
           :color (nth player-colors (mod id (count player-colors)))})
        occupants)))
   (sort-by key (group-by (fn [[_ player]] [(:x player) (:y player)]) players))))

(defn render-scene [players objects camera]
  (let [result (BufferedImage. (:width camera) (:height camera) BufferedImage/TYPE_INT_ARGB)
        graphics (.createGraphics result)]
    (try
      (.setRenderingHint graphics RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
      (.translate graphics (- (:x camera)) (- (:y camera)))
      (doseq [[x y] (visible-cells camera 0)]
        (.drawImage graphics ^BufferedImage
                    (get @terrain-tiles (if (road? x y) :dirt (mod (+ (* x 7) (* y 11)) 3)))
                    (* x tile-size) (* y tile-size) nil))
      (doseq [{:keys [image x y]} (scene-sprites objects camera)]
        (.drawImage graphics ^BufferedImage image (int x) (int y) nil))
      ;; Маркеры рисуются поверх крон и крыш, чтобы игрок не терялся за ними.
      (.setFont graphics (Font. "Arial" Font/BOLD 14))
      (doseq [{:keys [id x y color]} (player-markers players)
              :when (intersects? camera {:x (- x 18) :y (- y 18) :width 36 :height 36})]
        (.setColor graphics (Color. 27 37 30))
        (.fillOval graphics (- x 18) (- y 18) 36 36)
        (.setColor graphics Color/WHITE)
        (.fillOval graphics (- x 16) (- y 16) 32 32)
        (.setColor graphics color)
        (.fillOval graphics (- x 14) (- y 14) 28 28)
        (.setColor graphics Color/WHITE)
        (let [label (str id) metrics (.getFontMetrics graphics)]
          (.drawString graphics label (int (- x (quot (.stringWidth metrics label) 2))) (int (+ y 5)))))
      (finally (.dispose graphics)))
    result))
