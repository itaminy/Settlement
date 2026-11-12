(ns settlement.rendering
  (:require [clojure.java.io :as io])
  (:import [javax.imageio ImageIO]
           [java.awt Color Font RenderingHints]
           [java.awt.image BufferedImage]))

(def map-size 12)
(def tile-size 64)
;; Поля также покрыты травой: кроны и крыши крайних клеток остаются целыми.
(def padding 64)
(def canvas-size (+ (* map-size tile-size) (* 2 padding)))

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

;; Только оформление клиента: эти элементы не входят в состояние мира.
(def decorations
  {[0 2] :bush [11 1] :bush [2 3] :mushroom-red
   [6 4] :stump [10 4] :mushroom-green [9 7] :logs
   [8 7] :bench [2 8] :bush [10 9] :mushroom-red
   [0 10] :mushroom-green [6 10] :logs [5 11] :stump [10 11] :bench})

(defn road? [x y]
  (and (<= 0 x 11) (<= 0 y 11)
       (or (= y 6) (and (= x 4) (<= 3 y 8)))))

(def background
  (delay
    (let [grass (scale-image (load-image "resources/images/grass.png") tile-size tile-size)
          dirt (scale-image (load-image "resources/images/dirt.png") tile-size tile-size)
          result (BufferedImage. canvas-size canvas-size BufferedImage/TYPE_INT_ARGB)
          graphics (.createGraphics result)]
      (try
        (doseq [y (range -1 (inc map-size)) x (range -1 (inc map-size))]
          (let [px (+ padding (* x tile-size)) py (+ padding (* y tile-size))
                road (road? x y)]
            (.drawImage graphics (if road dirt grass) px py nil)
            (when-not road
              ;; Детерминированные оттенки поверх исходной пиксельной травы.
              (.setColor graphics (case (mod (+ (* x 7) (* y 11)) 3)
                                    0 (Color. 42 78 23 18)
                                    1 (Color. 183 199 100 14)
                                    (Color. 0 0 0 0)))
              (.fillRect graphics px py tile-size tile-size))))
        (finally (.dispose graphics)))
      result)))

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
        foot-x (+ padding (* x tile-size) (quot tile-size 2))
        foot-y (+ padding (* y tile-size) (- tile-size 8))]
    {:kind kind :image image :x (- foot-x (quot width 2)) :y (- foot-y height)
     :width width :height height :foot-y foot-y}))

(defn scene-sprites [objects]
  (->> (concat (for [[position kind] decorations :when (not (contains? objects position))]
                 (sprite-placement position kind))
               (for [[position object] objects]
                 (sprite-placement position (sprite-kind position object))))
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
           :x (+ padding (* x tile-size) (quot tile-size 2)
                 (int (* 34 (- index (/ (dec count-players) 2.0)))))
           :y (+ padding (* y tile-size) 37)
           :color (nth player-colors (mod id (count player-colors)))})
        occupants)))
   (sort-by key (group-by (fn [[_ player]] [(:x player) (:y player)]) players))))

(defn render-scene [players objects]
  (let [result (BufferedImage. canvas-size canvas-size BufferedImage/TYPE_INT_ARGB)
        graphics (.createGraphics result)]
    (try
      (.setRenderingHint graphics RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
      (.drawImage graphics ^BufferedImage @background 0 0 nil)
      (doseq [{:keys [image x y]} (scene-sprites objects)]
        (.drawImage graphics ^BufferedImage image (int x) (int y) nil))
      ;; Маркеры рисуются поверх крон и крыш, чтобы игрок не терялся за ними.
      (.setFont graphics (Font. "Arial" Font/BOLD 14))
      (doseq [{:keys [id x y color]} (player-markers players)]
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
