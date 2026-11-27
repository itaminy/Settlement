(ns settlement.relief
  "Draw statically baked relief for the main world and preview revisions."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [settlement.config :as config])
  (:import [java.awt Graphics2D RenderingHints]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]))

(def ^:private scenes (delay (edn/read-string (slurp (io/resource "relief_demos.edn")))))
(def ^:private natural-scenes (delay (edn/read-string (slurp (io/resource "relief_natural.edn")))))
(def ^:private art-scenes (delay (edn/read-string (slurp (io/resource "relief_art.edn")))))
(def ^:private integrated-scenes
  (delay (assoc-in (edn/read-string (slurp (io/resource "relief_integrated.edn")))
                   [:canyon :decks] (get-in @art-scenes [:canyon :decks]))))

(defn- art-region? [revision] (#{:art :materials :shapes :walls :finish :integrated} revision))

(def main-world-options
  (when (contains? #{nil "6.4.8" "6.5.2" "7.0"} (System/getProperty "settlement.world.version"))
    {:scene :world :view :lit :revision :integrated}))

(defn- full-region? [revision] (or (= :natural revision) (art-region? revision)))

(def ^:private asset
  (memoize (fn [revision id]
             (let [folder (if (full-region? revision) "relief_v6_4_2" "relief_v6_4_1")
                   file (str "/" (name id) ".png")]
               (ImageIO/read (or (when (= :integrated revision)
                                    (io/resource (str "images/relief_v6_4_8" file)))
                                  (when (#{:finish :integrated} revision)
                                    (io/resource (str "images/relief_v6_4_7" file)))
                                  (when (#{:walls :finish} revision)
                                    (io/resource (str "images/relief_v6_4_6" file)))
                                  (when (#{:shapes :walls :finish} revision)
                                    (io/resource (str "images/relief_v6_4_5" file)))
                                  (when (#{:materials :shapes :walls :finish} revision)
                                    (io/resource (str "images/relief_v6_4_4" file)))
                                  (when (art-region? revision)
                                    (io/resource (str "images/relief_v6_4_3" file)))
                                  (io/resource (str "images/" folder file))))))))

(defn options [scenery] (:terrain-relief scenery))

(defn- scene-data [scenery scene]
  (when-let [{:keys [view revision]} (options scenery)]
    (when-let [{:keys [origin size views tiles front decks]}
               (get @(case revision :integrated integrated-scenes
                            (:art :materials :shapes :walls :finish) art-scenes
                            :natural natural-scenes scenes) scene)]
      (let [id (get views view)
            chunks (get tiles view)]
        (when (or id (seq chunks))
          (let [[x y] (mapv #(* config/tile-size %) origin)
                [w h] (mapv #(* config/tile-size %) size)]
            {:x x :y y :width w :height h :asset id :tiles chunks :scene scene
             :revision revision :front (get front view) :decks decks}))))))

(defn- active-scenes [scenery]
  (case (:scene (options scenery))
    :world [:mountain :canyon]
    nil []
    [(:scene (options scenery))]))

(defn intersects? [{:keys [x y width height]} {u :x v :y w :width h :height}]
  (and (< x (+ u w)) (< u (+ x width)) (< y (+ v h)) (< v (+ y height))))

(defn replaces-rock? [scenery {:keys [kind x width foot-y asset]}]
  (when (and (#{:mountain-face :canyon-face} kind)
             (not (and (full-region? (:revision (options scenery)))
                       (#{:ruins3 :ruins4} asset))))
    (let [scene (if (= :mountain-face kind) :mountain :canyon)]
      (when (some #{scene} (active-scenes scenery))
        (when-let [{u :x v :y w :width h :height} (scene-data scenery scene)]
          (and (< (+ u 24) (+ x (/ width 2)) (- (+ u w) 24))
               (< (+ v 24) foot-y (- (+ v h) 24))))))))

(def ^:private north-margin 304)
(def ^:private east-margin 208)

(defn- edge-opacity ^double [^double distance ^double margin]
  (let [t (min 1.0 (/ distance margin))]
    (* t t (- 3.0 (* 2.0 t)))))

(def ^:private mountain-apron
  (memoize
   (fn [revision id left top right]
     (let [^BufferedImage source (asset revision id)
           width (.getWidth source) height (.getHeight source)
           pixels (.getRGB source 0 0 width height nil 0 width)
           target (BufferedImage. width height BufferedImage/TYPE_INT_ARGB_PRE)
           north (double-array (for [u (range width)
                                    :let [x (+ left (* u 2))]]
                                (+ 238.0 (* 42.0 (Math/sin (/ x 83.0)))
                                   (* 24.0 (Math/sin (/ x 31.0))))))]
       ;; The relief atlas uses one texel per two world pixels. Derive the
       ;; fade from atlas coordinates so it stays continuous across chunks.
       (dotimes [v height]
         (let [y (+ top (* v 2))
               east (+ 154.0 (* 36.0 (Math/sin (/ y 103.0)))
                       (* 18.0 (Math/sin (/ y 37.0))))]
           (dotimes [u width]
             (let [index (+ (* v width) u)
                   pixel (aget pixels index)
                   opacity (* (edge-opacity y (aget north u))
                              (edge-opacity (- right (* u 2)) east))
                   alpha (int (* (unsigned-bit-shift-right (bit-and pixel 0xffffffff) 24)
                                 opacity))]
               (aset-int pixels index (unchecked-int (bit-or (bit-and pixel 0xffffff)
                                                            (bit-shift-left alpha 24))))))))
       (.setRGB target 0 0 width height pixels 0 width)
       target))))

(defn- part-image [revision {:keys [x y width] id :asset} mountain]
  (if mountain
    (let [left (- x (:x mountain))
          top (- y (:y mountain))
          right (- (:width mountain) left)]
      (if (or (< top north-margin) (< (- right width) east-margin))
        (mountain-apron revision id left top right)
        (asset revision id)))
    (asset revision id)))

(defn- draw-part!
  ([graphics revision part] (draw-part! graphics revision part nil))
  ([^Graphics2D graphics revision {:keys [x y width height] :as part} mountain]
   (.drawImage graphics ^BufferedImage (part-image revision part mountain)
               (int x) (int y) (int width) (int height) nil)))

(defn- visible-parts [camera origin parts]
  (for [part parts
        :let [placed (-> part (update :x + (:x origin)) (update :y + (:y origin)))]
        :when (intersects? camera placed)]
    placed))

(defn draw! [^Graphics2D graphics camera scenery]
  (reduce (fn [drawn scene]
            (if-let [{:keys [revision tiles] :as part} (scene-data scenery scene)]
              (if (intersects? camera part)
                (let [mountain (when (and (:world-art scenery) (= revision :integrated)
                                          (= scene :mountain) (seq tiles)) part)]
                  (doseq [chunk (if (seq tiles) (visible-parts camera part tiles) [part])]
                    (draw-part! graphics revision chunk mountain))
                    (conj drawn scene))
                drawn)
              drawn))
          #{} (active-scenes scenery)))

(defn draw-decks! [graphics camera scenery]
  (when (some #{:canyon} (active-scenes scenery))
    (when-let [{:keys [decks revision] :as origin} (scene-data scenery :canyon)]
    (when (seq decks)
      (doseq [deck (visible-parts camera origin decks)]
        (draw-part! graphics revision deck))
      true))))

(def ^:private scaled-front
  (memoize (fn [revision id w h]
             (let [image (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
                   g (.createGraphics image)]
               (try
                 (.setRenderingHint g RenderingHints/KEY_INTERPOLATION
                                    RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
                 (.drawImage g ^BufferedImage (asset revision id) 0 0 (int w) (int h) nil)
                 (finally (.dispose g)))
               image))))

(defn finish-prop [scenery {:keys [kind asset position width height] :as placement}]
  (let [{:keys [revision scene]} (options scenery)]
    (if (= :finish revision)
      (let [[cx cy rx ry] (case scene :mountain [15 70 8 7] :canyon [81 66 8 6]
                               [0 0 1 1])
            [x y] position
            replacement (cond
                          (and (= scene :mountain) (= kind :mountain-camp) (= asset :camp1)) :camp-mountain
                          (and (= scene :canyon) (= kind :canyon-camp) (= asset :camp0)) :camp-canyon
                          (and position (#{:crate :barrel :sign} kind)
                               (< (+ (Math/pow (/ (- x cx) rx) 2)
                                     (Math/pow (/ (- y cy) ry) 2)) 1)) kind)]
        (if replacement
          (assoc placement :image (scaled-front :finish replacement width height))
          placement))
      (if (= :integrated revision)
        (let [[x y] position
              in-region? (and position
                              (or (and (<= 2 x 35) (<= 56 y 93))
                                  (and (<= 73 x 99) (<= 64 y 98))))
              replacement (cond
                            (and asset (#{:mountain-camp :canyon-camp} kind))
                            (keyword (str (name kind) "-" (name asset)))
                            (and in-region? (#{:crate :barrel :sign} kind)) kind)]
          (if replacement
            (assoc placement :image (scaled-front :integrated replacement width height))
            placement))
        placement))))

(defn foregrounds [scenery camera]
  (for [scene (active-scenes scenery)
        :let [{:keys [x y front revision]} (scene-data scenery scene)]
        :when front
        :let [placement (-> front (update :x + x) (update :y + y) (update :foot-y + y))]
        :when (intersects? camera placement)]
    (assoc placement :kind :relief-rock :natural? (boolean (full-region? revision))
           :image (scaled-front revision (:asset front) (:width front) (:height front)))))

(defn foreground [scenery camera]
  (first (foregrounds scenery camera)))
