(ns settlement.water-study
  "Opt-in water art. All positions and collisions come from the existing world."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [settlement.config :as config]
            [settlement.relief :as relief])
  (:import [java.awt AlphaComposite Color Graphics2D]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]))

(def ^:private atlas
  (delay (edn/read-string (slurp (io/resource "water_studies.edn")))))
(def ^:private polished-atlas
  (delay (edn/read-string (slurp (io/resource "water_studies_661.edn")))))
(def ^:private natural-atlas
  (delay (edn/read-string (slurp (io/resource "water_studies_662.edn")))))
(defn- crosses-deck? [[x y dx dy _ length] {:keys [width height] left :x top :y}]
  (and (< (+ x (min dx 0)) (+ left width))
       (> (+ x (max dx 0) (abs length)) left)
       (< (+ y (min dy 0)) (+ top height))
       (> (+ y (max dy 0) 2) (- top 20))))

(def ^:private world-atlas
  (delay
    (let [{:keys [decks] :as model} (edn/read-string (slurp (io/resource "water_world.edn")))]
      (-> model
          (update :bridge-cells set)
          (update :chunks (fn [chunks]
                            (mapv #(update % :motion
                                           (fn [motion] (filterv (fn [dash] (not-any? (partial crosses-deck? dash) decks))
                                                                 motion)))
                                  chunks)))))))

(defn options [scenery] (:water-study scenery))
(defn revision [scenery] (:water-revision scenery))
(defn natural? [scenery] (= :natural (revision scenery)))
(defn global? [scenery] (= :world (options scenery)))

(defn- atlas-for-world []
  (when (= config/world-seed (:seed @world-atlas)) @world-atlas))

(defn global-active? [scenery]
  (boolean (and (global? scenery) (atlas-for-world))))

(defn scene [scenery]
  (when-let [site (options scenery)]
    (if (global? scenery)
      (atlas-for-world)
      (get @(case (revision scenery) :natural natural-atlas :polished polished-atlas atlas) site))))

(def ^:private image
  (memoize
   (fn [revision asset]
     (let [file (str "/" (name asset) ".png")
           ^BufferedImage source (ImageIO/read
                                  (or (when (= :world revision)
                                        (io/resource (str "images/water_v7" file)))
                                      (when (#{:natural :world} revision)
                                        (io/resource (str "images/water_v6_6_2" file)))
                                      (io/resource (str (if revision "images/water_v6_6_1" "images/water_v6_6")
                                                        file))))
           scale (if (and (= :world revision) (.startsWith ^String file "/world-")) 1 2)
           target (BufferedImage. (* scale (.getWidth source)) (* scale (.getHeight source)) BufferedImage/TYPE_INT_ARGB_PRE)
           g (.createGraphics target)]
       (try
         (.setComposite g AlphaComposite/Src)
         (.setRenderingHint g java.awt.RenderingHints/KEY_INTERPOLATION
                            java.awt.RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
         (.drawImage g source 0 0 (.getWidth target) (.getHeight target) nil)
         (finally (.dispose g)))
       target))))

(defn- visible-chunks [model camera]
  (filter #(relief/intersects? camera %) (:chunks model)))

(defn restore-cells [scenery camera]
  (when-let [model (scene scenery)]
    (if (global? scenery)
      (distinct (concat (mapcat :restore (visible-chunks model camera)) (:bridge-cells model)))
      (when (relief/intersects? camera (:bounds model)) (:restore model)))))

(defn bridge-banks [scenery]
  (when (global? scenery) (:bridge-cells (scene scenery))))

(defn draw! [^Graphics2D g camera scenery]
  (when-let [{:keys [parts structure decks] :as model} (scene scenery)]
    (doseq [{:keys [asset x y width height] :as part} (if (global? scenery)
                                         (concat (visible-chunks model camera) decks
                                                 [(get-in @natural-atlas [:fall :structure])])
                                         (cond-> (vec parts) structure (conj structure)))
            :when (relief/intersects? camera part)]
      (.drawImage g ^BufferedImage (image (revision scenery) asset)
                  (int x) (int y) (int width) (int height) nil))))

(defn foreground [scenery camera]
  (when-let [{:keys [asset] :as rail} (:foreground (scene scenery))]
    (when (relief/intersects? camera rail)
      (assoc rail :kind :water-rail :image (image (revision scenery) asset)))))

(defn foregrounds [scenery camera]
  (if (global? scenery)
    (for [{:keys [asset] :as rail} (:rails (scene scenery))
          :when (relief/intersects? camera rail)]
      (assoc rail :kind :water-rail :image (image :world asset)))
    (when-let [rail (foreground scenery camera)] [rail])))

(defn replaces-motion? [scenery part]
  (when-let [model (scene scenery)]
    (if (global? scenery)
      (nil? (:flows part))
      (relief/intersects? (:bounds model) part))))

(def ^:private ripple-colors
  (mapv #(Color. 167 210 200 (int %)) [11 18 28 39 48]))
(def ^:private spray (Color. 196 218 205 138))
(def ^:private falling (Color. 171 210 204 105))
(def ^:private reflected (Color. 40 72 60 63))

(defn- draw-outlet! [^Graphics2D g {:keys [flow pool broken]} phase]
  (let [[x y w h] flow [px py] pool]
    (.setColor g falling)
    (dotimes [i 20]
      (let [t (mod (+ (* i 17) (* phase 6)) (max 1 (- h 4)))]
        (.fillRect g (int (+ x (mod (+ (* i 13) (- t)) w))) (int (+ y t)) 2 (+ 3 (mod i 5)))))
    (.setColor g spray)
    (dotimes [i 22]
      (let [t (mod (+ (* i 11) (* phase 3)) 26)]
        (.fillRect g (int (+ px (- (mod (+ (* i 19) t) 100) 50)))
                   (int (+ py (- (quot (* (- t 12) (- t 12)) 12) 10)))
                   (+ 2 (mod i 4)) 2)))
    (dotimes [i 3]
      (let [r (+ 9 (mod (+ (* i 14) phase) 42))]
        (.setColor g (ripple-colors (max 0 (- 4 (quot r 11)))))
        (if broken
          (do (.drawLine g (int (- px r)) (int (+ py (quot r 4)))
                         (int (- px (quot r 3))) (int (+ py (quot r 3))))
              (.drawLine g (int (+ px (quot r 5))) (int (+ py (quot r 3) (mod i 3)))
                         (int (+ px (quot r 2))) (int (+ py (quot r 4)))))
          (.drawArc g (int (- px r)) (int (- py 4)) (int (* r 2))
                    (int (quot r 2)) 195 151))))))

(defn- draw-local-motion! [^Graphics2D g camera scenery phase]
  (when-let [{:keys [bounds motion structure]} (scene scenery)]
    (when (relief/intersects? camera bounds)
      (if (= :fall (options scenery))
        (if (:flow structure)
          (draw-outlet! g structure phase)
          (let [{:keys [x y]} structure]
          (.setColor g falling)
          (dotimes [i 17]
            (let [shift (mod (+ (* i 23) (* phase 5)) 46)
                  u (+ x 30 (mod (* i 13) 58))
                  v (+ y 35 shift)]
              (.fillRect g (int u) (int v) 2 (+ 4 (mod i 7)))))
          (.setColor g spray)
          (dotimes [i 23]
            (let [t (mod (+ (* i 13) (* phase 3)) 27)
                  u (+ x 8 (mod (+ (* i 31) t) 112))
                  v (+ y 105 (- (quot (* (- t 12) (- t 12)) 14) 9))]
              (.fillRect g (int u) (int v) (+ 2 (mod i 4)) 2)))))
        (let [{cx :x cy :y w :width h :height} camera
              right (+ cx w) bottom (+ cy h)]
          (doseq [[x y dx dy offset length] motion
                  :when (and (<= (- cx 38) x right) (<= (- cy 24) y bottom)
                             (or (nil? structure)
                                 (not (<= (- (:y structure) 25) y
                                          (+ (:y structure) (:height structure))))))]
            (let [frame (mod (+ phase offset) 24)
                  intensity (min 4 (quot (min frame (- 23 frame)) 2))
                  px (+ x (quot (* dx frame) 23))
                  py (+ y (quot (* dy frame) 23))
                  bended? (and (natural? scenery) (= :river (options scenery)))]
              (if (neg? length)
                (do (.setColor g reflected)
                    (.fillRect g (int px) (int py) (int (- length)) 2))
                (do
                  (.setColor g (ripple-colors intensity))
                  (if bended?
                    (.drawLine g (int px) (int py) (int (+ px (quot length 2) (quot dx 3)))
                               (int (+ py 3)))
                    (.fillRect g (int px) (int py) (int length) 2))
                  (.fillRect g (int (+ px 3)) (int (+ py 2)) (int (max 2 (- length 6))) 1))))))))))

(defn draw-motion! [^Graphics2D g camera scenery phase]
  (if (global? scenery)
    (when-let [model (scene scenery)]
      (let [{left :x top :y :keys [width height]} camera
            right (+ left width) bottom (+ top height)
            fall (get-in @natural-atlas [:fall :structure])]
        (doseq [part (visible-chunks model camera)
                [x y dx dy offset length] (:motion part)
                :when (and (<= (- left 32) x (+ right 16))
                           (<= (- top 20) y (+ bottom 2)))]
          (let [t (mod (+ phase offset) 24)
                px (+ x (quot (* dx t) 23))
                py (+ y (quot (* dy t) 23))]
            (.setColor g (if (neg? length) reflected (ripple-colors (min 4 (quot (min t (- 23 t)) 2)))))
            (.drawLine g (int px) (int py) (int (+ px (abs length)))
                       (int (+ py (if (> dy 5) 2 0))))))
        (when (relief/intersects? camera fall)
          (draw-outlet! g fall phase))))
    (draw-local-motion! g camera scenery phase)))
