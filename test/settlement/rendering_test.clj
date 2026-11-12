(ns settlement.rendering-test
  (:require [clojure.test :refer :all]
            [settlement.gui :as gui]
            [settlement.rendering :as rendering]
            [settlement.world :as world]
            [settlement.config :as config]
            [settlement.layout :as layout]
            [settlement.test-support :as support])
  (:import [java.awt Color]
           [java.awt.image BufferedImage]
           [java.util Arrays]))

(defn- pixels [^BufferedImage image]
  (.getRGB image 0 0 (.getWidth image) (.getHeight image) nil 0 (.getWidth image)))

(deftest png-loading-and-transparency-test
  (is (= 12 (count rendering/sprite-files)))
  (doseq [[kind filename] rendering/sprite-files]
    (testing filename
      (let [source (rendering/load-image (str "resources/images/new/" filename))
            scaled ^BufferedImage (rendering/sprites kind)
            source-alpha (map #(unsigned-bit-shift-right % 24) (pixels source))
            scaled-alpha (map #(unsigned-bit-shift-right % 24) (pixels scaled))]
        (is (.hasAlpha (.getColorModel source)))
        (is (some zero? source-alpha))
        (is (some pos? source-alpha))
        (is (some zero? scaled-alpha))
        (is (some pos? scaled-alpha))
        ;; Округление размеров допускает погрешность до одного пикселя.
        (is (<= (Math/abs (- (double (/ (.getWidth scaled) (.getHeight scaled)))
                             (double (/ (.getWidth source) (.getHeight source)))))
                (/ 1.0 (.getHeight scaled))))))))

(deftest nearest-neighbor-scaling-test
  (let [source (BufferedImage. 2 1 BufferedImage/TYPE_INT_ARGB)]
    (.setRGB source 0 0 (.getRGB Color/RED))
    (.setRGB source 1 0 0)
    (let [scaled (rendering/scale-image source 6 6)]
      (is (= [6 3] [(.getWidth scaled) (.getHeight scaled)]))
      (is (= #{(.getRGB Color/RED) 0} (set (pixels scaled))))
      (is (= (.getRGB Color/RED) (.getRGB scaled 2 1)))
      (is (= 0 (.getRGB scaled 3 1))))))

(deftest complete-sprites-at-map-edges-test
  (doseq [kind (keys rendering/sprites)]
    (is (every? (fn [[x y]]
                  (let [{:keys [x y width height]} (rendering/sprite-placement [x y] kind)]
                    (and (<= 0 x) (<= 0 y)
                         (<= (+ x width) rendering/world-pixel-width)
                         (<= (+ y height) rendering/world-pixel-height))))
                (for [x (range config/world-width) y (range config/world-height)] [x y]))
        (str "Full sprite fits for " kind))))

(deftest layering-and-decorations-test
  (let [objects world/initial-objects
        layers (rendering/scene-sprites objects {:x 0 :y 0 :width rendering/world-pixel-width :height rendering/world-pixel-height})]
    (is (= layers (sort-by (juxt :foot-y :x :kind) layers)))
    (is (= (+ (count objects) (count rendering/decorations)) (count layers)))
    (is (every? #(not (contains? objects %)) (keys rendering/decorations)))
    (is (every? (fn [[x y]] (not (rendering/road? x y))) (keys rendering/decorations)))
    (is (not (contains? rendering/decorations [0 0]))))
  ;; Непрозрачные тестовые спрайты позволяют проверить фактическое наложение.
  (let [red (BufferedImage. 64 128 BufferedImage/TYPE_INT_ARGB)
        blue (BufferedImage. 64 128 BufferedImage/TYPE_INT_ARGB)]
    (doseq [[image color] [[red Color/RED] [blue Color/BLUE]]]
      (let [g (.createGraphics image)]
        (try (.setColor g color) (.fillRect g 0 0 64 128)
             (finally (.dispose g)))))
    (with-redefs [rendering/decorations {}
                  rendering/sprites {:house red :stone blue :stone-alt blue}]
      (let [image (rendering/render-scene {} {[4 4] :house [4 5] :stone} (rendering/camera nil 600 600))]
        (is (= (.getRGB Color/BLUE) (.getRGB image 280 280)))))))

(deftest players-visible-over-objects-test
  (let [players {1 {:x 0 :y 0} 2 {:x 0 :y 0}}
        markers (rendering/player-markers players)
        image (rendering/render-scene players {[0 0] :house} (rendering/camera nil 600 600))]
    (is (= #{1 2} (set (map :id markers))))
    (is (= 2 (count (set (map (juxt :x :y) markers)))))
    (doseq [{:keys [x y color]} markers]
      (is (= (.getRGB ^Color color) (.getRGB image x (+ y 10)))))))

(deftest map-panel-and-disappearing-object-test
  (javax.swing.SwingUtilities/invokeAndWait
   (fn []
     (let [panel (gui/create-map-panel 0 0 1)
           players {1 {:x 5 :y 5}}
           objects {[5 5] :house}
           target (BufferedImage. 1000 700 BufferedImage/TYPE_INT_ARGB)
           graphics (.createGraphics target)]
       (try
         (is (nil? (.getBorder panel)))
         (is (zero? (count (.getComponents panel))))
         (is (> (.width (.getPreferredSize panel)) 540))
         (gui/update-map panel players objects)
         (.setSize panel 1000 700)
         (.paint panel graphics)
         (let [expected (rendering/render-scene players objects (rendering/camera (players 1) 1000 700))
               actual (pixels target)]
           (is (Arrays/equals ^ints (pixels expected) ^ints actual)))
         (let [before (pixels (.getClientProperty panel :settlement.gui/scene))]
           (gui/update-map panel players {})
           (.paint panel graphics)
           (is (not (Arrays/equals ^ints before
                                   ^ints (pixels (.getClientProperty panel :settlement.gui/scene))))))
         (is (= {[5 5] :house} objects))
         (finally (.dispose graphics)))))))

(deftest terrain-and-stable-variants-test
  (is (every? #(apply rendering/road? %) layout/road-cells))
  (is (apply rendering/road? layout/spawn-position))
  (is (= #{:tree :tree-small :tree-tall}
         (set (for [[position object] world/initial-objects :when (= :tree object)]
                (rendering/sprite-kind position object)))))
  (is (= #{:stone :stone-alt}
         (set (for [[position object] world/initial-objects :when (= :stone object)]
                (rendering/sprite-kind position object))))))

(deftest temporary-silhouette-exclusion-test
  (is (not-any? #{:flowers} (vals rendering/decorations))))

(deftest camera-follows-local-player-test
  (let [player {:x 25 :y 20}
        camera (rendering/camera player 800 600)]
    (is (= 400 (- (+ (* 25 config/tile-size) 32) (:x camera))))
    (is (= 300 (- (+ (* 20 config/tile-size) 37) (:y camera))))
    (is (= [800 600] [(:width camera) (:height camera)]))
    (is (not= (:x camera) (:x (rendering/camera (update player :x inc) 800 600))))))

(deftest camera-stays-inside-world-test
  (is (= {:x 0 :y 0 :width 800 :height 600}
         (rendering/camera {:x 0 :y 0} 800 600)))
  (is (= {:x (- rendering/world-pixel-width 800)
          :y (- rendering/world-pixel-height 600) :width 800 :height 600}
         (rendering/camera {:x (dec config/world-width) :y (dec config/world-height)} 800 600)))
  (doseq [player [{:x 0 :y 49} {:x 49 :y 0} {:x 25 :y 25}]
          [width height] [[1 1] [641 479] [1200 600] [5000 5000]]]
    (let [camera (rendering/camera player width height)]
      (is (<= 0 (:x camera) (- rendering/world-pixel-width (:width camera))))
      (is (<= 0 (:y camera) (- rendering/world-pixel-height (:height camera)))))))

(deftest viewport-culling-and-sprite-overhang-test
  (with-redefs [rendering/decorations {}]
    (let [camera {:x 640 :y 640 :width 256 :height 256}
          objects {[11 11] :stone [11 14] :tree [11 15] :tree
                   [9 11] :house [8 11] :house [40 40] :tree}
          sprites (rendering/scene-sprites objects camera)]
      (is (= #{[11 11] [11 14] [9 11]} (set (map :position sprites))))
      (is (< (count (rendering/visible-cells camera 2)) 100))
      (is (every? config/in-bounds? (rendering/visible-cells camera 2))))))

(deftest renderer-only-prepares-nearby-sprites-test
  (let [camera (rendering/camera {:x 25 :y 25} 800 600)
        original rendering/sprite-placement
        positions (atom [])]
    (with-redefs [rendering/sprite-placement (fn [position kind]
                                              (swap! positions conj position)
                                              (original position kind))]
      (let [image (rendering/render-scene {} world/initial-objects camera)]
        (is (= [800 600] [(.getWidth image) (.getHeight image)]))
        (is (< (count @positions) (count world/initial-objects)))
        (is (every? (set (rendering/visible-cells camera 2)) @positions))))))

(deftest other-players-enter-and-leave-view-test
  (with-redefs [rendering/decorations {}]
    (let [camera (rendering/camera {:x 25 :y 25} 800 600)
          base (pixels (rendering/render-scene {1 {:x 25 :y 25}} {} camera))
          outside (pixels (rendering/render-scene {1 {:x 25 :y 25} 2 {:x 0 :y 0}} {} camera))
          inside (pixels (rendering/render-scene {1 {:x 25 :y 25} 2 {:x 26 :y 25}} {} camera))]
      (is (Arrays/equals ^ints base ^ints outside))
      (is (not (Arrays/equals ^ints base ^ints inside))))))

(deftest local-camera-and-window-resize-test
  (javax.swing.SwingUtilities/invokeAndWait
   (fn []
     (let [players {1 {:x 0 :y 0} 2 {:x 25 :y 25}}
           panel (gui/create-map-panel 0 0 2)]
       (gui/update-map panel players world/initial-objects)
       (doseq [[width height] [[800 600] [1100 450]]]
         (.setSize panel width height)
         (let [target (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
               graphics (.createGraphics target)]
           (try
             (.paint panel graphics)
             (is (= (rendering/camera (players 2) width height)
                    (.getClientProperty panel :settlement.gui/camera)))
             (is (= [width height]
                    (let [image (.getClientProperty panel :settlement.gui/scene)]
                      [(.getWidth image) (.getHeight image)])))
             (finally (.dispose graphics)))))))))

(deftest grass-variants-and-reproducibility-test
  (is (= #{:short :tall :clump} (set (keys rendering/grass-sprites))))
  (doseq [[_ image] rendering/grass-sprites]
    (is (<= (.getWidth image) 24))
    (is (<= (.getHeight image) 18))
    (is (some zero? (map #(unsigned-bit-shift-right % 24) (pixels image))))
    (is (some pos? (map #(unsigned-bit-shift-right % 24) (pixels image)))))
  (let [cells [[22 24] [8 10] [28 24] [0 0] [49 49]]
        forward (into {} (map #(vector % (rendering/grass-for-cell %)) cells))
        reverse-order (into {} (map #(vector % (rendering/grass-for-cell %)) (reverse cells)))]
    (is (= forward reverse-order))
    (is (= forward (into {} (map #(vector % (rendering/cached-grass-for-cell %)) cells)))))
  (doseq [cell [[0 0] [49 49] [22 24]] patch (rendering/grass-for-cell cell)]
    (let [[cx cy] cell {:keys [x y width height]} patch]
      (is (<= (* cx config/tile-size) x (- (* (inc cx) config/tile-size) width)))
      (is (<= (* cy config/tile-size) y (- (* (inc cy) config/tile-size) height))))))

(deftest grass-density-and-path-edges-test
  (let [cells (for [y (range config/world-height) x (range config/world-width)
                    :let [p [x y]]
                    :when (and (not (layout/house-clearance? p))
                               (not (contains? layout/initial-objects p)))] p)
        patches (vec (mapcat rendering/cached-grass-for-cell cells))
        mean-count (fn [predicate]
                     (let [group (filter #(and (predicate %) (not (apply layout/dirt? %))) cells)]
                       (/ (reduce + (map #(count (rendering/cached-grass-for-cell %)) group))
                          (double (count group)))))
        dirt-count (count (filter :on-dirt? patches))]
    (is (> (count patches) 8000))
    (is (> (mean-count layout/clearing?) (mean-count layout/forest?)))
    (is (< dirt-count (* 0.02 (count patches))))
    (is (> (count (filter #(and (:edge? %) (not (:on-dirt? %))) patches)) 150))
    (is (not-any? #(layout/house-clearance? (:cell %)) patches))))

(deftest grass-is-culled-to-camera-test
  (let [camera (rendering/camera {:x 23 :y 25} 800 600)
        cells (atom []) original rendering/cached-grass-for-cell]
    (with-redefs [rendering/cached-grass-for-cell (fn [cell] (swap! cells conj cell) (original cell))]
      (let [patches (vec (rendering/visible-grass {} camera))]
        (is (= (set (rendering/visible-cells camera 0)) (set @cells)))
        (is (< (count @cells) 200))
        (is (every? #(rendering/intersects? camera %) patches))
        (is (seq patches))))))

(deftest grass-does-not-cover-object-sprites-test
  (with-redefs [rendering/decorations {}]
    (let [objects {[24 24] :house [20 20] :tree [27 22] :stone}
          camera (rendering/camera {:x 24 :y 24} 800 600)
          rendered (rendering/render-scene {} objects camera)]
      (doseq [[position kind] objects]
        (let [{:keys [image x y width height]} (rendering/sprite-placement position (rendering/sprite-kind position kind))
              sprite image
              samples (for [sy (range height) sx (range width)
                            :let [color (.getRGB ^BufferedImage sprite sx sy)
                                  px (- (+ x sx) (:x camera)) py (- (+ y sy) (:y camera))]
                            :when (and (= 255 (bit-and 255 (unsigned-bit-shift-right color 24)))
                                       (<= 0 px (dec (:width camera))) (<= 0 py (dec (:height camera))))]
                        [px py color])]
          (is (seq samples))
          (is (every? (fn [[px py color]] (= color (.getRGB rendered px py))) samples)))))))

(deftest grass-preserves-layout-movement-and-mining-test
  (let [composition [layout/initial-objects layout/road-cells layout/house-positions
                     layout/spawn-position layout/decorations]
        [gx gy :as grass-cell] (first (for [y (range 22 29) x (range 20 27)
                                           :when (seq (rendering/grass-for-cell [x y]))] [x y]))]
    (with-redefs [world/world (ref {:players {} :objects {[(inc gx) gy] :tree} :resources {:wood 0 :stone 0}})
                  world/gathering (ref {}) world/now-ns #(deref support/time-ns)
                  world/schedule-resource-restore (fn [& _] nil)]
      (world/add-player 1 "A")
      (dosync (alter world/world update-in [:players 1] assoc :x (dec gx) :y gy))
      (let [before (world/get-world)
            camera (rendering/camera {:x gx :y gy} 800 600)]
        (is (seq (rendering/grass-for-cell grass-cell)))
        (rendering/render-scene (:players before) (:objects before) camera)
        (is (= before (world/get-world)))
        (is (true? (world/move-player 1 :right)))
        (is (= :object-blocked (:code (world/move-player-result 1 :right))))
        (is (= :ok (:status (support/gather! 1))))
        (is (= 1 (get-in (world/get-world) [:players 1 :inventory :items :wood])))
        (is (true? (world/move-player 1 :right)))))
    (is (= composition [layout/initial-objects layout/road-cells layout/house-positions
                        layout/spawn-position layout/decorations]))))
