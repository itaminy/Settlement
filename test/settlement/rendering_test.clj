(ns settlement.rendering-test
  (:require [clojure.test :refer :all]
            [settlement.gui :as gui]
            [settlement.rendering :as rendering]
            [settlement.world :as world])
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
                         (<= (+ x width) rendering/canvas-size)
                         (<= (+ y height) rendering/canvas-size))))
                (for [x (range 12) y (range 12)] [x y]))
        (str "Full sprite fits for " kind))))

(deftest layering-and-decorations-test
  (let [objects world/initial-objects
        layers (rendering/scene-sprites objects)]
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
      (let [image (rendering/render-scene {} {[4 4] :house [4 5] :stone})]
        (is (= (.getRGB Color/BLUE) (.getRGB image 350 350)))))))

(deftest players-visible-over-objects-test
  (let [players {1 {:x 0 :y 0} 2 {:x 0 :y 0}}
        markers (rendering/player-markers players)
        image (rendering/render-scene players {[0 0] :house})]
    (is (= #{1 2} (set (map :id markers))))
    (is (= 2 (count (set (map (juxt :x :y) markers)))))
    (doseq [{:keys [x y color]} markers]
      (is (= (.getRGB ^Color color) (.getRGB image x (+ y 10)))))))

(deftest map-panel-and-disappearing-object-test
  (javax.swing.SwingUtilities/invokeAndWait
   (fn []
     (let [panel (gui/create-map-panel 0 0)
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
         (let [expected (rendering/scale-image (rendering/render-scene players objects) 700 700)
               actual (.getRGB target 150 0 700 700 nil 0 700)]
           (is (Arrays/equals ^ints (pixels expected) ^ints actual)))
         (let [before (pixels (.getClientProperty panel :settlement.gui/scene))]
           (gui/update-map panel players {})
           (is (not (Arrays/equals ^ints before
                                   ^ints (pixels (.getClientProperty panel :settlement.gui/scene))))))
         (is (= {[5 5] :house} objects))
         (finally (.dispose graphics)))))))

(deftest terrain-and-stable-variants-test
  (is (every? #(rendering/road? % 6) (range 12)))
  (is (every? #(rendering/road? 4 %) (range 3 9)))
  (is (not (rendering/road? 3 5)))
  (is (= #{:tree}
         (set (for [[position object] world/initial-objects :when (= :tree object)]
                (rendering/sprite-kind position object)))))
  (is (= #{:stone :stone-alt}
         (set (for [[position object] world/initial-objects :when (= :stone object)]
                (rendering/sprite-kind position object))))))

(deftest temporary-silhouette-exclusion-test
  (is (not-any? #{:flowers} (vals rendering/decorations))))
