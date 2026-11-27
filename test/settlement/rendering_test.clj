(ns settlement.rendering-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [settlement.gui :as gui]
            [settlement.client :as client]
            [settlement.movement-ui :as movement-ui]
            [settlement.rendering :as rendering]
            [settlement.visual-v5 :as v5]
            [settlement.visual-v6 :as v6]
            [settlement.massive-mountain :as mountain]
            [settlement.desert-canyon :as canyon]
            [settlement.relief :as relief]
            [settlement.forest :as forest]
            [settlement.water-study :as water-study]
            [settlement.mountain-lake :as lake]
            [settlement.road-art :as road-art]
            [settlement.world :as world]
            [settlement.config :as config]
            [settlement.layout :as layout]
            [settlement.landscape :as landscape]
            [settlement.test-support :as support])
  (:import [javax.imageio ImageIO]
           [java.awt Color]
           [java.awt.image BufferedImage]
           [java.util Arrays]))

(defn- pixels [^BufferedImage image]
  (.getRGB image 0 0 (.getWidth image) (.getHeight image) nil 0 (.getWidth image)))

(deftest terrain-relief-is-opt-in-and-invalidates-ground-when-lighting-changes
  (let [camera (rendering/camera {:x 87 :y 68} 420 325)
        objects layout/initial-objects
        lit {:terrain-relief {:scene :canyon :view :lit}}
        unlit (assoc-in lit [:terrain-relief :view] :unlit)
        base (rendering/prepare-viewport nil objects camera nil)
        light (rendering/prepare-viewport base objects camera lit)
        neutral (rendering/prepare-viewport light objects camera unlit)]
    (is (nil? (relief/options nil)))
    (is (not (Arrays/equals (pixels (:ground base)) (pixels (:ground light)))))
    (is (not (Arrays/equals (pixels (:ground light)) (pixels (:ground neutral)))))
    (is (identical? neutral (rendering/prepare-viewport neutral objects camera unlit)))
    (let [elsewhere (rendering/camera {:x 25 :y 25} 420 325)]
      (is (Arrays/equals (pixels (rendering/render-scene {} objects elsewhere))
                         (pixels (rendering/render-scene {} objects elsewhere {} lit)))))))

(deftest terrain-relief-keeps-the-player-above-the-bridge
  (let [p [87 68]
        player {:x (first p) :y (second p) :team :blue :hp 100 :max-hp 100}
        camera (rendering/camera player 420 325)
        scenery {:terrain-relief {:scene :canyon :view :lit}}
        ^BufferedImage occupied (rendering/render-scene {1 player} layout/initial-objects camera {} scenery)
        ^BufferedImage empty (rendering/render-scene {} layout/initial-objects camera {} scenery)
        x (- (+ (* 87 config/tile-size) 32) (:x camera))
        y (- (* 69 config/tile-size) (:y camera))]
    (is (canyon/bridges p))
    (is (not (layout/initial-objects p)))
    (is (some (fn [[u v]]
                (let [rgb (Color. (.getRGB occupied u v))]
                  (and (not= (.getRGB occupied u v) (.getRGB empty u v))
                       (> (.getBlue rgb) (+ (.getRed rgb) 45)))))
              (for [u (range (- x 15) (+ x 16)) v (range (- y 30) y)] [u v])))))

(deftest terrain-relief-foreground-matches-a-wall-and-fades
  (let [scene {:terrain-relief {:scene :mountain :view :lit}}
        camera (rendering/camera {:x 13 :y 87} 420 325)
        {:keys [x y foot-y anchor] ^BufferedImage source :image :as part} (relief/foreground scene camera)
        [u v] (first (for [v (range (.getHeight source)) u (range (.getWidth source))
                           :when (and (pos? u) (pos? v)
                                      (= 255 (bit-and 255 (unsigned-bit-shift-right (.getRGB source u v) 24))))]
                       [u v]))
        target (BufferedImage. (.getWidth source) (.getHeight source) BufferedImage/TYPE_INT_ARGB)
        graphics (.createGraphics target)
        at {:x u :y v :foot-y (dec foot-y)}]
    (is (= :terrain-blocked (layout/initial-objects anchor)))
    (try
      (#'rendering/draw-layer! graphics (assoc part :x 0 :y 0) [at] 0)
      (is (< 0 (bit-and 255 (unsigned-bit-shift-right (.getRGB target u v) 24)) 255))
      (finally (.dispose graphics)))))

(deftest natural-relief-image-alignment-at-both-bridge-centers
  (when canyon/enabled?
    (doseq [revision [:natural :art :finish]
            [x y :as p] [[87 68] [95 81]]]
      (let [scene {:terrain-relief {:scene :canyon :view :lit :revision revision}}]
        (let [camera (rendering/camera {:x x :y y} 180 160)
              target (BufferedImage. 180 160 BufferedImage/TYPE_INT_ARGB)
              g (.createGraphics target)
              px (- (+ (* x config/tile-size) 32) (:x camera))
              py (- (+ (* y config/tile-size) 53) (:y camera))]
          (is (and (canyon/bridges p) (not (layout/initial-objects p))))
          (try
            (.translate g (- (:x camera)) (- (:y camera)))
            (is (relief/draw-decks! g camera scene))
            (is (pos? (bit-and 255 (unsigned-bit-shift-right (.getRGB target px py) 24))))
            (finally (.dispose g))))))))

(deftest relief-material-studies-are-local
  (doseq [[scene inside outside] [[:mountain [15 70] [18 87]]
                                  [:canyon [81 66] [94 86]]]]
    (let [ground (fn [[x y] revision]
                   (:ground
                    (rendering/prepare-viewport
                     nil layout/initial-objects (rendering/camera {:x x :y y} 320 240)
                     {:terrain-relief {:scene scene :view :lit :revision revision}})))]
      (is (not (Arrays/equals (pixels (ground inside :art))
                              (pixels (ground inside :materials)))))
      (is (Arrays/equals (pixels (ground outside :art))
                         (pixels (ground outside :materials))))
      (is (not (Arrays/equals (pixels (ground inside :materials))
                              (pixels (ground inside :shapes)))))
      (is (Arrays/equals (pixels (ground outside :materials))
                         (pixels (ground outside :shapes))))
      (is (not (Arrays/equals (pixels (ground inside :shapes))
                              (pixels (ground inside :walls)))))
      (is (Arrays/equals (pixels (ground outside :shapes))
                         (pixels (ground outside :walls))))
      (is (not (Arrays/equals (pixels (ground inside :walls))
                              (pixels (ground inside :finish)))))
      (is (Arrays/equals (pixels (ground outside :walls))
                         (pixels (ground outside :finish)))))))

(deftest finish-props-preserve-placement-and-scope
  (let [scenery {:terrain-relief {:scene :canyon :view :lit :revision :finish}}
        crate {:kind :crate :position [81 70] :x 0 :y 0 :width 53 :height 53 :foot-y 53}
        updated (relief/finish-prop scenery crate)
        outside (assoc crate :position [95 76])]
    (is (= crate (dissoc updated :image)))
    (is (= 53 (.getWidth ^BufferedImage (:image updated))))
    (is (= outside (relief/finish-prop scenery outside)))
    (is (= crate (relief/finish-prop nil crate)))))

(deftest integrated-relief-covers-both-regions-without-changing-distant-terrain
  (let [scenery {:terrain-relief {:scene :world :view :lit :revision :integrated}}
        ground (fn [[x y] options]
                 (pixels (:ground (rendering/prepare-viewport
                                   nil layout/initial-objects
                                   (rendering/camera {:x x :y y} 320 240) options))))]
    (doseq [p [[17 86] [94 86]]]
      (is (not (Arrays/equals (ground p {}) (ground p scenery)))))
    (is (Arrays/equals (ground [44 25] {}) (ground [44 25] scenery)))))

(deftest water-study-cache-and-authoritative-bridge
  (let [base rendering/game-scenery
        demo (assoc base :water-study :river :water-revision :natural)
        view (rendering/camera {:x 49 :y 36} 600 440)
        normal (rendering/prepare-viewport nil layout/initial-objects view base)
        polished (rendering/prepare-viewport normal layout/initial-objects view
                                              (assoc demo :water-revision :polished))
        prepared (rendering/prepare-viewport polished layout/initial-objects view demo)]
    (is (not (Arrays/equals (pixels (:ground normal)) (pixels (:ground prepared)))))
    (is (identical? prepared (rendering/prepare-viewport prepared layout/initial-objects view demo)))
    (is (not (Arrays/equals (pixels (:ground polished)) (pixels (:ground prepared)))))
    (let [rail (first (filter #(= :water-rail (:kind %)) (:layers prepared)))
          actor (first (rendering/player-markers {1 {:x 49 :y 36 :team :blue}}))]
      (is rail)
      (is (< (:foot-y actor) (:foot-y rail))))
    (is (not (Arrays/equals (pixels (rendering/render-viewport nil {} view {} prepared 0))
                            (pixels (rendering/render-viewport nil {} view {} prepared 7)))))
    (let [fall-camera (rendering/camera {:x 33 :y 68} 480 360)
          fall (rendering/prepare-viewport nil layout/initial-objects fall-camera
                                          (assoc base :water-study :fall :water-revision :natural))]
      (is (not (Arrays/equals (pixels (rendering/render-viewport nil {} fall-camera {} fall 0))
                              (pixels (rendering/render-viewport nil {} fall-camera {} fall 7))))))
    (with-redefs [world/world (ref {:players {} :objects layout/initial-objects
                                    :resources {:wood 0 :stone 0}})
                  world/gathering (ref {})]
      (world/add-player 1 "Crossing")
      (dosync (alter world/world update-in [:players 1] assoc :x 47 :y 36))
      (doseq [_ (range 4)]
        (is (= :ok (:status (world/move-player-result 1 :right)))))
      (let [blocked (first (for [p [[48 35] [49 35] [50 35] [48 37] [49 37] [50 37]]
                                 :when (= :water (layout/initial-objects p))]
                             p))
            [x y] blocked]
        (is blocked)
        (dosync (alter world/world update-in [:players 1] assoc :x x :y 36))
        (is (= :object-blocked
               (:code (world/move-player-result 1 (if (= y 35) :up :down)))))))))

(deftest global-water-keeps-server-decks-and-canyon-separate
  (let [scenery (assoc rendering/game-scenery :water-study :world :water-revision :world)
        model (water-study/scene scenery)]
    (when (= config/world-seed (:seed model))
      (is (= (count landscape/bridges) (count (:decks model))))
      (is (= (count (:decks model)) (count (:rails model))))
      (is (every? #(nil? (layout/initial-objects %)) landscape/bridge-cells))
      (is (not-any? (:bridge-cells model) canyon/bridges))
      (doseq [[[_ y] {:keys [west east]}] landscape/bridges]
        (let [camera (rendering/camera {:x (/ (+ west east) 2) :y y} 640 300)
              actors (rendering/player-markers {1 {:x (inc west) :y y :team :blue}})
              rail (first (water-study/foregrounds scenery camera))]
          (is rail)
          (is (< (:foot-y (first actors)) (:foot-y rail))))))))

(deftest world-v7-sprites-preserve-building-anchors-and-state
  (let [position [25 25]
        camera (rendering/camera {:x 25 :y 25} 320 240)
        scenery (assoc rendering/game-scenery :world-art true)
        house {:type :building :kind :house :hp 40 :max-hp 200}
        layer (fn [options objects]
                (first (filter #(and (:building? %) (= position (:position %)))
                               (rendering/scene-sprites objects camera options))))
        blue {position (assoc house :team :blue)}
        red {position (assoc house :team :red)}
        previous (layer (dissoc scenery :world-art) blue)
        updated (layer scenery blue)
        rival (layer scenery red)]
    (is (= (dissoc previous :image) (dissoc updated :image) (dissoc rival :image)))
    (is (= :critical (:damage-tier updated)))
    (is (not (Arrays/equals (pixels (:image previous)) (pixels (:image updated)))))
    (is (not (Arrays/equals (pixels (:image updated)) (pixels (:image rival)))))
    (is (identical? (:image updated) (:image (layer scenery blue))))
    (is (nil? (layer scenery {})))))

(deftest forest-study-cache-visibility-and-collisions
  (doseq [[site [tx ty :as tree-cell] position direction]
          [[:leaf [12 38] [13 38] :left] [:pine [37 38] [37 37] :down]]]
    (let [scenery (assoc rendering/game-scenery :forest-study site :forest-revision :polished)
          camera (rendering/camera {:x tx :y ty} 500 400)
          base (rendering/prepare-viewport nil layout/initial-objects camera
                                           (dissoc scenery :forest-revision))
          demo (rendering/prepare-viewport base layout/initial-objects camera scenery)
          layer (first (filter #(= tree-cell (:position %)) (:layers demo)))
          {:keys [x y width height foot-y] ^BufferedImage sprite :image} layer
          [u v] (first (for [v (range 20 130) u (range 20 (- width 20))
                             :when (not (zero? (unsigned-bit-shift-right (.getRGB sprite u v) 24)))]
                         [u v]))
          paint (fn [feet]
                  (let [target (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
                        g (.createGraphics target)]
                    (try
                      (#'rendering/draw-layer! g (assoc layer :x 0 :y 0)
                                               [{:x u :y v :foot-y feet}] 0)
                      (unsigned-bit-shift-right (.getRGB target u v) 24)
                      (finally (.dispose g)))))]
      (is (:forest-tree? layer))
      (is (:forest-shadow layer))
      (is (= :tree (layout/initial-objects tree-cell)))
      (is (not (Arrays/equals (pixels (:ground base)) (pixels (:ground demo)))))
      (is (identical? demo (rendering/prepare-viewport demo layout/initial-objects camera scenery)))
      (is (< 0 (paint (dec foot-y)) (paint (inc foot-y))))
      (with-redefs [world/world (ref {:objects layout/initial-objects
                                      :players {1 {:x (first position) :y (second position) :hp 100}}})]
        (is (= :object-blocked (:code (world/move-player-result 1 direction)))))
      (let [harvested (dissoc layout/initial-objects tree-cell)
            refreshed (rendering/prepare-viewport demo harvested camera scenery)]
        (is (not-any? #(and (:forest-tree? %) (= tree-cell (:position %))) (:layers refreshed)))))))

(deftest forest-world-keeps-relief-and-follows-harvest
  (let [scenery (assoc rendering/game-scenery :forest-study :world :forest-revision :polished)
        previous (dissoc scenery :forest-study :forest-revision)
        camera (rendering/camera {:x 73 :y 7} 1200 630)
        prepared (rendering/prepare-viewport nil layout/initial-objects camera scenery)]
    (is (some :forest-tree? (:layers prepared)))
    (is (identical? prepared
                    (rendering/prepare-viewport prepared layout/initial-objects camera scenery)))
    (doseq [p [[17 86] [94 86]]]
      (let [view (rendering/camera {:x (first p) :y (second p)} 320 240)]
        (is (Arrays/equals (pixels (rendering/render-scene {} layout/initial-objects view {} previous))
                           (pixels (rendering/render-scene {} layout/initial-objects view {} scenery))))))
    (with-redefs [world/world (ref {:players {} :objects layout/initial-objects
                                    :resources {:wood 0 :stone 0}})
                  world/gathering (ref {}) world/now-ns #(deref support/time-ns)
                  world/schedule-resource-restore (fn [& _] nil)]
      (world/add-player 1 "Forest")
      (dosync (alter world/world update-in [:players 1] assoc :x 13 :y 38))
      (let [tree [12 38]
            view (rendering/camera {:x 13 :y 38} 500 400)
            before (rendering/prepare-viewport nil (:objects (world/get-world)) view scenery)]
        (is (some #(and (:forest-tree? %) (= tree (:position %))) (:layers before)))
        (is (some #(and (:ground-shadow? %) (= tree (:position %))) (:layers before)))
        (is (= :object-blocked (:code (world/move-player-result 1 :left))))
        (is (= :ok (:status (support/gather! 1))))
        (let [after (rendering/prepare-viewport before (:objects (world/get-world)) view scenery)]
          (is (not-any? #(and (:forest-tree? %) (= tree (:position %))) (:layers after)))
          (is (not (Arrays/equals (pixels (:ground before)) (pixels (:ground after)))))
          (is (= 1 (get-in (world/get-world) [:players 1 :inventory :items :wood])))
          (is (= :ok (:status (world/move-player-result 1 :left)))))))))

(deftest mountain-lake-water-and-rock-mask-follow-server-cells
  (when lake/enabled?
    (let [[left top right bottom] lake/bounds
          cells (for [x (range left right) y (range top bottom)] [x y])
          wet (filter landscape/water-blocks cells)
          dry (remove landscape/water-blocks cells)
          opacity (fn [^BufferedImage image u v] (unsigned-bit-shift-right (.getRGB image u v) 24))]
      (is (seq wet))
      (is (every? #(and (= :water (layout/initial-objects %))
                        (lake/water-at? landscape/water-blocks % 32 32)
                        (pos? (opacity (:cut (lake/fragment %)) 32 32))) wet))
      (is (every? #(nil? (:cut (lake/fragment %))) dry))
      (is (some (fn [p]
                  (let [^BufferedImage cut (:cut (lake/fragment p))]
                    (and (zero? (opacity cut 0 0)) (pos? (opacity cut 32 32)))))
                wet))
      (is (not-any? lake/local-cell? [[49 70] [9 48] [91 55]])))))

(deftest bridge-adapts-to-crossing-without-losing-opaque-pixels-test
  (let [native (ImageIO/read (io/resource "images/visual_v3/buildings/bridge.png"))
        adapted @@#'rendering/bridge-source
        visible #(frequencies (remove zero? (pixels %)))]
    (is (= (visible native) (visible adapted)))))

(deftest png-loading-and-transparency-test
  (doseq [[kind path] rendering/sprite-paths]
    (testing path
      (let [source (ImageIO/read (io/resource path))
            scaled ^BufferedImage (rendering/sprites kind)
            {[cells-x cells-y] :visual-cells factor :scale :or {factor 1}} (rendering/graphic-specs kind)
            source-alpha (map #(unsigned-bit-shift-right % 24) (pixels source))
            scaled-alpha (map #(unsigned-bit-shift-right % 24) (pixels scaled))]
        (is (.hasAlpha (.getColorModel source)))
        (is (some zero? source-alpha))
        (is (some pos? source-alpha))
        (is (some zero? scaled-alpha))
        (is (some pos? scaled-alpha))
        (is (<= (.getWidth scaled) (* cells-x config/tile-size) rendering/world-pixel-width))
        (is (<= (.getHeight scaled) (* cells-y config/tile-size) rendering/world-pixel-height))
        (is (<= (.getWidth scaled) (* factor (.getWidth source))))
        (is (<= (.getHeight scaled) (* factor (.getHeight source))))
        ;; Округление размеров допускает погрешность до одного пикселя.
        (is (<= (Math/abs (- (double (/ (.getWidth scaled) (.getHeight scaled)))
                             (double (/ (.getWidth source) (.getHeight source)))))
                (/ 1.0 (.getHeight scaled)))))))
  (is (= [128 128] ((juxt #(.getWidth %) #(.getHeight %)) (rendering/sprites :large-boulder))))
  (is (not-any? (set (vals rendering/sprite-paths))
                ["images/mountains/snow_mountain.png" "images/mountains/rock_peak.png"
                 "images/buildings/blue_house.png" "images/buildings/red_house.png"
                 "images/new/pine_tree_alt.png"
                 "images/nature/pine.png" "images/nature/spruce.png" "images/nature/birch.png"
                 "images/nature/broadleaf_tree.png" "images/nature/autumn_tree.png"
                 "images/nature/dead_tree.png"])))

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

(deftest visual-extent-does-not-change-game-cells-test
  (is (= (if v5/enabled? [1 2 3 4 6] [1 2 3 4]) (sort (distinct (map (comp first :visual-cells val) rendering/graphic-specs)))))
  (let [position [4 13]
        camera {:x 256 :y 640 :width 64 :height 64}
        scenery {position :snow-mountain}
        objects {}
        state {:objects objects :players {}}
        test-image (BufferedImage. 256 224 BufferedImage/TYPE_INT_ARGB)
        graphics (.createGraphics test-image)]
    (try (.setColor graphics Color/MAGENTA)
         (.fillRect graphics 0 0 256 224)
         (finally (.dispose graphics)))
    (with-redefs [rendering/decorations {}
                  rendering/sprites (assoc rendering/sprites :snow-mountain test-image)
                  rendering/graphic-specs (assoc rendering/graphic-specs :snow-mountain {:visual-cells [4 4]})]
      ;; A four-cell sprite can enter the view even when its anchor is
      ;; three rows below the last visible cell.
      (is (empty? (rendering/scene-sprites objects camera)))
      (let [[placement] (rendering/scene-sprites objects camera scenery)]
        (is (= position (:position placement)))
        (is (= [4 4] (:visual-cells placement)))
        (is (rendering/intersects? camera placement)))
      (let [plain (rendering/render-scene {} objects camera)
            mountain (rendering/render-scene {} objects camera {} scenery)]
        (is (not (Arrays/equals ^ints (pixels plain) ^ints (pixels mountain)))))
      (is (world/free-cell? state position))))
  (let [static {:objects {[10 10] :house [20 20] :tree} :players {}}]
    (is (not (world/free-cell? static [10 10])))
    (is (world/free-cell? static [11 10]))
    (is (not (world/free-cell? static [20 20])))
    (is (world/free-cell? static [20 19])))
  (let [house {:type :building :kind :house}
        state {:objects {[10 10] house} :players {}}
        placement (rendering/building-placement [10 10] house)]
    (is (= [3 3] (:visual-cells placement)))
    (is (every? #(not (world/free-cell? state %)) [[10 10] [10 11] [11 10] [11 11]]))
    (is (world/free-cell? state [12 10]))))

(deftest large-graphics-keep-their-full-image-at-map-corners-test
  (doseq [kind [:tree-tall :house :large-boulder :tree-small]
          x [0 (dec config/world-width)] y [0 (dec config/world-height)]]
    (let [{:keys [image width height foot-y] left :x top :y}
          (rendering/sprite-placement [x y] kind)]
      (is (identical? image (rendering/sprites kind)))
      (is (<= 0 left (+ left width) rendering/world-pixel-width))
      (is (<= 0 top foot-y rendering/world-pixel-height))
      (is (= foot-y (+ top height))))))

(deftest map-panel-caches-scenery-separately-from-world-test
  (javax.swing.SwingUtilities/invokeAndWait
   (fn []
     (let [panel (doto (gui/create-map-panel 0 0) (.setSize 256 256))
           target (BufferedImage. 256 256 BufferedImage/TYPE_INT_ARGB)
           graphics (.createGraphics target)]
       (try
         (.paint panel graphics)
         (let [before (pixels target)]
           (gui/set-map-scenery! panel {[1 2] :large-boulder})
           (.paint panel graphics)
           (is (not (Arrays/equals ^ints before ^ints (pixels target))))
           (is (= {:players {} :objects {}}
                  (.getClientProperty panel :settlement.gui/state)))
           (gui/set-map-scenery! panel {})
           (.paint panel graphics)
           (is (Arrays/equals ^ints before ^ints (pixels target))))
         (finally (.dispose graphics)))))))

(deftest default-scenery-avoids-protected-and-current-game-cells-test
  (let [camera {:x 0 :y 0 :width rendering/world-pixel-width :height rendering/world-pixel-height}
        scenery rendering/default-scenery]
    (is (= #{:large-boulder :tree :tree-small} (set (vals scenery))))
    (with-redefs [rendering/decorations {}]
      (is (= (set (keys scenery))
             (set (map :position (filter #(scenery (:position %))
                                         (rendering/scene-sprites world/initial-objects camera scenery))))))
      (is (not-any? #(= :large-boulder (:kind %))
                    (rendering/scene-sprites {} camera {layout/spawn-position :large-boulder})))
      (let [[anchor kind] (first (filter (comp #{:large-boulder} val) scenery))
            {:keys [x y]} (rendering/sprite-placement anchor kind)
            covered [(quot x config/tile-size) (quot y config/tile-size)]]
        (is (not= anchor covered))
        (is (#{:stone :stone-alt :mossy-rock}
             (:kind (first (rendering/scene-sprites {covered :stone} camera {anchor kind})))))
        (is (not-any? #(= :large-boulder (:kind %))
                      (rendering/scene-sprites {covered :stone} camera {anchor kind})))))))

(deftest layering-and-decorations-test
  (let [objects world/initial-objects
        layers (rendering/scene-sprites objects {:x 0 :y 0 :width rendering/world-pixel-width :height rendering/world-pixel-height})]
    (is (= layers (sort-by (juxt :foot-y :x :kind) layers)))
    (is (every? (fn [{:keys [^BufferedImage image width height]}]
                  (= [width height] [(.getWidth image) (.getHeight image)]))
                (filter (comp #{:canyon-face :canyon-camp} :kind) layers)))
    (let [replaced (if mountain/redesigned?
                     (count (filter (comp #{:tent :raider-tent} val) mountain/props))
                     0)]
      (is (= (+ (count (filter #(rendering/sprites (rendering/sprite-kind (key %) (val %))) objects))
                (- (count (remove (comp v5/ground-props val) rendering/decorations)) replaced)
                (count (:front mountain/art))
                (count (remove (comp #{:canyon-bridge} :kind) (:front @canyon/art))))
             (count layers))))
    (is (every? #(not (contains? objects %)) (keys rendering/decorations)))
    (is (every? (fn [[x y]] (not (rendering/road? x y))) (keys rendering/decorations)))
    (is (every? config/in-bounds? (keys rendering/decorations))))
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

(deftest players-sharing-a-cell-remain-separate-test
  (let [players {1 {:x 0 :y 0} 2 {:x 0 :y 0}}
        markers (rendering/player-markers players)
        image (rendering/render-scene players {} (rendering/camera nil 600 600))]
    (is (= #{1 2} (set (map :id markers))))
    (is (= 2 (count (set (map (juxt :x :y) markers)))))
    (doseq [{:keys [x y]} markers]
      (is (= (.getRGB (Color. 110 116 109)) (.getRGB image x (+ y 10)))))))

(deftest buildings-fade-behind-their-roof-and-preserve-depth-test
  (let [house (BufferedImage. 128 160 BufferedImage/TYPE_INT_ARGB)
        graphics (.createGraphics house)
        camera {:x 256 :y 192 :width 256 :height 256}
        objects {[5 5] :house}
        marker-color (.getRGB (Color. 110 116 109))]
    (try
      (.setColor graphics Color/RED)
      (.fillRect graphics 0 0 128 160)
      (finally (.dispose graphics)))
    (with-redefs [rendering/sprites {:house house}
                  rendering/decorations {}]
      (let [behind (rendering/render-scene {1 {:x 5 :y 4}} objects camera)
            in-front (rendering/render-scene {1 {:x 5 :y 5}} objects camera)
            x (- (+ (* 5 config/tile-size) 32) (:x camera))]
        (let [blended (.getRGB behind x (- (+ (* 4 config/tile-size) 47) (:y camera)))]
          (is (not= (.getRGB Color/RED) blended))
          (is (not= marker-color blended)))
        (is (= marker-color (.getRGB in-front x (- (+ (* 5 config/tile-size) 47) (:y camera)))))))))

(deftest all-character-sprites-load-from-project-resources-test
  (doseq [[team prefix] [[:blue "player"] [:red "bot"]]
          direction [:up :down :left :right]
          frame (range 3)]
    (let [path (str "images/characters/" prefix "_" (name direction) "_" frame ".png")
          source (ImageIO/read (io/resource path))
          sprite (rendering/character-sprite team direction frame)]
      (is (= [config/tile-size config/tile-size] [(.getWidth sprite) (.getHeight sprite)]) path)
      (is (Arrays/equals ^ints (pixels source) ^ints (pixels sprite)) path)))
  (is (nil? (rendering/character-sprite nil :down 0))))

(deftest character-direction-and-animation-test
  (with-redefs [config/move-animation-ms 220]
    (let [origin {1 {:x 5 :y 5}}
          start (rendering/update-character-motion {} origin 0)]
      (is (= {1 {:direction :down :frame 0}} (rendering/character-poses start 0)))
      (doseq [[x y direction] [[5 4 :up] [5 6 :down] [4 5 :left] [6 5 :right]]]
        (let [players {1 {:x x :y y}}
              moved (rendering/update-character-motion start players 10)
              repeated (rendering/update-character-motion moved players 150)
              middle (get (rendering/visual-players players repeated 120) 1)]
          (is (= [1 2 0 0]
                 (mapv #(get-in (rendering/character-poses repeated %) [1 :frame]) [10 90 180 230])))
          (is (== (/ (+ x 5) 2.0) (:x middle)))
          (is (== (/ (+ y 5) 2.0) (:y middle)))
          (is (= {:direction direction :frame 0}
                 (get (rendering/character-poses repeated 230) 1)))
          (is (= players (rendering/visual-players players repeated 230))))))))

(deftest confirmed-turns-preserve-corners-and-discontinuities-reset-motion-test
  (with-redefs [config/move-animation-ms 220]
    (let [start (rendering/update-character-motion {} {1 {:x 5 :y 5}} 0)
          east (rendering/update-character-motion start {1 {:x 6 :y 5}} 10)
          south {1 {:x 6 :y 6}}
          turn (rendering/update-character-motion east south 30)
          further {1 {:x 6 :y 7}}
          burst (rendering/update-character-motion turn further 40)
          far {1 {:x 70 :y 80}}]
      (is (= [5.5 5.0] ((juxt :x :y) (get (rendering/visual-players south turn 120) 1))))
      (is (= [6.0 5.5] ((juxt :x :y) (get (rendering/visual-players south turn 340) 1))))
      (is (= :right (get-in (rendering/character-poses turn 120) [1 :direction])))
      (is (= :down (get-in (rendering/character-poses turn 340) [1 :direction])))
      (is (= (rendering/visual-players south turn 40) (rendering/visual-players further burst 40)))
      (is (= further (rendering/visual-players further burst 480)))
      (is (= far (rendering/visual-players far (rendering/update-character-motion turn far 50) 50)))
      (is (= {} (rendering/update-character-motion burst {} 60))))))

(deftest depth-and-bridge-foot-height-follow-the-visual-step-test
  (with-redefs [config/move-animation-ms 220]
    (let [west (:west (some (fn [[[_ y] crossing]] (when (= y 25) crossing)) landscape/bridges))
          before {1 {:x (dec west) :y 25 :team :blue}}
          confirmed {1 {:x west :y 25 :team :blue}}
          start (rendering/update-character-motion {} before 0)
          motion (rendering/update-character-motion start confirmed 10)
          markers (fn [now] (first (rendering/player-markers
                                    (rendering/visual-players confirmed motion now))))
          standing (:foot-y (markers 10))]
      (is (== (- standing 8) (:foot-y (markers 120))))
      (is (== (- standing 16) (:foot-y (markers 230))))
      (is (== (+ (:x (markers 10)) 32) (:x (markers 120)))))))

(deftest prepared-rendering-matches-uncached-frames-at-fractional-origins-test
  (let [players {1 {:x 22.4 :y 25.125 :team :blue :name "A"}}
        objects layout/initial-objects
        first-camera (rendering/camera (players 1) 640 432)
        next-camera (update first-camera :x + 41.25)
        prepared (rendering/prepare-viewport nil objects first-camera {})
        image (rendering/render-viewport nil players next-camera {} prepared)
        direct (rendering/render-scene players objects next-camera)]
    (is (identical? prepared (rendering/prepare-viewport prepared objects next-camera {})))
    ;; Java2D can round a small number of road-clip boundary pixels differently
    ;; when rasterizing the same path against two viewport clip rectangles.
    (is (< (count (filter false? (map = (pixels image) (pixels direct)))) 24))
    (is (not (identical? prepared
                         (rendering/prepare-viewport prepared (assoc objects [22 25] :stone) next-camera {}))))
    (let [shifted (-> first-camera (update :x + 172) (update :y + 149))
          partial (rendering/prepare-viewport prepared objects shifted {})
          fresh (rendering/prepare-viewport nil objects shifted {})
          draw #(pixels (rendering/render-viewport nil players shifted {} %))]
      (is (not (identical? prepared partial)))
      (is (< (count (filter false? (map = (draw partial) (draw fresh)))) 48)))))

(deftest visible-water-animates-without-rebuilding-the-ground
  (let [camera (rendering/camera {:x (landscape/river-center 9) :y 9} 640 432)
        prepared (rendering/prepare-viewport nil layout/initial-objects camera {})
        first-frame (pixels (rendering/render-viewport nil {} camera {} prepared 0))
        next-frame (pixels (rendering/render-viewport nil {} camera {} prepared 3))]
    (is (rendering/water-visible? camera))
    (is (not (Arrays/equals ^ints first-frame ^ints next-frame)))
    (is (identical? prepared (rendering/prepare-viewport prepared layout/initial-objects camera {})))))

(deftest held-input-keeps-one-request-and-finishes-an-acknowledged-step-test
  (with-redefs [config/move-animation-ms 220]
    (let [clock (atom 0) requests (atom []) worlds (atom [])
          controller (movement-ui/create-controller {} 1 (constantly nil) #(swap! worlds conj %)
                                                     #(deref clock))
          {:keys [press! release! tick! clear!]} controller]
      (with-redefs [client/async! (fn [_ key message receive send? deliver? _]
                                    (swap! requests conj {:key key :message message :receive receive
                                                          :send? send? :deliver? deliver?})
                                    true)]
        (press! :d :right)
        (press! :d :right)
        (press! :w :up)
        (tick!)
        (is (= 1 (count @requests)))
        (is (false? ((:send? (peek @requests)))))
        ((:receive (peek @requests)) {:status :error :code :stale-request})
        (tick!)
        (is (= :up (get-in (peek @requests) [:message :direction])))
        (is ((:send? (peek @requests))))
        (release! :w)
        (release! :d)
        (reset! clock 500)
        (is ((:deliver? (peek @requests))))
        ((:receive (peek @requests)) {:status :ok :world :confirmed})
        (tick!)
        (is (= [:confirmed] @worlds))
        (is (= 2 (count @requests)))
        (press! :left :left)
        (reset! clock 720)
        (tick!)
        (is (= 3 (count @requests)))
        ((:receive (peek @requests)) {:status :error :code :object-blocked})
        (reset! clock 900)
        (tick!)
        (is (= 3 (count @requests)))
        (is (= [:confirmed] @worlds))
        (reset! clock 940)
        (tick!)
        (is (= 4 (count @requests)))
        (clear!)
        (is (false? ((:send? (peek @requests)))))))))

(deftest scene-uses-server-team-position-and-player-name-test
  (let [camera {:x 0 :y 0 :width 250 :height 250}
        poses {99 {:direction :right :frame 2} 2 {:direction :up :frame 1}}
        ;; These IDs deliberately don't imply their teams.
        players {99 {:x 1 :y 1 :team :blue :name "Алиса"}
                 2 {:x 2 :y 1 :team :red :name "Борис"}}
        image (rendering/render-scene players {} camera poses)
        renamed (rendering/render-scene (assoc-in players [99 :name] "Екатерина") {} camera poses)]
    (doseq [[id {:keys [x y team]}] players
            :let [{:keys [direction frame]} (poses id)
                  sprite (rendering/character-sprite team direction frame)]]
      (is (every? (fn [[sx sy]]
                    (let [rgb (.getRGB sprite sx sy)]
                      (or (not= 255 (bit-and 255 (unsigned-bit-shift-right rgb 24)))
                          (= rgb (.getRGB image (+ (* x config/tile-size) sx)
                                                (+ (* y config/tile-size) sy))))))
                  (for [sy (range 6 64) sx (range 64)] [sx sy]))))
    (is (not (Arrays/equals ^ints (pixels image) ^ints (pixels renamed))))))

(deftest map-panel-renders-new-frames-without-a-new-world-snapshot-test
  (javax.swing.SwingUtilities/invokeAndWait
   (bound-fn []
     (let [now (atom 0)
           panel (doto (gui/create-map-panel 0 0 7) (.setSize 320 240))
           image (BufferedImage. 320 240 BufferedImage/TYPE_INT_ARGB)
           graphics (.createGraphics image)
           original rendering/render-viewport
           rendered (atom nil)
           paint! (fn [milliseconds]
                    (reset! now milliseconds)
                    (.paint panel graphics)
                    (get @rendered 7))]
       (try
         (with-redefs-fn {#'gui/animation-time #(deref now)
                          #'config/move-animation-ms 220
                          #'rendering/render-viewport (fn [previous players camera poses prepared phase]
                                                        (reset! rendered poses)
                                                        (original previous players camera poses prepared phase))}
           (fn []
             (gui/update-map panel {7 {:x 11 :y 11 :team :blue :name "Alice"}} {})
             (is (= {:direction :down :frame 0} (paint! 0)))
             (let [camera-before (.getClientProperty panel :settlement.gui/camera)]
               (gui/update-map panel {7 {:x 12 :y 11 :team :blue :name "Alice"}} {})
               (is (= {:direction :right :frame 1} (paint! 0)))
               (is (= camera-before (.getClientProperty panel :settlement.gui/camera)))
               (let [first-frame (pixels (.getClientProperty panel :settlement.gui/scene))]
                 (is (= {:direction :right :frame 2} (paint! 100)))
                 (is (not (Arrays/equals ^ints first-frame ^ints
                                         (pixels (.getClientProperty panel :settlement.gui/scene))))))
               (let [camera-now (.getClientProperty panel :settlement.gui/camera)
                     target-camera (rendering/camera {:x 12 :y 11} 320 240)]
                 (is (< (:x camera-before) (:x camera-now) (:x target-camera)))
                 (is (not (== (:x camera-now) (long (:x camera-now)))))
                 (is (= 12 (get-in (.getClientProperty panel :settlement.gui/state) [:players 7 :x])))
                 (is (= {:direction :right :frame 0} (paint! 300)))
                 (is (= target-camera (.getClientProperty panel :settlement.gui/camera)))))))
         (finally (.dispose graphics)))))))

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
  (is (= #{:tree :tree-small :tree-tall :broadleaf-tree :autumn-tree :dead-tree}
         (set (for [[position object] world/initial-objects :when (= :tree object)]
                (rendering/sprite-kind position object)))))
  (is (every? #{:stone :stone-alt :mossy-rock}
              (for [[position object] world/initial-objects :when (= :stone object)]
                (rendering/sprite-kind position object))))
  (with-redefs [landscape/biome (constantly :swamp)]
    (is (= :mossy-rock (rendering/sprite-kind [1 2] :stone)))))

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
      (is (= #{[11 11] [11 14] [11 15] [9 11]} (set (map :position sprites))))
      (is (< (count (rendering/visible-cells camera 2)) 100))
      (is (every? config/in-bounds? (rendering/visible-cells camera 2))))))

(deftest tall-tree-preserves-opaque-pixels-at-camera-and-world-edges-test
  (let [source (ImageIO/read (io/resource "images/nature_v3/spruce.png"))
        sprite (rendering/sprites :tree-tall)
        opaque (for [sy (range (.getHeight sprite)), sx (range (.getWidth sprite))
                     :let [pixel (.getRGB source sx sy)]
                     :when (= 255 (bit-and 255 (unsigned-bit-shift-right pixel 24)))]
                 [sx sy pixel])
        edge-x (dec config/world-width)
        edge-y (dec config/world-height)
        corners (for [x [0 edge-x] y [0 edge-y]]
                  [[x y] (rendering/camera {:x x :y y} 192 192) false])]
    (is (= [128 128] [(.getWidth sprite) (.getHeight sprite)]))
    (with-redefs [rendering/decorations {}]
      (doseq [[anchor camera cropped?]
              (concat corners [[[9 14] {:x 512 :y 768 :width 128 :height 128} true]])]
        (let [{left :x top :y} (rendering/sprite-placement anchor :tree-tall)
              objects {anchor :tree-tall}
              scene (rendering/render-scene {} objects camera)
              visible (for [[sx sy pixel] opaque
                            :let [x (- (+ left sx) (:x camera))
                                  y (- (+ top sy) (:y camera))]
                            :when (and (<= 0 x (dec (:width camera)))
                                       (<= 0 y (dec (:height camera))))]
                        [x y pixel])]
          (is (= [anchor] (mapv :position (rendering/scene-sprites objects camera))))
          (is (seq visible))
          (is (= cropped? (< (count visible) (count opaque))))
          (is (every? (fn [[x y expected]] (= expected (.getRGB scene x y))) visible)))))))

(deftest overlapping-canopy-fades-without-changing-depth-test
  (let [anchor [9 14]
        camera {:x 512 :y 768 :width 192 :height 224}
        marker-color (.getRGB (Color. 110 116 109))
        objects {anchor :tree}
        x (- (+ (* 9 config/tile-size) 32) (:x camera))
        behind-y (- (+ (* 13 config/tile-size) 47) (:y camera))
        front-y (- (+ (* 14 config/tile-size) 47) (:y camera))]
    (with-redefs [rendering/decorations {}]
      (is (= :tree-tall (rendering/sprite-kind anchor :tree)))
      (let [tree (.getRGB (rendering/render-scene {} objects camera) x behind-y)]
        (is (not= marker-color tree))
        (is (= marker-color (.getRGB (rendering/render-scene {1 {:x 9 :y 13}} {} camera) x behind-y)))
        (let [faded (.getRGB (rendering/render-scene {1 {:x 9 :y 13}} objects camera) x behind-y)]
          (is (not= tree faded))
          (is (not= marker-color faded)))
        (is (= marker-color
               (.getRGB (rendering/render-scene {1 {:x 9 :y 14}} objects camera) x front-y)))))))

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
        (is (every? (set (rendering/visible-cells camera rendering/visual-culling-margin)) @positions))))))

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


(deftest v5-raster-connects-bank-approaches-and-adjacent-road-tiles
  (when v5/enabled?
    (let [joins (for [[x y :as p] layout/road-cells
                      [dx dy] [[1 0] [0 1]]
                      :let [q [(+ x dx) (+ y dy)]]
                      :when (and (layout/road-link? p q)
                                 (not-any? landscape/wet-cells [p q])
                                 (not-any? landscape/bridge-cells [p q]))]
                  (let [^BufferedImage a (road-art/image p)
                        ^BufferedImage b (road-art/image q)
                        size config/tile-size
                        edge (dec size)]
                    (boolean
                     (some (fn [i]
                             (if (pos? dx)
                               (and (neg? (.getRGB a edge i)) (neg? (.getRGB b 0 i)))
                               (and (neg? (.getRGB a i edge)) (neg? (.getRGB b i 0)))))
                           (range size)))))]
      (is (<= 300 (count joins)))
      (is (every? true? joins)))))


(deftest world-v6-ridge-keeps-its-full-width-and-ground-footprint
  (when (and v6/enabled? (not mountain/enabled?))
    (let [[p [kind width height :as spec]] (find v6/ridges [5 61])
          layer (rendering/sprite-placement p kind)
          camera {:x (+ (:x layer) (* 10 config/tile-size)) :y (:y layer)
                  :width config/tile-size :height (:height layer)}]
      (is (= 14 width))
      (is (= (* width config/tile-size) (:width layer)))
      (is (not (some #{p} (rendering/visible-cells camera rendering/visual-culling-margin))))
      (is (some #(= p (:position %)) (rendering/scene-sprites layout/initial-objects camera)))
      (is (= (* width height) (count (landscape/footprint p spec))))
      (is (every? layout/initial-objects (landscape/footprint p spec)))
      (is (not-any? layout/road-cells (landscape/footprint p spec))))))


(deftest massive-mountain-redesign-culls-depth-sorted-foreground
  (when mountain/redesigned?
    (let [[ox oy] (map #(* config/tile-size %) (:origin mountain/model))
          part (first (:front mountain/art))
          camera (assoc (select-keys part [:width :height]) :x (+ ox (:x part)) :y (+ oy (:y part)))
          layers (rendering/scene-sprites layout/initial-objects camera)
          shown (filter (comp #{:mountain-face :mountain-camp} :kind) layers)]
      (is (some #(= (:asset part) (:asset %)) shown))
      (is (< (count shown) (count (:front mountain/art))))
      (is (every? #(rendering/intersects? camera %) shown))
      (is (= layers (sort-by (juxt :foot-y :x :kind) layers))))))
