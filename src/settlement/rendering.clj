(ns settlement.rendering
  (:require [clojure.java.io :as io]
            [settlement.config :as config]
            [settlement.landscape :as landscape]
            [settlement.layout :as layout]
            [settlement.road-art :as road-art]
            [settlement.visual-v5 :as v5]
            [settlement.visual-v6 :as v6]
            [settlement.massive-mountain :as mountain]
            [settlement.mountain-lake :as lake]
            [settlement.desert-canyon :as canyon]
            [settlement.relief :as relief]
            [settlement.forest :as forest]
            [settlement.water-study :as water-study]
            [settlement.world-props :as world-props]
            [settlement.world-terrain :as world-terrain]
            [settlement.effects :as effects]
            [settlement.ui-theme :as theme]
            [settlement.buildings :as buildings])
  (:import [javax.imageio ImageIO]
           [java.awt AlphaComposite Color Font Graphics2D RenderingHints]
           [java.awt.image BufferedImage]
           [java.util LinkedHashMap]))

(def map-size config/world-width)
(def tile-size config/tile-size)
(def world-pixel-width (* config/world-width tile-size))
(def world-pixel-height (* config/world-height tile-size))

(defn camera [player width height]
  (let [width (max 1 (min width world-pixel-width))
        height (max 1 (min height world-pixel-height))
        center-x (+ (* (:x player 0) tile-size) (quot tile-size 2))
        center-y (+ (* (:y player 0) tile-size) 37)
        clamp (fn [value maximum] (max 0 (min maximum value)))]
    {:x (clamp (- center-x (quot width 2)) (- world-pixel-width width))
     :y (clamp (- center-y (quot height 2)) (- world-pixel-height height))
     :width width :height height}))

(defn raster-origin [{:keys [x y]}]
  [(Math/round (double x)) (Math/round (double y))])

(defn visible-cells [{:keys [x y width height]} margin]
  (let [first-cell (fn [p] (max 0 (- (long (Math/floor (/ p tile-size))) margin)))
        end-cell (fn [p extent limit]
                   (min limit (+ margin (long (Math/ceil (/ (+ p extent) tile-size))))))]
    (for [cy (range (first-cell y) (end-cell y height config/world-height))
          cx (range (first-cell x) (end-cell x width config/world-width))]
      [cx cy])))

(defn intersects? [camera {:keys [x y width height]}]
  (and (< x (+ (:x camera) (:width camera)))
       (< y (+ (:y camera) (:height camera)))
       (> (+ x width) (:x camera)) (> (+ y height) (:y camera))))

(defn- visual-path [folder file]
  (str "images/visual_v3/" (name folder) "/" file ".png"))

(def ^:private visual-groups
  {:vegetation {:bush "bush" :berry-bush "berry_bush" :mushroom-red "mushrooms"
                :mushroom-green "mushroom_group" :mushroom-group "mushroom_group"
                :stump "stump" :fallen-tree "fallen_tree" :fern "fern" :marsh-fern "fern" :reeds "reeds"
                :tall-grass "tall_grass" :white-flowers "white_flowers"
                :yellow-flowers "yellow_flowers" :blue-flowers "blue_flowers"}
   :mountains {:stone "small_rock" :stone-alt "medium_rock" :large-boulder "large_boulder"
               :mossy-rock "mossy_rock" :mountain "mountain" :snow-mountain "snow_mountain"
               :rock-peak "rock_peak" :cliff "cliff" :cave "cave" :gold-ore "gold_ore"
               :iron-ore "iron_ore" :crystals "crystals"}
   :desert {:cactus "cactus" :flowering-cactus "flowering_cactus" :dry-bush "dry_bush"
            :bones "bones" :sand-boulders "sand_boulders" :sand-rocks "sand_rocks" :palm "palm"}
   :buildings {:house "neutral_house" :blue-house "blue_house" :red-house "red_house"
               :wooden-fence "fence" :sign "sign" :mine "mine" :tent "tent"
               :watchtower "watchtower" :sawmill "sawmill" :gate "gate"
               :blue-flag "blue_flag" :red-flag "red_flag"}
   :resources {:log-bundle "log_bundle" :stone-pile "stone_pile"
               :barrel "barrel" :crate "crate" :chest "chest"}
   :decorations {:well "well" :shore-rocks "shore_rocks" :campfire "campfire"}})

(def ^:private v4-props
  {:raider-tent 112 :ruined-wall 128 :boardwalk 110 :ember-pit 54 :cairn 55
   :bank-stones 54 :rushes 58 :dune-grass 55 :forest-fern 60 :lantern 56 :islet 128})

(def ^:private ridge-kinds #{:massif :escarpment :snow-ridge :sand-ridge})
(defn- v41-path [file] (str "images/visual_v4_1/" file ".png"))

(def sprite-paths
  (into {:tree "images/nature_v3/pine.png"
         :tree-small "images/nature_v3/birch.png"
         :tree-tall "images/nature_v3/spruce.png"
         :broadleaf-tree "images/nature_v3/broadleaf_tree.png"
         :autumn-tree "images/nature_v3/autumn_tree.png"
         :dead-tree "images/nature_v3/dead_tree.png"}
        (concat (for [[folder paths] visual-groups [kind file] paths]
                  [kind (visual-path folder file)])
                (for [kind (keys v4-props)]
                  [kind (str "images/visual_v4/props/" (name kind) "_0.png")])
                (for [kind ridge-kinds] [kind (if v5/enabled? (v5/path kind 0) (v41-path (str (name kind) "_0")))])
                (when v5/enabled? (for [kind (keys v5/props)] [kind (v5/path kind 0)]))
                (when v6/enabled? [[:frost-pine (v6/path "frost-pine")]
                                   [:marsh-hamlet (v6/path "marsh-hamlet")]]))))

(defn load-image [path]
  (or (ImageIO/read (io/file path))
      (throw (ex-info "Cannot load image" {:path path}))))

(defn- load-resource-image [path]
  (or (when-let [url (io/resource path)] (ImageIO/read url))
      (throw (ex-info "Cannot load image resource" {:path path}))))

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

(defn- graphic [cells width height]
  {:visual-cells [cells cells] :pixel-size [width height]})

;; The visual envelope and image scale belong to the renderer. Game cells
;; are determined by the world and the building catalog.
(def graphic-specs
  (into {:tree (graphic 2 112 112), :tree-small (graphic 2 100 100)
   :tree-tall (graphic 2 128 128)
   :broadleaf-tree (graphic 2 122 122), :autumn-tree (graphic 2 116 116)
   :dead-tree (graphic 2 108 108)
   :stone (graphic 1 54 54), :stone-alt (graphic 2 95 95)
   :house (graphic 3 172 172), :built-house (graphic 3 172 172)
   :blue-house (graphic 3 172 172), :red-house (graphic 3 172 172)
   :fence (graphic 1 tile-size tile-size), :wooden-fence (graphic 1 tile-size tile-size)
   :bush (graphic 1 64 64), :berry-bush (graphic 1 64 64)
   :mushroom-red (graphic 1 44 44), :mushroom-green (graphic 1 44 44)
   :mushroom-group (graphic 1 47 47), :stump (graphic 1 50 50)
   :fallen-tree (graphic 2 85 85), :fern (graphic 1 52 52), :marsh-fern (graphic 1 52 52), :reeds (graphic 1 62 62)
   :tall-grass (graphic 1 48 48), :white-flowers (graphic 1 48 48)
   :yellow-flowers (graphic 1 48 48), :blue-flowers (graphic 1 48 48)
   :shore-rocks (graphic 1 48 48), :sign (graphic 1 56 56)
   :large-boulder (graphic 2 128 128), :mossy-rock (graphic 2 96 96)
   :mountain (graphic 4 256 256), :snow-mountain (graphic 4 256 256)
   :rock-peak (graphic 3 192 192), :cliff (graphic 3 192 192)
   :cave (graphic 2 128 128), :gold-ore (graphic 2 92 92), :iron-ore (graphic 2 92 92)
   :crystals (graphic 2 125 125), :cactus (graphic 2 108 108)
   :flowering-cactus (graphic 2 108 108), :dry-bush (graphic 1 53 53)
   :bones (graphic 1 56 56), :sand-boulders (graphic 2 128 128)
   :sand-rocks (graphic 3 192 192), :palm (graphic 3 150 150)
   :mine (graphic 2 128 128), :tent (graphic 2 128 128), :watchtower (graphic 3 168 168)
   :sawmill (graphic 3 168 168), :well (graphic 2 98 98), :gate (graphic 2 100 100)
   :blue-flag (graphic 1 58 58), :red-flag (graphic 1 58 58)
   :log-bundle (graphic 1 62 62), :stone-pile (graphic 1 52 52), :barrel (graphic 1 54 54)
   :crate (graphic 1 53 53), :chest (graphic 1 53 53), :campfire (graphic 1 57 57)}
        (concat (for [[kind size] v4-props]
                  [kind (graphic (if (> size tile-size) 2 1) size size)])
                (when v6/enabled? [[:frost-pine (graphic 2 128 128)]
                                   [:marsh-hamlet (graphic 4 232 174)]])
                (for [kind ridge-kinds] [kind (if v5/enabled? (graphic 6 384 300) (graphic 4 256 256))])
                (when v5/enabled?
                  (for [kind (keys v5/props)]
                    [kind (if (v5/ground-props kind)
                            (graphic 4 224 96)
                            (graphic 3 192 128))])))))

(def visual-culling-margin (if v5/enabled? 6 4)) ; Maximum supported visual span in either axis.

(defn- size-sprite [kind ^BufferedImage image]
  (let [{[width height] :pixel-size [cells-x cells-y] :visual-cells factor :scale :or {factor 1}}
        (graphic-specs kind)
        native-width (.getWidth image)
        native-height (.getHeight image)
        limit-x (min width (* cells-x tile-size) (* factor native-width))
        limit-y (min height (* cells-y tile-size) (* factor native-height))]
    (if (= [limit-x limit-y] [native-width native-height])
      image
      (scale-image image limit-x limit-y))))

(def ^:private base-sprites
  (into {} (map (fn [[kind path]] [kind (size-sprite kind (load-resource-image path))]) sprite-paths)))

(def fence-image (:wooden-fence base-sprites))

(defn- connected-fence-image [mask]
  (if (zero? mask) fence-image
    (let [image (BufferedImage. tile-size tile-size BufferedImage/TYPE_INT_ARGB)
          g (.createGraphics image)]
      (try
        (doseq [[bit rectangles] [[1 [[25 0 8 32] [39 0 8 32]]]
                                  [2 [[32 25 32 8] [32 39 32 8]]]
                                  [4 [[25 32 8 32] [39 32 8 32]]]
                                  [8 [[0 25 32 8] [0 39 32 8]]]]
                :when (pos? (bit-and mask bit)) [x y w h] rectangles]
          (.setColor g (Color. 78 49 28)) (.fillRect g x y w h)
          (.setColor g (Color. 170 124 67)) (.fillRect g (+ x 2) (+ y 2) (- w 4) (- h 4)))
        (.drawImage g ^BufferedImage fence-image 0 0 nil)
        (finally (.dispose g))) image)))

(def fence-images (into {} (for [mask (range 16)] [mask (connected-fence-image mask)])))

(defn fence-connections [objects [x y]]
  (reduce + (for [[bit dx dy] [[1 0 -1] [2 1 0] [4 0 1] [8 -1 0]]
                  :let [object (get objects [(+ x dx) (+ y dy)])]
                  :when (and (buildings/building? object) (= :fence (:kind object)))] bit)))

(def sprites (assoc base-sprites :fence fence-image :built-house (:house base-sprites)))

(def decorations layout/decorations)
(def road? layout/road?)

(defn- v4-path [folder file]
  (str "images/visual_v4/" (name folder) "/" file ".png"))

(def ^:private prop-variants
  (into {} (for [kind (concat (keys v4-props) ridge-kinds (when v5/enabled? (keys v5/props)))]
             [kind (mapv #(let [file (str (name kind) "_" %)
                                path (cond
                                       (and v5/enabled? (or (ridge-kinds kind) (v5/props kind))) (v5/path kind %)
                                       (ridge-kinds kind) (v41-path file)
                                       :else (v4-path :props file))]
                            (size-sprite kind (load-resource-image path))) (range (get v5/props kind 4)))])))

(def ^:private terrain-assets
  (into (into {} (for [kind [:duff :mud :dune :scree :packed]]
                   [kind (mapv #(load-resource-image (v41-path (str (name kind) "_" %))) (range 4))]))
        (for [[folder kinds count] [[:terrain [:grass :flowering_grass :rocky_ground :sand :desert :snow :swamp] 7]
                                      [:water [:river :lake :shallow_water] 5]]
                 kind kinds]
             [kind (mapv #(load-resource-image (v4-path folder (str (name kind) "_" %))) (range count))])))

(def ^:private terrain-v5
  (when v5/enabled?
    (into {} (for [kind v5/terrain]
               [kind (mapv #(load-resource-image (v5/path kind %)) (range 6))]))))

(def ^:private road-assets
  (into {} (for [kind [:dirt :forest :mountain :sand :stone]]
             [kind (mapv #(load-resource-image (v4-path :roads (str (name kind) "_" %))) (range 16))])))

(defn- sample-tile [terrain position]
  (let [images (or (get terrain-v5 terrain) (terrain-assets terrain))]
    (nth images (mod (hash [config/world-seed position terrain]) (count images)))))

(def ^:private composition-image
  (memoize #(load-resource-image (v6/path %))))

(defn- water-region-weight [kind x y]
  (let [[cx cy radius-x radius-y] (if (= kind :mire-water) [67 55 9.2 10.7] [24.1 68.8 6.1 4.7])
        dx (/ (- x cx) radius-x)
        dy (/ (- y cy) radius-y)
        radius (+ (* dx dx) (* dy dy))
        shoreline (* 0.10 (Math/sin (+ (* x 1.4) (* y 1.8))))]
    (max 0.0 (min 1.0 (* 2.5 (+ 1.20 shoreline (- radius)))))))

(defn- regional-water [[x y :as p]]
  (when v6/enabled?
    (cond
      (pos? (water-region-weight :mire-water (+ x 0.5) (+ y 0.5))) :mire-water
      (or (= :snow (landscape/biome p))
          (pos? (water-region-weight :ice-water (+ x 0.5) (+ y 0.5)))) :ice-water)))

(defn- wet-image [[x y :as p]]
  (let [main? (< (abs (- (+ x 0.5) (landscape/river-center (+ y 0.5)))) 3.0)
        ;; The first four lake variations share the same deep-water border.
        ;; Lily pads stay off the main channel.
        variants (terrain-assets :lake)
        index (mod (hash [config/world-seed p :water]) (if main? 4 5))]
    (if (and v5/enabled? (not= 4 index))
      (sample-tile :depth p)
      (nth variants index))))

(defn- ground-texture-at [p]
  (let [kind (landscape/terrain p)
        patch (landscape/variation :ground p)]
    (cond
      (some (fn [[x y]] (< (+ (abs (- x (first p))) (abs (- y (second p)))) 3))
            landscape/camps) :packed
      (and (not v5/enabled?) (= kind :grass) (layout/forest? p) (> patch 0.52)) :duff
      (and (= kind :grass) (= :plains (landscape/biome p)) (> patch 0.61)) :flowering_grass
      (and (not v5/enabled?) (= kind :rocky_ground) (> patch 0.49)) :scree
      (and (not v5/enabled?) (= kind :desert) (> patch 0.46)) :dune
      (and (not v5/enabled?) (= kind :swamp) (> patch 0.41)) :mud
      :else kind)))

(def ^:private ground-texture (memoize ground-texture-at))

(defn- blend-mask [[nw ne se sw] u v]
  (+ (* (- 1 u) (- 1 v) nw) (* u (- 1 v) ne) (* u v se) (* (- 1 u) v sw)))

(defn- mix-ground-rgb [a b weight]
  (reduce (fn [rgb shift]
            (let [from (bit-and 255 (unsigned-bit-shift-right a shift))
                  to (bit-and 255 (unsigned-bit-shift-right b shift))
                  value (+ from (quot (* (- to from) weight) 8))]
              (bit-or rgb (bit-shift-left value shift))))
          0 [16 8 0]))

(def ^:private detailed-ground
  (memoize
   (fn [[x y :as p]]
     (let [kind (ground-texture p)
           detail (case kind
                    :desert :dune
                    :rocky_ground :shale
                    :swamp :bog
                    :grass (when (layout/forest? p) :litter)
                    nil)
           ^BufferedImage base (sample-tile kind p)]
       (if-not (and v5/enabled? detail)
         base
         (let [^BufferedImage alternate (sample-tile detail p)
               image (BufferedImage. tile-size tile-size BufferedImage/TYPE_INT_RGB)
               corners (mapv #(landscape/variation :ground %) [p [(inc x) y] [(inc x) (inc y)] [x (inc y)]])]
           (doseq [u (range 0 tile-size 2) v (range 0 tile-size 2)
                   :let [weight (int (max 0 (min 8 (* 32 (- (blend-mask corners (/ u (double tile-size)) (/ v (double tile-size))) 0.36)))))
                         color (mix-ground-rgb (.getRGB base u v) (.getRGB alternate u v) weight)]
                   du [0 1] dv [0 1]]
             (.setRGB image (+ u du) (+ v dv) color))
           image))))))

(defn- regional-water-texture [[x y :as p] kind ^BufferedImage base ^BufferedImage regional]
  (if (and (= :ice-water kind) (= :snow (landscape/biome p)))
    regional
    (let [out (BufferedImage. tile-size tile-size BufferedImage/TYPE_INT_RGB)]
      (doseq [u (range 0 tile-size 2) v (range 0 tile-size 2)
              :let [weight (int (+ 0.5 (* 8 (water-region-weight kind (+ x (/ u (double tile-size)))
                                                                   (+ y (/ v (double tile-size)))))))
                    rgb (mix-ground-rgb (.getRGB base u v) (.getRGB regional u v) weight)]
              du [0 1] dv [0 1]]
        (.setRGB out (+ u du) (+ v dv) rgb))
      out)))

(defn- ice-tile [p]
  (let [variant (mod (hash p) 6)]
    (composition-image (if (zero? variant) "ice-water" (str "ice-water_" variant)))))

(defn- terrain-at [[x y :as p]]
  (let [mask (landscape/shore-masks p)
        land (detailed-ground p)
        water-kind (regional-water p)
        wet (if (and water-kind (not= [1 1 1 1] mask))
              (regional-water-texture p water-kind (wet-image p)
                                      (if (= :ice-water water-kind)
                                        (ice-tile p)
                                        (composition-image (if (zero? (mod (hash p) 9))
                                                             "mire-pads" "mire-water"))))
              (wet-image p))
        center-wet? (landscape/wet-cells p)
        texture (ground-texture p)
        edge? (some #(not= (ground-texture %) texture) (layout/neighbors p))]
    (cond
      (= [0 0 0 0] mask) wet
      (and (= [1 1 1 1] mask) (not center-wet?) (not edge?)) land
      :else
      (let [image (BufferedImage. tile-size tile-size BufferedImage/TYPE_INT_RGB)
            bank (sample-tile (case (landscape/biome p)
                                :mountains :rocky_ground :snow :rocky_ground
                                :swamp :swamp :sand) p)
            shallow-base (sample-tile (if v5/enabled? :lagoon :shallow_water) p)
            shallow (if water-kind
                      (regional-water-texture p water-kind shallow-base
                                              (if (= :ice-water water-kind) (ice-tile p)
                                                  (composition-image "mire-shallows")))
                      shallow-base)
            adjacent (mapv detailed-ground [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])]
        (doseq [v (range 0 tile-size 2) u (range 0 tile-size 2)
                :let [tx (/ (+ 1.0 u) tile-size)
                      ty (/ (+ 1.0 v) tile-size)
                      edge-distance (min tx ty (- 1 tx) (- 1 ty))
                      wobble (* 0.055 (Math/sin (+ (* 0.23 (+ u (* x tile-size)))
                                                  (* 0.17 (+ v (* y tile-size))))))
                      dry (+ (blend-mask mask tx ty) wobble
                             (if (and center-wet? (= [1 1 1 1] mask))
                               (- (* 0.74 (Math/pow (* 16 tx (- 1 tx) ty (- 1 ty)) 2))) 0))
                      edge-index (cond (= edge-distance tx) 0 (= edge-distance (- 1 tx)) 1
                                       (= edge-distance ty) 2 :else 3)
                      ^BufferedImage source (cond (< dry 0.29) wet (< dry 0.48) shallow
                                                  (< dry 0.60) bank :else land)
                      base (.getRGB source u v)
                      other ^BufferedImage (adjacent edge-index)
                      width (+ (if v5/enabled? 0.26 0.17) (* 0.7 wobble))
                      weight (if (< edge-distance width)
                               (int (+ 0.5 (* 4.0 (- 1.0 (/ edge-distance width))))) 0)
                      color (if (and edge? (= source land) (> weight 0))
                              (mix-ground-rgb base (.getRGB other u v) weight) base)]
                dv [0 1] du [0 1]]
          (.setRGB image (+ u du) (+ v dv) color))
        image))))

(def ^:private cached-terrain (memoize terrain-at))

(defn- road-mask [[x y :as p]]
  (reduce + (map-indexed (fn [index cell] (if (layout/road-link? p cell) (bit-shift-left 1 index) 0))
                         [[x (dec y)] [(inc x) y] [x (inc y)] [(dec x) y]])))

(defn- road-image [p]
  (if v5/enabled?
    (road-art/image p)
    (when (and (layout/road-cells p) (not (landscape/wet-cells p)))
      (get-in road-assets [(layout/road-material p) (road-mask p)]))))

(def ^:private dry-wash
  (memoize
   (fn [[x y :as p]]
     (when (and (<= 77 x 98) (<= 83 y 94) (= :desert (landscape/biome p))
                (not (landscape/wet-cells p)))
       (let [image (BufferedImage. tile-size tile-size BufferedImage/TYPE_INT_ARGB)
             ^BufferedImage texture (sample-tile :wash p)
             [ox oy] [(* x tile-size) (* y tile-size)]]
         (doseq [u (range 0 tile-size 2)
                 :let [world-x (/ (+ ox u) (double tile-size))
                       t (/ (- world-x 77.0) 22.0)
                       center (+ 88.5 (* 2.9 (Math/sin (+ 0.7 (* t 6.4)))) (* t 3.1))
                       channel-width (* (+ 0.27 (* 0.22 (Math/sin (* 13 t))))
                                        (Math/sin (* Math/PI t)))]
                 v (range 0 tile-size 2)
                 :let [world-y (/ (+ oy v) (double tile-size))
                       distance (abs (- world-y center))
                       alpha (int (min 240 (* 430 (- channel-width distance))))]
                 :when (pos? alpha)
                 du [0 1] dv [0 1]]
           (.setRGB image (+ u du) (+ v dv)
                    (unchecked-int (bit-or (bit-shift-left alpha 24)
                                           (bit-and 0xffffff (.getRGB texture u v))))))
         image)))))

(defn draw-terrain-tiles! [graphics camera]
  (doseq [[x y :as p] (visible-cells camera 0)]
    (.drawImage graphics ^BufferedImage (cached-terrain p) (* x tile-size) (* y tile-size) nil)
    (when-let [image (when v5/enabled? (dry-wash p))]
      (.drawImage graphics ^BufferedImage image (* x tile-size) (* y tile-size) nil))
    (when-let [image (when (or (not (mountain/route-cells p)) (= p mountain/entrance))
                       (road-image p))]
      (.drawImage graphics ^BufferedImage image (* x tile-size) (* y tile-size) nil))))


(def terrain-chunk-cells 10)
(defn- chunk-overlaps? [x y [left top right bottom]]
  (let [cx (* x terrain-chunk-cells)
        cy (* y terrain-chunk-cells)]
    (and (< cx right) (< left (+ cx terrain-chunk-cells))
         (< cy bottom) (< top (+ cy terrain-chunk-cells)))))

(defn- mountain-ground-chunk? [x y]
  (let [[ox oy] (:origin mountain/model)
        [w h] (:size mountain/model)]
    (chunk-overlaps? x y [ox oy (+ ox w) (+ oy h)])))

(defn- canyon-ground-chunk? [x y]
  (and canyon/enabled? (chunk-overlaps? x y [73 64 100 100])))

(def ^:private lake-ground-chunk?
  (memoize (fn [x y]
             (and (chunk-overlaps? x y lake/bounds)
                  (let [left (* x terrain-chunk-cells)
                        top (* y terrain-chunk-cells)]
                    (some lake/affected-cell?
                          (for [cy (range top (+ top terrain-chunk-cells))
                                cx (range left (+ left terrain-chunk-cells))]
                            [cx cy])))))))

(defn owns-ground-chunk? [x y]
  (cond
    lake/enabled? (or (lake-ground-chunk? x y) (canyon-ground-chunk? x y))
    (#{"6_3" "6_3_1"} mountain/art-version) (mountain-ground-chunk? x y)
    :else true))

(defn terrain-chunk-path
  ([x y] (terrain-chunk-path :terrain x y))
  ([layer x y]
   (let [root (cond
                (not mountain/enabled?) "images/visual_v6/"
                (= layer :terrain) "images/visual_v6_1/"
                (and (= layer :ground) (canyon-ground-chunk? x y)) canyon/image-root
                (and lake/enabled? (lake-ground-chunk? x y)) "images/visual_v6_3_2/"
                (and lake/enabled? (mountain-ground-chunk? x y)) mountain/image-root
                (and (= layer :ground) (not (owns-ground-chunk? x y))) "images/visual_v6_2/"
                :else mountain/image-root)]
     (str root (name layer) "/" config/world-seed "/" x "_" y ".png"))))

(def ^:private terrain-chunks (LinkedHashMap. 48 0.75 true))
(def ^:private chunk-limit 36)

(defn- terrain-chunk [layer x y]
  (locking terrain-chunks
    (let [key [layer x y]]
      (if (.containsKey terrain-chunks key)
        (.get terrain-chunks key)
        (let [image (when-let [url (io/resource (terrain-chunk-path layer x y))]
                      (ImageIO/read url))]
          (.put terrain-chunks key image)
          (when (> (.size terrain-chunks) chunk-limit)
            (.remove terrain-chunks (first (.keySet terrain-chunks))))
          image)))))

(def ^:private baked-terrain?
  (delay (and v6/enabled? (some? (io/resource (terrain-chunk-path 0 0))))))

(def ^:private road-mask-image
  (delay (when v6/enabled? (terrain-chunk :terrain "roads" "mask"))))

(defn- draw-chunks! [graphics {:keys [x y width height]} layer draw-missing!]
  (let [span (* terrain-chunk-cells tile-size)]
    (doseq [cy (range (quot y span) (inc (quot (+ y height -1) span)))
            cx (range (quot x span) (inc (quot (+ x width -1) span)))
            :let [ox (* cx span) oy (* cy span)]]
      (if-let [^BufferedImage image (terrain-chunk layer cx cy)]
        (.drawImage graphics image ox oy nil)
        (draw-missing! graphics {:x ox :y oy :width span :height span})))))

(defn- draw-terrain! [graphics camera]
  (if @baked-terrain?
    (draw-chunks! graphics camera :terrain draw-terrain-tiles!)
    (draw-terrain-tiles! graphics camera)))

(def grass-patterns
  {:short ["   m l   " " l ml m  " " mdmdmdm " "  dmmdm  " "   ddd   "]
   :tall ["    l    " " l  m    " " m  m l  " " md m m  " "  mmm m l"
          "l dmmdm m" "m  mmmmmd" " d dmmdm " "   dddd  "]
   :clump ["  l    l    " "  m l  m  l " "l mdm lmd m " "m mmm mmmmd "
           " mdmmmdmmmdm" "  dmmdmmmd  " "   ddddd    "]})

(defn- make-grass-sprite [rows]
  (let [width (apply max (map count rows)) height (count rows)
        image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        colors {\d (Color. 55 98 39) \m (Color. 91 130 49) \l (Color. 144 167 76)}]
    (doseq [[y row] (map-indexed vector rows) [x pixel] (map-indexed vector row)
            :let [color (colors pixel)] :when color]
      (.setRGB image x y (.getRGB ^Color color)))
    (scale-image image (* 2 width) (* 2 height))))

(def grass-sprites (into {} (for [[kind rows] grass-patterns] [kind (make-grass-sprite rows)])))

(defn grass-for-cell
  "Pure client-side decoration, independent of world state and generation order."
  [[x y :as position]]
  (if (or (not (config/in-bounds? position)) (layout/house-clearance? position)
          (not= :grass (landscape/terrain position))
          (not= [1 1 1 1] (landscape/shore-masks position))) []
    (let [random (java.util.Random. (long (+ config/world-seed 734111 (* x 73856093) (* y 19349663))))
          ^BufferedImage road-mask @road-mask-image
          on-road? (fn [px py]
                     (if road-mask
                       (and (<= 0 px (dec world-pixel-width)) (<= 0 py (dec world-pixel-height))
                            (pos? (.getSample (.getRaster road-mask) (quot px 2) (quot py 2) 0)))
                       (when-let [^BufferedImage image (road-image [(quot px tile-size) (quot py tile-size)])]
                         (neg? (.getRGB image (mod px tile-size) (mod py tile-size))))))
          density (cond (layout/clearing? position) 0.92 (layout/forest? position) 0.44 :else 0.67)]
      (vec
       (for [_ (range 9)
             :let [foot-x (+ (* x tile-size) 12 (.nextInt random 41))
                   foot-y (+ (* y tile-size) 18 (.nextInt random 41))
                   inside? (boolean (on-road? foot-x foot-y))
                   edge? (some #(not= inside? (boolean (on-road? (+ foot-x (first %)) (+ foot-y (second %)))))
                               [[-9 0] [9 0] [0 -9] [0 9]])
                   chance (cond (and inside? edge?) 0.07 inside? 0.004 edge? 0.96 :else density)
                   roll (.nextDouble random)
                   variant (.nextInt random 4)
                   kind (if inside? :short (case variant 0 :tall 1 :clump :short))
                   image ^BufferedImage (grass-sprites kind)]
             :when (< roll chance)]
         {:kind kind :cell position :x (- foot-x (quot (.getWidth image) 2))
          :y (- foot-y (.getHeight image)) :width (.getWidth image) :height (.getHeight image)
          :on-dirt? inside? :edge? (boolean edge?)})))))

;; Ограниченный числом клеток мира кэш; камера запрашивает только видимые клетки.
(def cached-grass-for-cell (memoize grass-for-cell))

(defn visible-grass [objects camera]
  (let [occupied (into #{} (mapcat (fn [p] (when-let [o (objects p)] (buildings/cells p o))))
                       (visible-cells camera visual-culling-margin))]
   (for [position (visible-cells camera 0)
        :when (not (occupied position))
        patch (cached-grass-for-cell position)
        :when (intersects? camera patch)] patch)))

(defn- draw-grass! [graphics objects camera]
  (doseq [{:keys [kind x y]} (visible-grass objects camera)]
    (.drawImage graphics ^BufferedImage (grass-sprites kind) (int x) (int y) nil)))

(defn sprite-kind [[x y] object]
  (let [alternate? (odd? (+ x y))
        biome (landscape/biome [x y])
        variant (mod (+ (* x 17) (* y 31)) 7)]
    (case object
      :tree (case biome
              :conifer (nth [:tree :tree-tall :tree :tree-tall :tree-small :tree :tree-tall] variant)
              :deciduous (nth [:tree-small :broadleaf-tree :broadleaf-tree :autumn-tree :dead-tree
                               :tree-small :broadleaf-tree] variant)
              (nth [:tree-small :tree-small :broadleaf-tree :autumn-tree :dead-tree :tree :tree-tall] variant))
      :stone (cond (and (= :swamp biome) alternate?) :mossy-rock
                   alternate? :stone-alt
                   :else :stone)
      object)))

(defn- place-image [position kind ^BufferedImage image foot-x foot-y clamp?]
  (let [width (.getWidth image)
        height (.getHeight image)
        left (int (- foot-x (quot width 2)))
        top (- foot-y height)
        x (if clamp? (max 0 (min left (- world-pixel-width width))) left)
        y (if clamp? (max 0 (min top (- world-pixel-height height))) top)]
    {:position position :kind kind :image image :x x :y y
     :width width :height height :foot-y (+ y height)
     :visual-cells (:visual-cells (graphic-specs kind))}))

(def ^:private tree-ages
  (when v5/enabled?
    (into {} (for [kind [:tree :tree-tall :tree-small :broadleaf-tree :autumn-tree]
                   :let [^BufferedImage image (sprites kind)]]
               [kind (mapv #(scale-image image (* % (.getWidth image)) (* % (.getHeight image)))
                           [0.78 0.9 1 1.08 1.19])]))))

(defn- place-sprite [[x y :as position] kind resource-tree?]
  (let [[landmark width height composition] (landscape/landmarks position)
        [width height] (cond
                         (= landmark kind) [width height]
                         (and (= :house kind) (layout/house-positions position)) [2 1]
                         :else [1 1])]
    (place-image position kind (if (and composition (= kind landmark))
                                  (composition-image composition)
                                  (if-let [variants (prop-variants kind)]
                                  (nth variants (mod (hash position) (count variants)))
                                  (if (and v5/enabled? resource-tree? (tree-ages kind))
                                    (get (tree-ages kind) (mod (hash position) 5))
                                    (sprites kind))))
                 (+ (* x tile-size) (quot (* width tile-size) 2))
                 (- (* (+ y height) tile-size) 8)
                 true)))

(defn sprite-placement [position kind]
  (place-sprite position kind false))

(defn- visual-cells-touched [{:keys [x y width height]}]
  (for [cx (range (quot x tile-size) (inc (quot (+ x width -1) tile-size)))
        cy (range (quot y tile-size) (inc (quot (+ y height -1) tile-size)))]
    [cx cy]))

(defn- scenery-placement [position kind occupied]
  (when (sprites kind)
    (let [placement (sprite-placement position kind)]
      (when-not (some #(or (occupied %) (buildings/protected-cells %))
                      (visual-cells-touched placement))
        placement))))

(def default-scenery
  (let [occupied (buildings/occupied-cells layout/initial-objects)
        sites [[:north-rocks :large-boulder] [:east-rocks :large-boulder]
               [:south-rocks :large-boulder] [:east-forest :tree]
               [:southwest-forest :tree-small]]]
    (:scenery
     (reduce (fn [{:keys [blocked] :as state} [zone kind]]
               (let [[x y] (layout/zone-anchors zone)
                     candidates (sort-by (fn [[cx cy]] [(+ (abs (- cx x)) (abs (- cy y))) cx cy])
                                         (for [cx (range (- x 5) (+ x 6))
                                               cy (range (- y 5) (+ y 6))
                                               :when (config/in-bounds? [cx cy])]
                                           [cx cy]))]
                 (if-let [placement (some #(scenery-placement % kind blocked) candidates)]
                   (-> state
                       (assoc-in [:scenery (:position placement)] kind)
                       (update :blocked into (visual-cells-touched placement)))
                   state)))
             {:scenery {} :blocked occupied}
             sites))))

(def game-scenery
  (cond-> default-scenery
    relief/main-world-options (assoc :terrain-relief relief/main-world-options)
    forest/world-options (into forest/world-options)
    config/world-v7?
    (assoc :world-art true :water-study :world :water-revision :world)))

(defn building-placement
  ([position entry] (building-placement position entry true))
  ([position entry clamp?]
   (let [[x y] position [width height] (get-in buildings/catalog [(:kind entry) :size])
        kind (if (= :house (:kind entry)) :built-house (:kind entry))
        image (case kind
                :fence (get fence-images (:connections entry 0))
                :built-house (sprites (case (:team entry) :blue :blue-house :red :red-house :built-house))
                (sprites kind))
        foot-x (+ (* x tile-size) (quot (* width tile-size) 2))
        foot-y (- (* (+ y height) tile-size) 8)]
    (assoc (place-image position kind image foot-x foot-y clamp?)
           :building? (not= kind :fence) :damage-tier (effects/damage-tier entry)))))

(defn- nearby-ridges [camera]
  (for [[[x y :as p] [_ width height]] v6/ridges
        :let [foot-y (- (* (+ y height) tile-size) 8)]
        :when (and (landscape/landmarks p)
                   (intersects? camera {:x (* x tile-size) :y (- foot-y 520)
                                   :width (* width tile-size) :height 520}))]
    p))

(def ^:private mountain-front-image
  (memoize #(load-resource-image (str mountain/image-root (name %) ".png"))))

(defn- mountain-front [camera]
  (when mountain/redesigned?
    (let [[ox oy] (map #(* tile-size %) (:origin mountain/model))]
      (for [{:keys [x y foot-y asset] :as part} (:front mountain/art)
            :let [placement (assoc part :x (+ ox x) :y (+ oy y) :foot-y (+ oy foot-y))]
            :when (intersects? camera placement)]
        (assoc placement :image (mountain-front-image asset))))))

(def ^:private canyon-image
  (memoize #(load-resource-image (str canyon/image-root % ".png"))))

(def ^:private canyon-front-image
  (memoize (fn [asset width height]
             (scale-image (canyon-image (name asset)) width height))))

(defn- canyon-front [camera]
  (when canyon/enabled?
    (let [[ox oy] (map #(* tile-size %) (:origin canyon/model))]
      (for [{:keys [x y foot-y asset width height] :as part} (:front @canyon/art)
            :let [placement (assoc part :x (+ ox x) :y (+ oy y) :foot-y (+ oy foot-y))]
            :when (and (not= :canyon-bridge (:kind part)) (intersects? camera placement))]
        (assoc placement :image (canyon-front-image asset width height))))))

(defn- mountain-prop [position kind]
  (if (and mountain/redesigned? (mountain/props position))
    (case kind
      (:tent :raider-tent) nil
      :cairn :campfire
      kind)
    kind))

(defn scene-sprites
  "Scenery maps visual-only anchors to graphic kinds and is kept outside the world objects."
  ([objects camera] (scene-sprites objects camera {}))
  ([objects camera scenery]
   (let [positions (if v6/enabled?
                     (distinct (concat (visible-cells camera visual-culling-margin)
                                       (nearby-ridges camera)))
                     (visible-cells camera visual-culling-margin))
         occupied (into #{} (mapcat (fn [p] (when-let [o (objects p)] (buildings/cells p o)))) positions)]
     (->> (concat (relief/foregrounds scenery camera)
                  (water-study/foregrounds scenery camera)
                  (remove #(relief/replaces-rock? scenery %)
                          (concat (mountain-front camera) (canyon-front camera)))
                  (for [position positions
                :let [object (get objects position)
                      placement (cond
                                  (buildings/building? object)
                                  (building-placement position (if (= :fence (:kind object))
                                                                (assoc object :connections (fence-connections objects position)) object))
                                  object (when-let [kind (sprite-kind position object)]
                                           (when (sprites kind) (place-sprite position kind (= :tree object))))
                                  (not (occupied position))
                                  (or (when-let [kind (get scenery position)]
                                        (scenery-placement position kind occupied))
                                      (when-let [kind (mountain-prop position (get decorations position))]
                                        (when-not (and v5/enabled? (v5/ground-props kind))
                                          (sprite-placement position kind)))))]
                :when placement] placement))
          (map #(relief/finish-prop scenery %))
          (map #(world-props/decorate scenery objects %))
          (map #(forest/decorate scenery %))
          (map #(world-terrain/decorate scenery %))
          (filter #(intersects? camera %))
          (sort-by (juxt :foot-y :x :kind))))))

(def ^:private bridge-source
  (delay
    (let [source (load-resource-image (visual-path :buildings "bridge"))
          ;; Cancel the deck's vertical rise while keeping the posts upright.
          shift #(Math/round (* 0.43 %))
          [left top right bottom]
          (reduce (fn [[left top right bottom] [x y]]
                    [(min left x) (min top y) (max right x) (max bottom y)])
                  [Integer/MAX_VALUE Integer/MAX_VALUE 0 0]
                  (for [x (range (.getWidth source)) y (range (.getHeight source))
                        :when (pos? (unsigned-bit-shift-right (.getRGB source x y) 24))]
                    [x (+ y (shift x))]))
          margin 4
          flatten (BufferedImage. (+ 1 (- right left) (* 2 margin))
                                  (+ 1 (- bottom top) (* 2 margin))
                                  BufferedImage/TYPE_INT_ARGB)
          graphics (.createGraphics flatten)]
      (try
        (doseq [x (range left (inc right))
                :let [u (+ margin (- x left)) y (+ margin (- (shift x) top))
                      h (.getHeight source)]]
          (.drawImage graphics source u y (inc u) (+ y h) x 0 (inc x) h nil))
        (finally (.dispose graphics)))
      flatten)))

(def ^:private falls
  (delay
    (let [sill (load-resource-image (v41-path "fall_sill"))
          rockbeds (when v5/enabled? (mapv #(load-resource-image (v5/path :cascade %)) (range 4)))
          flows (mapv #(load-resource-image (v41-path (str "fall_flow_" %))) (range 6))]
      (mapv (fn [row]
              (let [center (long (Math/floor (landscape/river-center (+ row 0.5))))
                    west (first (remove #(landscape/water-blocks [% row]) (iterate dec center)))
                    east (first (remove #(landscape/water-blocks [% row]) (iterate inc center)))
                    width (* (+ 2 (- east west)) tile-size)
                    height 156]
                {:x (- (* west tile-size) tile-size) :y (- (* row tile-size) 76)
                 :width width :height height :image sill :flows flows
                 :rockbed (when rockbeds (nth rockbeds (mod row (count rockbeds))))}))
            landscape/waterfall-rows))))

(defn- flat-bridge [west east y stone?]
  (let [width (- (* (inc (- east west)) tile-size) 38)
        image (BufferedImage. width 43 BufferedImage/TYPE_INT_ARGB)
        ^Graphics2D g (.createGraphics image)]
    (try
      (.setColor g (Color. 34 39 29 100))
      (.fillRect g 3 10 (- width 6) 30)
      (doseq [x (range 4 (- width 4) 8)
              :let [w (min 7 (- width x 4)) offset (mod (hash x) 5)]]
        (.setColor g (if stone? (Color. 75 74 70) (Color. 86 65 43)))
        (.fillRect g x (+ 6 offset) w 28)
        (.setColor g (if stone? (Color. (+ 132 offset) (+ 127 offset) 114) (Color. (+ 133 offset) (+ 100 offset) 64)))
        (.fillRect g x (+ 5 offset) (- w 1) 24)
        (.setColor g (if stone? (Color. 158 151 133) (Color. 166 126 78)))
        (.fillRect g x (+ 5 offset) 2 24)
        (.setColor g (Color. 62 53 36))
        (.fillRect g (+ x 3) (+ 8 offset) 2 2)
        (.fillRect g (+ x 3) (+ 25 offset) 2 2))
      (finally (.dispose g)))
    {:image image :x (+ (* west tile-size) 19) :y (+ (* y tile-size) 7)
     :width width :height (.getHeight image)}))

(def ^:private water-scenery
  (delay
    (let [bridges (for [[[_ y] {:keys [west east marsh?]}] landscape/bridges
                        :let [width (- (* (inc (- east west)) tile-size) 20)
                              image (scale-image @bridge-source width Integer/MAX_VALUE)]]
                    (if (or marsh? (and v6/enabled? (#{36 58 83} y)))
                      (flat-bridge west east y (not marsh?))
                      {:image image :x (+ (* west tile-size) 10)
                       :y (- (+ (* y tile-size) 27) (quot (.getHeight image) 2))
                       :width (.getWidth image) :height (.getHeight image)}))
          decks (when-not v5/enabled? (for [[ox oy] [[63 51] [65 54] [68 55] [71 59]]
                      :let [[[bx by] [x y] :as landing]
                            (first (sort-by (fn [[[x y] _]]
                                              [(+ (abs (- x ox)) (abs (- y oy))) x y])
                                            (for [dx (range -2 3) dy (range -2 3)
                                                  :let [bank [(+ ox dx) (+ oy dy)]]
                                                  :when (not (or (landscape/wet-cells bank)
                                                                 (layout/initial-objects bank)))
                                                  p (layout/neighbors bank)
                                                  :when (landscape/water-blocks p)]
                                              [bank p])))]
                      :when landing
                      :let [deck (first (prop-variants :boardwalk))
                            cx (* (+ bx 0.5 (* 0.27 (- x bx))) tile-size)
                            cy (* (+ by 0.5 (* 0.27 (- y by))) tile-size)
                            width (if (= x bx) 80 112)
                            height (if (= x bx) 126 108)]]
                  {:image deck :x (long (- cx (quot width 2))) :y (long (- cy (quot height 2)))
                   :width width :height height}))
          islets (for [[x y :as p] [[46 72] [92 54] [8 48] [70 47]]
                       :when (and (landscape/water-blocks p)
                                  (every? landscape/water-blocks (layout/neighbors p)))
                       :let [variants (prop-variants :islet)
                             image (nth variants (mod (hash p) (count variants)))]]
                   {:image image :x (- (* x tile-size) 32) :y (- (* y tile-size) 36)
                    :width (.getWidth image) :height (.getHeight image)})]
      (concat islets decks bridges @falls))))

(defn- draw-water-scenery! [graphics camera]
  (doseq [{:keys [image x y width height rockbed] :as scenery} @water-scenery :when (intersects? camera scenery)]
    (.drawImage graphics ^BufferedImage image (int x) (int y) (int width) (int height) nil)
    (when rockbed
      (.drawImage graphics ^BufferedImage rockbed (int x) (int y) (int width) (int height) nil))))

(defn water-visible? [camera]
  (and camera (or (some #(intersects? camera %) @falls)
                  (some landscape/water-blocks (visible-cells camera 0)))))

(def ^:private marsh-haze
  (delay (when v5/enabled?
           (mapv (fn [i [x y]]
                   {:x (* x tile-size) :y (* y tile-size) :width 232 :height 110
                    :image (load-resource-image (v5/path :marsh-haze i))})
                 (range) [[63 51] [66 55] [68 57] [70 59]]))))

(defn- ripples-in [camera]
  (into []
        (keep (fn [[x y :as p]]
                (when (and (landscape/water-blocks p)
                           (or (not (lake/local-cell? p))
                               (every? landscape/water-blocks (layout/neighbors p)))
                           (not= :ice-water (regional-water p))
                           (< (reduce + (landscape/shore-masks p)) 2)
                           (zero? (mod (+ (* 23 x) y) 3)))
                  {:x (* x tile-size) :y (* y tile-size) :width tile-size :height tile-size
                   :offset-x (* 7 y) :offset-y (* 11 x)
                   :river? (< (abs (- x (landscape/river-center y))) 3)})))
        (visible-cells camera 0)))

(def ^:private legacy-ripple-color (Color. 162 213 217 70))

(defn- draw-water-motion! [^Graphics2D graphics camera phase ripples scenery]
  (.setColor graphics legacy-ripple-color)
  (doseq [{:keys [x y offset-x offset-y river?] :as ripple} ripples
          :when (and (intersects? camera ripple) (not (water-study/replaces-motion? scenery ripple)))]
    (let [px (+ x 8 (mod (+ offset-x (if river? (quot phase 5) phase)) 38))
          py (+ y 14 (mod (+ offset-y (* phase (if river? 3 1))) 36))]
      (.fillRect graphics px py 12 2)
      (.fillRect graphics (+ px 5) (+ py 5) 7 1)))
  (doseq [{:keys [flows x y width height] :as fall} @falls
          :when (and (intersects? camera fall) (not (water-study/replaces-motion? scenery fall)))]
    (.drawImage graphics ^BufferedImage (nth flows (mod phase (count flows)))
                (int x) (int y) (int width) (int height) nil))
  (doseq [{:keys [image x y width height] :as haze} @marsh-haze :when (intersects? camera haze)]
    (.drawImage graphics ^BufferedImage image (+ (int x) (int (* 8 (Math/sin (/ phase 15.0)))))
                    (int y) nil))
  (water-study/draw-motion! graphics camera scenery phase))

(def ^:private character-prefixes {:blue "player" :red "bot" :bandits "bandit"})
(def ^:private directions [:up :down :left :right])

(def ^:private character-sprites
  (into {}
        (for [[team prefix] character-prefixes, direction directions, frame (range 3)
              :let [path (str "images/characters/" prefix "_" (name direction) "_" frame ".png")
                    image (load-resource-image path)]]
          [[team direction frame] (if (= tile-size (.getWidth image) (.getHeight image))
                                    image
                                    (scale-image image tile-size tile-size))])))

(defn character-sprite [team direction frame]
  (get character-sprites [team direction frame]))

(defn- bridge-lift [position]
  (if (landscape/bridge-cells position) (quot tile-size 4) 0))

(defn- position-sample [[x y :as position]]
  [x y (bridge-lift position)])

(defn- interpolate [from to progress]
  (if (zero? progress)
    from
    (mapv (fn [a b] (+ a (* progress (- b a)))) from to)))

(defn- active-steps [steps now]
  (vec (drop-while #(<= (:end %) now) steps)))

(defn- step-position [{:keys [from to start end]} now]
  (interpolate from to (max 0.0 (/ (- now start) (double (- end start))))))

(defn- limit-motion-delay [steps now]
  (let [duration (- (:end (peek steps)) now)
        limit (* 2 config/move-animation-ms)]
    (if (<= duration limit)
      steps
      ;; Keep the current point and each confirmed corner, while compressing
      ;; the remaining timeline after a burst of packets.
      (let [shift (fn [time] (+ now (* (/ (- time now) duration) limit)))]
        (mapv (fn [index {:keys [start end] :as step}]
                (if (zero? index)
                  (assoc step :from (step-position step now) :start now :end (shift end))
                  (assoc step :start (shift start) :end (shift end))))
              (range) steps)))))

(defn update-character-motion [previous players now]
  (into {}
        (for [[id {:keys [x y hp] facing :direction}] players
              :let [old (get previous id)
                    target [x y]
                    [old-x old-y] (:target old target)
                    dx (- x old-x) dy (- y old-y)
                    direction (get {[1 0] :right [-1 0] :left [0 1] :down [0 -1] :up}
                                   [dx dy])
                    waiting (active-steps (:steps old) now)
                    hit-until (if (and old hp (< hp (:hp old (:hp config/player-combat))))
                                (+ now 420) (:hit-until old 0))
                    motion (cond
                direction
                (let [start (or (:end (peek waiting)) now)
                      step {:from (position-sample [old-x old-y]) :to (position-sample target)
                            :direction direction :start start :end (+ start config/move-animation-ms)}]
                  {:target target :direction direction :began (if (seq waiting) (:began old) now)
                   :steps (limit-motion-delay (conj waiting step) now)})
                (= target (:target old)) (assoc old :steps waiting)
                :else {:target target :direction (or facing (:direction old) :down) :steps []})]]
          [id (cond-> (assoc motion :hit-until hit-until)
                hp (assoc :hp hp))])))

(defn visual-players [players motion now]
  (into {}
        (for [[id player] players
              :let [step (first (active-steps (get-in motion [id :steps]) now))
                    hit? (< now (get-in motion [id :hit-until] 0))]]
          [id (cond-> (if step
                (let [[x y lift] (step-position step now)]
                  (assoc player :x x :y y :bridge-lift lift))
                player)
                hit? (assoc :hit? true))])))

(defn moving? [motion now]
  (some #(or (< now (:hit-until % 0)) (seq (active-steps (:steps %) now))) (vals motion)))

(defn character-poses [motion now]
  (into {}
        (for [[id {:keys [direction began steps]}] motion
              :let [step (first (active-steps steps now))
                    frame-ms (/ config/move-animation-ms 3.0)
                    frame (if step (mod (inc (long (/ (- now began) frame-ms))) 3) 0)]]
          [id {:direction (:direction step direction) :frame frame}])))

(defn player-markers [players]
  (mapcat
   (fn [[[x y] occupants]]
     (let [occupants (sort-by (comp str key) occupants)
           count-players (count occupants)]
       (map-indexed
        (fn [index [id player]]
          {:id id
           :x (+ (* x tile-size) (quot tile-size 2)
                 (int (* 34 (- index (/ (dec count-players) 2.0)))))
           :y (+ (* y tile-size) 37)
           :foot-y (- (* (inc y) tile-size)
                      (:bridge-lift player (bridge-lift [x y])))
           :team (:team player)
           :hp (:hp player) :max-hp (:max-hp player) :hit? (:hit? player)
           :label (or (:name player) (str id))})
        occupants)))
   (sort-by key (group-by (fn [[_ player]] [(:x player) (:y player)]) players))))

(defn- draw-legacy-marker! [graphics x y]
  (.setColor graphics (Color. 27 37 30))
  (.fillOval graphics (- x 18) (- y 18) 36 36)
  (.setColor graphics Color/WHITE)
  (.fillOval graphics (- x 16) (- y 16) 32 32)
  (.setColor graphics (Color. 110 116 109))
  (.fillOval graphics (- x 14) (- y 14) 28 28))

(defn- draw-player! [graphics camera poses {:keys [id x y foot-y team label hp max-hp hit?]}]
  (let [{:keys [direction frame]} (get poses id {:direction :down :frame 0})
        sprite ^BufferedImage (character-sprite team direction frame)
        width (if sprite (.getWidth sprite) 36)
        height (if sprite (.getHeight sprite) 36)
        left (- x (quot width 2))
        top (if sprite (- foot-y height) (- y 18))]
    (when (intersects? camera {:x left :y top :width width :height height})
      (if sprite
        (.drawImage graphics sprite (Math/round (double left)) (Math/round (double top)) nil)
        (draw-legacy-marker! graphics (Math/round (double x)) (Math/round (double y))))
      (let [metrics (.getFontMetrics graphics)
            label-width (.stringWidth metrics label)
            label-x (max (:x camera)
                         (min (- x (quot label-width 2))
                              (- (+ (:x camera) (:width camera)) label-width)))
            above (+ top (if (= :bandits team) -7 3))
            label-y (if (< (- above (.getAscent metrics)) (:y camera))
                      (+ top height (.getAscent metrics))
                      above)]
        (when (:names @theme/settings)
          (.setColor graphics (Color. 27 37 30))
          (.drawString graphics label (int (inc label-x)) (int (inc label-y)))
          (.setColor graphics Color/WHITE)
          (.drawString graphics label (int label-x) (int label-y)))
        (when (= :bandits team)
          (let [bar-width 34 bar-x (- (int x) 17)
                bar-y (+ (int label-y) 3)]
            (.setColor graphics (Color. 23 20 29))
            (.fillRect graphics bar-x bar-y bar-width 5)
            (.setColor graphics (if hit? (Color. 249 121 108) (Color. 174 105 208)))
            (.fillRect graphics (inc bar-x) (inc bar-y) (int (* 32 (/ hp max-hp))) 3)))
        (when hit?
          (.setColor graphics (Color. 255 102 105))
          (.fillRect graphics (+ (int x) 13) (+ (int top) 13) 10 2)
          (.fillRect graphics (+ (int x) 17) (+ (int top) 9) 2 10))))))

(defn- viewport-transform! [^Graphics2D graphics camera]
  (.setRenderingHint graphics RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
  (let [[x y] (raster-origin camera)]
    (.translate graphics (int (- x)) (int (- y))))
  (.clipRect graphics 0 0 world-pixel-width world-pixel-height))

(defn- draw-entrances! [^Graphics2D graphics objects camera]
  (doseq [[x y :as anchor] (visible-cells camera 2)
          :let [entry (objects anchor)
                [w h] (if (buildings/building? entry)
                        (get-in buildings/catalog [(:kind entry) :size])
                        (when (= entry :house) [2 1]))]
          :when (and w (> w 1))
          :let [[ex ey :as p] [(+ x (dec w)) (+ y h)]
                sides [[ex (dec ey)] [(inc ex) ey] [ex (inc ey)] [(dec ex) ey]]
                joins (reduce + (keep-indexed (fn [i side] (when (layout/road-cells side) (bit-shift-left 1 i)))
                                              sides))]
          :when (and (config/in-bounds? p) (not (objects p))
                     (not (landscape/wet-cells p)) (not (layout/road-cells p)) (pos? joins))]
    (.drawImage graphics ^BufferedImage (get-in road-assets [:stone (bit-or 1 joins)])
                (* (first p) tile-size) (* (second p) tile-size) nil)))

(def ^:private saddle-assets
  (mapv #(load-resource-image (v41-path (str "saddle_" %))) (range 4)))

(defn- draw-ridge-saddles! [^Graphics2D graphics objects camera]
  (let [anchors (filter (fn [[p [kind]]] (and (ridge-kinds kind) (= kind (objects p))))
                        landscape/landmarks)]
    (doseq [[[ax ay :as a] [_ aw ah]] anchors
            [[bx by :as b] [_ bw bh]] anchors
            :when (and (neg? (compare a b)) (<= (+ (abs (- ax bx)) (abs (- ay by))) 5))
            :let [center-x (* tile-size (/ (+ ax bx (/ (+ aw bw) 2.0)) 2.0))
                  center-y (* tile-size (/ (+ ay by ah bh) 2.0))
                  x (- (int center-x) 125) y (- (int center-y) 134)
                  area {:x x :y y :width 256 :height 144}]
            :when (intersects? camera area)]
      (.drawImage graphics ^BufferedImage (nth saddle-assets (mod (hash [a b]) 4)) x y nil))))

(defn- draw-ground-props! [^Graphics2D graphics objects camera]
  (doseq [p (visible-cells camera 4)
          :let [kind (decorations p)]
          :when (and (v5/ground-props kind) (not (objects p)))
          :let [{:keys [x y image] :as placement} (sprite-placement p kind)]
          :when (intersects? camera placement)]
    ;; Preserve the viewport strip clip while keeping decals clear of the road and bank.
    (doseq [[cx cy :as cell] (visual-cells-touched placement)
            :when (and (config/in-bounds? cell) (not (layout/road-cells cell))
                       (not (landscape/wet-cells cell)))]
      (let [g ^Graphics2D (.create graphics)]
        (try
          (.clipRect g (* cx tile-size) (* cy tile-size) tile-size tile-size)
          (.drawImage g ^BufferedImage image (int x) (int y) nil)
          (finally (.dispose g)))))))

(def ^:private regional-decals
  (delay
    (when v6/enabled?
      (mapv (fn [[cell file]]
              (let [[x y] (map #(* tile-size %) cell)
                    ^BufferedImage image (composition-image file)]
                {:x x :y y :width (.getWidth image) :height (.getHeight image) :image image}))
            (cond-> [[[31 4] "forest-mat"] [[5 6] "forest-mat"] [[9 32] "forest-mat"]
                     [[18 81] "snow-drifts"] [[26 85] "snow-drifts"]]
              (not canyon/enabled?) (into [[[81 84] "dune-basin"] [[85 66] "dune-north"]
                                           [[80 88] "desert-court"]]))))))

(defn- draw-regional-decals! [^Graphics2D graphics camera]
  (doseq [{:keys [x y image] :as decal} @regional-decals
          :when (intersects? camera decal)
          [cx cy :as p] (visual-cells-touched decal)
          :when (and (config/in-bounds? p) (not (layout/road-cells p))
                     (not (landscape/wet-cells p)))
          :let [g ^Graphics2D (.create graphics)]]
    (try
      (.clipRect g (* cx tile-size) (* cy tile-size) tile-size tile-size)
      (.drawImage g ^BufferedImage image (int x) (int y) nil)
      (finally (.dispose g)))))

(def ^:private mountain-chunk
  (memoize
   (fn [layer x y]
     (let [^BufferedImage source (load-resource-image (str mountain/image-root (when layer (str (name layer) "_")) x "_" y ".png"))
           scale (:scale mountain/art 1)
           ^BufferedImage image (if (= scale 1)
                                  source
                                  (let [image (BufferedImage. (quot (.getWidth source) scale)
                                                              (quot (.getHeight source) scale)
                                                              BufferedImage/TYPE_INT_ARGB)
                                        graphics (.createGraphics image)]
                                    (try
                                      (.setRenderingHint graphics RenderingHints/KEY_INTERPOLATION
                                                         RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
                                      (.drawImage graphics source 0 0 (.getWidth image) (.getHeight image) nil)
                                      (finally (.dispose graphics)))
                                    image))
           [ox oy] (map #(* tile-size %) (:origin mountain/model))
           left (+ ox x) top (+ oy y)
           view {:x left :y top :width (.getWidth image) :height (.getHeight image)}
           wet (filter landscape/wet-cells (visible-cells view 0))]
       (when (seq wet)
         (let [graphics (.createGraphics image)]
           (try
             (.setComposite graphics AlphaComposite/Clear)
             (doseq [[cx cy :as p] wet :when (not (lake/local-cell? p))]
               (.fillRect graphics (- (* cx tile-size) left)
                          (- (* cy tile-size) top) tile-size tile-size))
             (.setComposite graphics AlphaComposite/DstOut)
             (doseq [[cx cy :as p] wet
                     :let [^BufferedImage cut (:cut (lake/fragment p))]
                     :when cut]
               (.drawImage graphics cut (- (* cx tile-size) left)
                           (- (* cy tile-size) top) nil))
             (finally (.dispose graphics)))))
       image))))

(defn- draw-mountain! [^Graphics2D graphics camera]
  (when mountain/enabled?
    (let [[ox oy] (map #(* tile-size %) (:origin mountain/model))]
      (doseq [[layer chunks] (if mountain/redesigned?
                               [[:base (:base mountain/art)] [:tread (:tread mountain/art)]]
                               [[nil (:chunks mountain/model)]])
              [u v width height] chunks
              :let [x (+ ox u) y (+ oy v)]
              :when (intersects? camera {:x x :y y :width width :height height})]
        (.drawImage graphics ^BufferedImage (mountain-chunk layer u v) (int x) (int y) nil)))))

(def ^:private canyon-chunk
  (memoize
   (fn [layer u v w h]
     (let [image (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
           g (.createGraphics image)
           [ox oy] (map #(* tile-size %) (:origin canyon/model))
           left (+ ox u) top (+ oy v)]
       (try
         (.setRenderingHint g RenderingHints/KEY_INTERPOLATION
                            RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
         (.drawImage g ^BufferedImage (canyon-image (str (name layer) "_" u "_" v)) 0 0 w h nil)
         (.setComposite g AlphaComposite/Clear)
         (doseq [[x y :as p] (visible-cells {:x left :y top :width w :height h} 0)
                 :when (landscape/wet-cells p)]
           (.fillRect g (- (* x tile-size) left) (- (* y tile-size) top) tile-size tile-size))
         (finally (.dispose g)))
       image))))

(defn- draw-canyon-bridges! [^Graphics2D graphics camera]
  (let [[ox oy] (map #(* tile-size %) (:origin canyon/model))]
    (doseq [{:keys [x y width height kind asset]} (:front @canyon/art)
            :let [placement {:x (+ ox x) :y (+ oy y) :width width :height height}]
            :when (and (= :canyon-bridge kind) (intersects? camera placement))]
      (.drawImage graphics ^BufferedImage (canyon-image (name asset))
                  (int (:x placement)) (int (:y placement)) (int width) (int height) nil))))

(defn- draw-canyon! [^Graphics2D graphics camera]
  (when canyon/enabled?
    (let [[ox oy] (map #(* tile-size %) (:origin canyon/model))]
      (doseq [layer [:base :floor]
              [u v w h] (layer @canyon/art)
              :let [x (+ ox u) y (+ oy v)]
              :when (intersects? camera {:x x :y y :width w :height h})]
        (.drawImage graphics ^BufferedImage (canyon-chunk layer u v w h) (int x) (int y) nil))
      (draw-canyon-bridges! graphics camera))))

(def ^:private lake-ground-tile
  (memoize
   (fn [[x _ :as p]]
     (let [^BufferedImage land (detailed-ground p)
           ^BufferedImage original (cached-terrain p)
           image (BufferedImage. tile-size tile-size BufferedImage/TYPE_INT_RGB)]
       (doseq [u (range tile-size)
               :let [weight (int (Math/round (* 8.0 (lake/blend-strength (+ (* x tile-size) u)))))]
               v (range tile-size)]
         (.setRGB image u v (mix-ground-rgb (.getRGB original u v) (.getRGB land u v) weight)))
       image))))

(defn draw-ground-layers! [graphics objects camera]
  (let [[left top right bottom] lake/bounds
        lake-view {:x (* left tile-size) :y (* top tile-size)
                   :width (* (- right left) tile-size) :height (* (- bottom top) tile-size)}
        shore-cells (when (and lake/enabled? (intersects? camera lake-view))
                      (filterv lake/affected-cell? (visible-cells camera 0)))]
    (draw-terrain! graphics camera)
    (doseq [[x y :as p] shore-cells :when (lake/repaint-ground? p)
            :let [px (* x tile-size) py (* y tile-size)]]
      (.drawImage graphics ^BufferedImage (lake-ground-tile p) px py nil)
      (when-let [road (when-not (mountain/route-cells p) (road-image p))]
        (.drawImage graphics ^BufferedImage road px py nil)))
    (draw-mountain! graphics camera)
    (draw-canyon! graphics camera)
    (if v6/enabled? (draw-regional-decals! graphics camera)
        (draw-ridge-saddles! graphics objects camera))
    (if v5/enabled? (draw-ground-props! graphics objects camera) (draw-entrances! graphics objects camera))
    (draw-grass! graphics objects camera)
    (doseq [[x y :as p] shore-cells
            :let [^BufferedImage shore (:image (lake/fragment p))]]
      (.drawImage graphics shore (* x tile-size) (* y tile-size) nil))
    (draw-water-scenery! graphics camera)))

(def ^:private baked-ground?
  (delay (and v6/enabled? (some? (io/resource (terrain-chunk-path :ground 0 0))))))

(defn- draw-ground! [graphics objects camera]
  (if (and @baked-ground? (= layout/initial-objects objects))
    (draw-chunks! graphics camera :ground #(draw-ground-layers! %1 objects %2))
    (draw-ground-layers! graphics objects camera)))

(def ^:private study-bank
  (memoize
   (fn [[x y :as p] bounds]
     (let [mask (landscape/shore-masks p)
           ^BufferedImage land (detailed-ground p)
           pressed? (and (landscape/wet-cells p) (= [1 1 1 1] mask))
           patch (BufferedImage. tile-size tile-size BufferedImage/TYPE_INT_ARGB_PRE)]
       (doseq [v (range 0 tile-size 2) u (range 0 tile-size 2)
               :let [tx (/ (+ u 1.0) tile-size) ty (/ (+ v 1.0) tile-size)
                     wobble (* 0.055 (Math/sin (+ (* 0.23 (+ u (* x tile-size)))
                                                 (* 0.17 (+ v (* y tile-size))))))
                     dry (- (+ (blend-mask mask tx ty) wobble)
                            (if pressed? (* 0.74 (Math/pow (* 16 tx (- 1 tx) ty (- 1 ty)) 2)) 0))
                     edge (if bounds
                            (let [px (+ (* x tile-size) u) py (+ (* y tile-size) v)
                                  bx (:x bounds) by (:y bounds)
                                  distance (min (- px bx) (- py by) (- (+ bx (:width bounds) -2) px)
                                                (- (+ by (:height bounds) -2) py))
                                  t (max 0.0 (min 1.0 (/ distance 84.0)))]
                              (* t t (- 3.0 (* 2.0 t))))
                            1.0)
                     alpha (int (* edge (max 0 (min 255 (* 1800 (- 0.74 dry))))))
                     rgb (bit-and 0xffffff (.getRGB land u v))]
               :when (pos? alpha)
               dv [0 1] du [0 1]]
         (.setRGB patch (+ u du) (+ v dv) (unchecked-int (bit-or (bit-shift-left alpha 24) rgb))))
       patch))))

(defn- draw-view-ground! [graphics objects camera scenery]
  (draw-ground! graphics objects camera)
  (world-terrain/draw! graphics camera scenery)
  (forest/draw-ground! graphics camera scenery)
  (when (and (contains? (relief/draw! graphics camera scenery) :canyon) canyon/enabled?)
    (when-not (relief/draw-decks! graphics camera scenery)
      (draw-canyon-bridges! graphics camera)))
  (world-props/draw-stairs! graphics camera scenery)
  (when (water-study/options scenery)
    (let [study (water-study/scene scenery)
          bridge-banks (water-study/bridge-banks scenery)]
      (doseq [[x y :as p] (water-study/restore-cells scenery camera)
              :let [px (* x tile-size) py (* y tile-size)]
              :when (intersects? camera {:x px :y py :width tile-size :height tile-size})]
        (let [bridge-bank? (or (contains? bridge-banks p)
                               (and (= :river (water-study/options scenery)) (= y 36)))]
          (.drawImage graphics ^BufferedImage (if bridge-bank?
                                              (detailed-ground p)
                                              (study-bank p (when (water-study/natural? scenery) (:bounds study))))
                      px py nil))
        (when-let [road (road-image p)]
          (.drawImage graphics ^BufferedImage road px py nil)))
      (water-study/draw! graphics camera scenery)
      (when (and study (water-study/global? scenery))
        (doseq [{:keys [x y width height image rockbed] :as fall} @falls
                :when (intersects? camera fall)]
          (.drawImage graphics ^BufferedImage (or rockbed image)
                      (int x) (int y) (int width) (int height) nil)))))
  (world-terrain/draw-water-plants! graphics camera scenery)
  (when (= :world (:forest-study scenery))
    (let [^Graphics2D g graphics
          ;; A stem can be just beyond an exposed strip while its shadow spans it.
          view (-> camera (update :x - 256) (update :y - 128)
                   (update :width + 512) (update :height + 256))]
      (doseq [{:keys [ground-shadow? forest-shadow]} (scene-sprites objects view scenery)
              :when ground-shadow?]
        (.drawImage g ^BufferedImage (:image forest-shadow)
                    (int (:x forest-shadow)) (int (:y forest-shadow)) nil)))))

(def ^:private canopy-kinds #{:tree :tree-tall :tree-small :broadleaf-tree :autumn-tree :frost-pine})
(def ^:private wind-kinds (into canopy-kinds [:tall-grass :reeds :rushes :forest-fern]))
(def ^:private ambient-kinds (into wind-kinds [:lantern :campfire :ember-pit :built-house :house]))

(def ^:private damaged-sprite
  (memoize
   (fn [^BufferedImage image tier]
     (let [w (.getWidth image) h (.getHeight image)
           out (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
           g (.createGraphics out)
           marks (take (case tier :light 1 :heavy 3 :critical 6 0)
                       [[0.48 0.57] [0.30 0.65] [0.69 0.66] [0.57 0.38] [0.72 0.51] [0.42 0.74]])]
       (try
         (.drawImage g image 0 0 nil)
         (.setComposite g AlphaComposite/SrcAtop)
         (doseq [[u v] marks
                 :let [x (int (* w u)) y (int (* h v))]]
           (.setColor g (Color. 43 32 28 218))
           (.drawLine g x y (+ x 5) (+ y 7))
           (.drawLine g (+ x 5) (+ y 7) (+ x 1) (+ y 12))
           (.drawLine g (+ x 5) (+ y 7) (+ x 12) (+ y 9))
           (.setColor g (Color. 211 171 121 190))
           (.drawLine g (inc x) y (+ x 6) (+ y 6))
           (when (= tier :critical)
             (.setColor g (Color. 46 34 28 120))
             (.fillRect g (- x 7) (+ y 6) 10 5)))
         (finally (.dispose g)))
       out))))

(def ^:private impact-image
  (memoize
   (fn [^BufferedImage image]
     (let [w (.getWidth image) h (.getHeight image)
           out (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
           g (.createGraphics out)]
       (try
         (.drawImage g image 0 0 nil)
         (.setComposite g AlphaComposite/SrcAtop)
         (.setColor g (Color. 255 103 74))
         (.fillRect g 0 0 w h)
         (finally (.dispose g)))
       out))))

(defn draw-building-impacts! [^Graphics2D graphics layers events camera ox oy now]
  (let [anchors (into #{} (keep #(when (and (= :damage (:kind %)) (< (- now (:at %)) 155))
                                   (:anchor %))) events)]
    (when (seq anchors)
      (let [^Graphics2D g (.create graphics)
            [cx cy] (raster-origin camera)]
        (try
          (.clipRect g ox oy (:width camera) (:height camera))
          (.setComposite g (AlphaComposite/getInstance AlphaComposite/SRC_OVER (float 0.31)))
          (doseq [{:keys [position image x y] :as layer} layers
                  :when (and (anchors position) (intersects? camera layer))]
            (.drawImage g ^BufferedImage (impact-image image) (+ ox (- x cx)) (+ oy (- y cy)) nil))
          (finally (.dispose g)))))))

(defn- ambient! [^Graphics2D g {:keys [kind x y width height foot-y]} phase]
  (let [time (+ phase (mod (+ x y) 47))
        cx (+ x (quot width 2))
        particles? (:particles @theme/settings)]
    (case kind
      (:ember-pit :campfire)
      (let [fy (- foot-y 24)]
        (.setColor g (Color. 254 185 70 205))
        (.fillRect g (+ cx (- (mod time 5) 3)) (- fy (mod time 8)) 5 8)
        (.setColor g (Color. 255 235 146))
        (.fillRect g (- cx 1) (- fy 2) 3 6)
        (doseq [i [0 12 24] :when particles?]
          (let [rise (mod (+ time i) 34)]
            (.setColor g (Color. 117 112 106 (- 100 (* rise 2))))
            (.fillRect g (+ cx (quot rise 3)) (- fy rise 9) (+ 4 (quot rise 5)) 4))))
      (:built-house :house)
      (doseq [i [0 13 26] :when particles?]
        (let [rise (mod (+ time i) 38)]
          (.setColor g (Color. 159 157 145 (- 112 (* rise 2))))
          (.fillRect g (+ x (int (* width 0.65)) (quot rise 3))
                     (- (+ y (int (* height 0.28))) rise) (+ 5 (quot rise 6)) 4)))
      :lantern (when (< (mod time 11) 7)
                 (.setColor g (Color. 255 222 126 183))
                 (.fillRect g (+ cx 11) (- foot-y 28) 3 4))
      (when (and particles? (canopy-kinds kind) (< (mod time 53) 17))
        (let [fall (mod time 53)]
          (.setColor g (Color. 198 161 80 208))
          (.fillRect g (+ cx 22 (quot fall 2)) (+ y 35 (* 2 fall)) 3 2))))))

(def ^:private relief-actor-samples
  (vec (for [dx [-12 0 12] dy [-22 -11 0 11 19]] [dx dy])))

(defn- draw-layer! [^Graphics2D graphics {:keys [^BufferedImage image x y width height kind foot-y building? damage-tier natural? forest-tree?]
                                          :as layer} characters phase]
  (let [cut (int (* height 0.74))
        image (if (#{:light :heavy :critical} damage-tier) (damaged-sprite image damage-tier) image)
        wind (if (and (pos? phase) (wind-kinds kind))
               (int (Math/round (* 2.0 (Math/sin (+ (* phase 0.32) (* x 0.031)))))) 0)
        ridge? (or (and v5/enabled? (ridge-kinds kind))
                   (#{:mountain-face :mountain-camp :canyon-face :canyon-camp :relief-rock} kind))
        fade-height (if (or building? (= kind :house) ridge? forest-tree?) height cut)
        faded? (and (or (canopy-kinds kind) building? (= kind :house) ridge?)
                    (some (fn [{px :x py :y feet :foot-y}]
                            (when (<= feet foot-y)
                              (if natural?
                                (some (fn [[dx dy]]
                                        (let [sx (+ px dx) sy (+ py dy)]
                                          (and (< x sx (+ x width)) (< y sy (+ y fade-height))
                                               (pos? (unsigned-bit-shift-right
                                                (.getRGB image (int (- sx x)) (int (- sy y))) 24)))))
                                      relief-actor-samples)
                                (and (< x px (+ x width)) (< y py (+ y fade-height))
                                     (pos? (unsigned-bit-shift-right
                                            (.getRGB image (int (- px x)) (int (- py y))) 24))))))
                          characters))]
    (if faded?
      (let [original (.getComposite graphics)]
        (try
          (.setComposite graphics (AlphaComposite/getInstance AlphaComposite/SRC_OVER
                                                             (float (cond forest-tree? 0.33 ridge? 0.55 :else 0.40))))
          (.drawImage graphics image (int (+ x wind)) (int y) (int (+ x width wind)) (int (+ y fade-height))
                      0 0 width fade-height nil)
          (finally (.setComposite graphics original)))
        (when (< fade-height height)
          (.drawImage graphics image (int x) (int (+ y fade-height)) (int (+ x width)) (int (+ y height))
                      0 fade-height width height nil)))
      (cond
        (zero? wind)
        (.drawImage graphics image (int x) (int y) nil)
        (:ground-shadow? layer)
        (.drawImage graphics ^BufferedImage (forest/wind-frame image wind cut)
                    (int (- x 2)) (int y) nil)
        :else
        (do (.drawImage graphics image (int (+ x wind)) (int y) (int (+ x width wind)) (int (+ y cut))
                        0 0 width cut nil)
            (.drawImage graphics image (int x) (int (+ y cut)) (int (+ x width)) (int (+ y height))
                        0 cut width height nil))))
    (when (and (pos? phase) (ambient-kinds kind))
      (ambient! graphics layer phase))))

(def ^:private contact-shadow
  (memoize
   (fn [width]
     (let [image (BufferedImage. width 13 BufferedImage/TYPE_INT_ARGB)
           ^Graphics2D g (.createGraphics image)]
       (try
         (.setColor g (Color. 25 28 23 44))
         (.fillOval g 0 0 width 13)
         (finally (.dispose g)))
       image))))

(defn- draw-layers! [^Graphics2D graphics players camera poses sprites phase]
  (.setFont graphics (Font. "Arial" Font/BOLD 14))
  (let [characters (vec (player-markers players))]
    (doseq [{:keys [kind x width foot-y building? forest-shadow ground-shadow?]} sprites
            :when (and (not ground-shadow?) (or (canopy-kinds kind) building? (ridge-kinds kind)))]
      (if forest-shadow
        (.drawImage graphics ^BufferedImage (:image forest-shadow)
                    (int (:x forest-shadow)) (int (:y forest-shadow)) nil)
        (let [w (max 14 (quot width 2))]
          (.drawImage graphics ^BufferedImage (contact-shadow w)
                      (+ x (quot (- width w) 2)) (- foot-y 15) nil))))
    (loop [scenery (seq sprites)
           actors (seq (sort-by (juxt :foot-y :x) characters))]
      (let [sprite (first scenery)
            actor (first actors)]
        (cond
          (and actor (or (nil? sprite) (<= (:foot-y actor) (:foot-y sprite))))
          (do (draw-player! graphics camera poses actor)
              (recur scenery (next actors)))

          sprite
          (do (draw-layer! graphics sprite characters phase)
              (recur (next scenery) actors)))))))

(defn- covers? [{left :x top :y w :width h :height} {:keys [x y width height]}]
  (and (<= left x) (<= top y)
       (<= (+ x width) (+ left w))
       (<= (+ y height) (+ top h))))

(defn- intersection [a b]
  (let [x (max (:x a) (:x b))
        y (max (:y a) (:y b))
        right (min (+ (:x a) (:width a)) (+ (:x b) (:width b)))
        bottom (min (+ (:y a) (:height a)) (+ (:y b) (:height b)))]
    (when (and (< x right) (< y bottom))
      {:x x :y y :width (- right x) :height (- bottom y)})))

(defn- exposed-strips [{:keys [x y width height]} {left :x top :y w :width h :height}]
  (let [right (+ left w) bottom (+ top h)]
    (for [[sx sy sw sh] [[x y width (- top y)]
                         [x bottom width (- (+ y height) bottom)]
                         [x top (- left x) h]
                         [right top (- (+ x width) right) h]]
          :when (and (pos? sw) (pos? sh))]
      {:x sx :y sy :width sw :height sh})))

(defn- prepare-ground! [^Graphics2D graphics bounds objects previous scenery]
  (if-let [overlap (when previous (intersection bounds (:bounds previous)))]
    (do
      (.drawImage graphics ^BufferedImage (:ground previous)
                  (int (get-in previous [:bounds :x])) (int (get-in previous [:bounds :y])) nil)
      (doseq [{:keys [x y width height] :as strip} (exposed-strips bounds overlap)]
        (let [g ^Graphics2D (.create graphics)]
          (try
            (.clipRect g x y width height)
            (draw-view-ground! g objects strip scenery)
            (finally (.dispose g))))))
    (draw-view-ground! graphics objects bounds scenery)))

(defn prepare-viewport [previous objects camera scenery]
  (let [same-objects? (and previous (= objects (:objects previous)))
        same-ground? (and (= (relief/options scenery) (relief/options (:scenery previous)))
                          (= (forest/options scenery) (forest/options (:scenery previous)))
                          (= (water-study/options scenery) (water-study/options (:scenery previous)))
                          (= (water-study/revision scenery) (water-study/revision (:scenery previous)))
                          (= (:world-art scenery) (:world-art (:scenery previous))))]
    (if (and same-objects? (= scenery (:scenery previous)) (covers? (:bounds previous) camera))
      previous
      (let [{:keys [x y width height]} camera
            pad (* (if (>= width 1920)
                     (if (:world-art scenery) 6 4)
                     (if (:world-art scenery) 3 2))
                   tile-size)
            left (max 0 (- (long (Math/floor x)) pad))
            top (max 0 (- (long (Math/floor y)) pad))
            right (min world-pixel-width (+ (long (Math/ceil (+ x width))) pad))
            bottom (min world-pixel-height (+ (long (Math/ceil (+ y height))) pad))
            bounds {:x left :y top :width (- right left) :height (- bottom top)}
            image (BufferedImage. (:width bounds) (:height bounds) BufferedImage/TYPE_INT_RGB)
            graphics (.createGraphics image)]
        (try
          (viewport-transform! graphics bounds)
          (prepare-ground! graphics bounds objects (when (and same-objects? same-ground?) previous) scenery)
          (finally (.dispose graphics)))
        {:bounds bounds :ground image :layers (vec (scene-sprites objects bounds scenery))
         :objects objects :scenery scenery
         :ripples (when-not (water-study/global-active? scenery) (ripples-in bounds))}))))

(defn render-viewport
  ([previous players camera poses prepared] (render-viewport previous players camera poses prepared 0))
  ([^BufferedImage previous players camera poses {:keys [bounds ground layers ripples scenery]} phase]
  (let [{:keys [width height]} camera
        target (if (and previous (= width (.getWidth previous)) (= height (.getHeight previous)))
                 previous
                 (BufferedImage. width height BufferedImage/TYPE_INT_RGB))
        graphics (.createGraphics target)]
    (try
      (viewport-transform! graphics camera)
      (.drawImage graphics ^BufferedImage ground (int (:x bounds)) (int (:y bounds)) nil)
      (draw-water-motion! graphics camera phase ripples scenery)
      (draw-layers! graphics players camera poses (filter #(intersects? camera %) layers) phase)
      (finally (.dispose graphics)))
    target)))

(defn render-scene
  ([players objects camera] (render-scene players objects camera {}))
  ([players objects camera poses] (render-scene players objects camera poses {}))
  ([players objects camera poses scenery]
   (let [result (BufferedImage. (:width camera) (:height camera) BufferedImage/TYPE_INT_ARGB)
         graphics (.createGraphics result)]
     (try
       (viewport-transform! graphics camera)
       (draw-view-ground! graphics objects camera scenery)
       (draw-water-motion! graphics camera 0
                           (when-not (water-study/global-active? scenery) (ripples-in camera))
                           scenery)
       (draw-layers! graphics players camera poses (scene-sprites objects camera scenery) 0)
       (finally (.dispose graphics)))
     result)))
