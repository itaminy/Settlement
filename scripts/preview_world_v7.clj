(require '[clojure.java.io :as io]
         '[settlement.buildings :as buildings]
         '[settlement.config :as config]
         '[settlement.gui :as gui]
         '[settlement.layout :as layout]
         '[settlement.rendering :as render]
         '[settlement.world :as world])

(import '[java.awt RenderingHints]
        '[java.awt.image BufferedImage]
        '[javax.imageio ImageIO]
        '[javax.swing JFrame JLabel JProgressBar SwingUtilities])

(def output (io/file "docs/previews/world_v7_0"))

(def views
  [["01-deciduous-forest" [13 37] [13 38]]
   ["02-conifer-forest" [38 37] [37 37]]
   ["03-mixed-forest" [73 7] [73 7]]
   ["04-plains" [57 23] nil]
   ["05-meadow" [63 35] nil]
   ["06-marsh" [66 56] nil]
   ["07-mountain-range" [18 77] nil]
   ["08-snowy-mountains" [17 88] [17 88]]
   ["09-canyon" [87 72] [87 68]]
   ["10-desert" [63 91] nil]
   ["11-oasis" [90 79] nil]
   ["12-river-bridge" [49 36] [49 36]]
   ["13-large-lake" [49 71.5] nil]
   ["14-mountain-waterfall" [31.6 67.4] nil]
   ["15-buildings-and-camp" [18 18] nil :camp]
   ["16-gameplay" [25 25] nil :gameplay]])

(defn near [[cx cy] radius]
  (sort-by (juxt #(reduce + (map abs (map - % [cx cy]))) first second)
           (for [x (range (- cx radius) (+ cx radius 1))
                 y (range (- cy radius) (+ cy radius 1))
                 :when (config/in-bounds? [x y])]
             [x y])))

(defn place-actor! [id [x y]]
  (dosync (alter world/world update-in [:players id] assoc :x x :y y)))

(defn build-near! [id center kind]
  (or (some (fn [anchor]
              (let [state (world/get-world)
                    footprint (set (buildings/cells anchor {:type :building :kind kind}))]
                (when (not-any? #(or (buildings/protected-cells %)
                                    (buildings/occupied? (:objects state) %)) footprint)
                  (when-let [stand (first (filter #(and (not (footprint %))
                                                        (world/free-cell? state %))
                                                  (mapcat layout/neighbors footprint)))]
                    (place-actor! id stand)
                    (when (= :ok (:status (world/check-build id kind anchor)))
                      (let [result (world/build! id kind anchor)]
                        (when (= :ok (:status result)) anchor)))))))
            (near center 5))
      (throw (ex-info "Could not build preview structure" {:center center :kind kind}))))

(defn staged-state [mode]
  (let [initial (world/get-world)]
    (with-redefs [world/world (ref initial)]
      (assert (world/add-player 1 "Blue" :blue))
      (assert (world/add-player 2 "Red" :red))
      (doseq [id [1 2]]
        (world/change-player-items! id {:wood 80 :stone 70 :axe 1 :pickaxe 1}))
      (doseq [[id p kind] (if (= mode :camp)
                            [[1 [20 16] :house] [2 [20 19] :watchtower] [1 [17 19] :fence]]
                            [[1 [25 21] :house] [2 [27 25] :house] [1 [20 28] :sawmill]])]
        (build-near! id p kind))
      (let [[cx cy] (if (= mode :camp) [18 18] [25 25])]
        (doseq [[id center] [[1 [(dec cx) cy]] [2 [(+ cx 2) (inc cy)]]]]
          (place-actor! id (first (filter #(world/free-cell? (world/get-world) %) (near center 3))))))
      (world/get-world))))

(defn scene-state [view]
  (let [[_ _ [x y :as actor] fixture] view]
    (if fixture
      (staged-state fixture)
      (do (when actor (assert (nil? (layout/initial-objects actor)) actor))
          {:objects layout/initial-objects
           :players (if actor {1 {:x x :y y :team :blue :name "Blue" :hp 100 :max-hp 100}} {})
           :match {:scores {:blue 0 :red 0}} :match-duration-ms 0}))))

(defn capture! [^JFrame window [name [cx cy] _ fixture] state prepared]
  (let [map-panel (gui/create-map-panel cx cy)
        ui (gui/create-resource-panel {})
        ^java.awt.Container panel (:panel (gui/create-game-content
                                            map-panel ui (doto (JProgressBar.) (.setVisible false)) (JLabel.)))
        scale (if (#{"07-mountain-range" "09-canyon" "13-large-lake"} name) 1.0 0.9)]
    (gui/set-map-scenery! map-panel render/game-scenery)
    (gui/prime-map-viewport! map-panel prepared)
    (gui/update-map map-panel (:players state) (:objects state) (when fixture (:bandits state)))
    (when (seq (:players state))
      (gui/update-player-panels! ui state 1))
    (.setContentPane window panel)
    (.setSize window (int (* 1440 scale)) (int (* 900 scale)))
    (.setVisible window true)
    (.validate window)
    (let [image (BufferedImage. (.getWidth panel) (.getHeight panel) BufferedImage/TYPE_INT_RGB)
          graphics (.createGraphics image)]
      (try
        (.printAll panel graphics)
        (ImageIO/write image "png" (io/file output (str name ".png")))
        (finally
          (.dispose graphics)
          (.setVisible window false))))))

(defn overview! []
  ;; Use the same prepared layers and compositor as Swing, then scale in one
  ;; pass. Tiling bounds the working set and preserves a full-width camera.
  (let [w render/world-pixel-width h render/world-pixel-height
        summary (BufferedImage. (quot w 4) (quot h 4) BufferedImage/TYPE_INT_RGB)
        graphics (.createGraphics summary)
        scenery render/game-scenery
        edge (* 10 config/tile-size)]
    (try
      (.setRenderingHint graphics RenderingHints/KEY_INTERPOLATION
                         RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
      (doseq [y (range 0 h edge) x (range 0 w edge)
              :let [width (min edge (- w x))
                    height (min edge (- h y))
                    camera {:x x :y y :width width :height height}
                    prepared (render/prepare-viewport nil layout/initial-objects camera scenery)
                    image (render/render-viewport nil {} camera {} prepared 0)]]
        (.drawImage graphics image (quot x 4) (quot y 4) (quot width 4) (quot height 4) nil))
      (finally (.dispose graphics)))
    (ImageIO/write summary "png" (io/file output "map-overview.png"))))

(let [selected (set *command-line-args*)]
  (assert (:world-art render/game-scenery) "Launch the V7.0 candidate to capture final previews")
  (.mkdirs output)
  (let [window (volatile! nil)]
    (try
      (doseq [[name [x y] :as view] views
              :when (or (empty? selected) (selected name))]
        (let [state (scene-state view)
              prepared (render/prepare-viewport nil (:objects state)
                         (render/camera {:x x :y y} 1440 900) render/game-scenery)]
          (SwingUtilities/invokeAndWait
           #(do (when-not @window
                  (vreset! window (doto (JFrame. "Settlement | World V7.0") (.setUndecorated true))))
                (capture! @window view state prepared)))))
      (finally
        (when @window (SwingUtilities/invokeAndWait #(.dispose ^JFrame @window))))))
  (when (or (empty? selected) (selected "map-overview")) (overview!)))
(shutdown-agents)
