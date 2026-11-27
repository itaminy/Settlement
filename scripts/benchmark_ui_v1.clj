(require '[clojure.string :as str]
         '[settlement.effects :as effects]
         '[settlement.gui :as gui]
         '[settlement.forest :as forest]
         '[settlement.layout :as layout]
         '[settlement.minimap :as minimap]
         '[settlement.rendering :as rendering])
(import '[java.awt Container]
        '[java.awt.image BufferedImage]
        '[java.lang.management GarbageCollectorMXBean ManagementFactory]
        '[javax.swing JLabel JProgressBar SwingUtilities])

(defn layout! [^Container container]
  (.doLayout container)
  (doseq [child (.getComponents container) :when (instance? Container child)]
    (layout! child)))

(defn- percentiles [samples]
  (let [ordered (vec (sort samples))]
    {:p50-ms (ordered 50) :p95-ms (ordered 94) :max-ms (peek ordered)}))

(defn benchmark [w h [cx cy] scenery warmup preload? pan?]
  (let [screen (BufferedImage. w h BufferedImage/TYPE_INT_RGB)
        current (volatile! nil)
        prepared (volatile! nil)
        terrain (gui/create-map-panel cx cy 1)
        ui (gui/create-resource-panel {:wood 18 :stone 12 :axe 1})
        content (gui/create-game-content terrain ui (doto (JProgressBar.) (.setVisible false)) (JLabel.))
        ^Container panel (:panel content)
        header (:panel ui)
        footer (:footer content)
        _ (.setSize panel w h)
        _ (layout! panel)
        offset (.getY terrain)
        view-height (.getHeight terrain)
        first-camera (rendering/camera {:x cx :y cy} w view-height)
        startup-ms (.getUptime (ManagementFactory/getRuntimeMXBean))
        launcher-start (some-> (System/getenv "SETTLEMENT_BENCH_LAUNCHED_AT") parse-long)
        launch-overhead (when launcher-start
                          (- (.getStartTime (ManagementFactory/getRuntimeMXBean))
                             launcher-start))
        preload-start (System/nanoTime)
        _ (when preload?
            (vreset! prepared (rendering/prepare-viewport nil layout/initial-objects first-camera scenery)))
        preload-ms (if preload? (/ (- (System/nanoTime) preload-start) 1e6) 0)
        sparks (effects/enqueue [] 0
                 (for [dy (range -2 2), dx (range -2 1)]
                   {:kind :debris :cell [(+ cx 3 dx) (+ cy dy)]}))
        gc-time (fn [] (reduce + (map #(.getCollectionTime ^GarbageCollectorMXBean %)
                                      (ManagementFactory/getGarbageCollectorMXBeans))))
        warm-gc-start (volatile! nil)
        samples (vec
                 (for [i (range (+ warmup 100))]
                   (let [_ (when (= i warmup) (vreset! warm-gc-start (gc-time)))
                         player {:x (+ cx (* 0.16 (if pan? (- 55 (abs (- 55 (mod i 110)))) (mod i 20))))
                                 :y (+ cy (if pan? (* 0.085 (mod i 41)) 0)) :team :blue}
                         camera (rendering/camera player w view-height)
                         start (System/nanoTime)
                         _ (vswap! prepared rendering/prepare-viewport layout/initial-objects camera scenery)
                         prepare-end (System/nanoTime)
                         _ (vswap! current rendering/render-viewport {1 player} camera {} @prepared (inc (quot i 3)))
                         render-end (System/nanoTime)
                         g (.createGraphics screen)
                         map-g (.create g 0 offset w view-height)
                         footer-g (.create g 0 (.getY footer) w (.getHeight footer))]
                     (try
                       (.drawImage map-g ^BufferedImage @current 0 0 nil)
                       (effects/draw! map-g sparks {1 player} camera 0 0 320)
                       (minimap/draw! map-g terrain camera {1 player} [])
                       (.printAll header g)
                       (.printAll footer footer-g)
                       (finally (.dispose map-g) (.dispose footer-g) (.dispose g)))
                     (let [end (System/nanoTime)]
                       [(/ (- end start) 1e6) (/ (- prepare-end start) 1e6)
                        (/ (- render-end prepare-end) 1e6) (/ (- end render-end) 1e6)]))))
        [first-frame first-prepare] (first samples)
        warm (vec (drop warmup samples))
        frame (percentiles (map first warm))]
    {:size [w h] :center [cx cy] :samples (count warm) :warmup-frames warmup :active-effects 12
     :pid (.pid (java.lang.ProcessHandle/current))
     :cold-first-ms first-frame :cold-prepare-ms first-prepare :preload-ms preload-ms
     :jvm-to-first-ms (+ startup-ms preload-ms first-frame)
     :launch-to-first-ms (when launch-overhead (+ launch-overhead startup-ms preload-ms first-frame))
     :total-first-ms (+ preload-ms first-frame)
     :median-ms (:p50-ms frame) :p95-ms (:p95-ms frame) :max-ms (:max-ms frame)
     :prepare (percentiles (map second warm))
     :render (percentiles (map #(nth % 2) warm))
     :composite-hud (percentiles (map peek warm))
     :warm-gc-ms (- (gc-time) @warm-gc-start)
     :heap-used-mib (let [runtime (Runtime/getRuntime)]
                      (/ (- (.totalMemory runtime) (.freeMemory runtime)) 1048576.0))}))

(let [mode (first *command-line-args*)
      mountain-720? (and (= mode "relief-integrated")
                         (some #{"--mountain-720"} *command-line-args*))
      mixed-1440? (and (= mode "forest-world")
                       (some #{"--mixed-1440"} *command-line-args*))
      water? (#{"water-natural" "water-polished" "water-study" "water-baseline"} mode)
      world-v7? (#{"world-v7" "world-v7-pan"} mode)
      pan? (= mode "world-v7-pan")
      preload? (some #{"--preload"} *command-line-args*)
      focus (some #(when (str/starts-with? % "--center=")
                     (mapv parse-long (str/split (subs % 9) #","))) *command-line-args*)
      resolution (some #(when (str/starts-with? % "--width=")
                          ({1280 [1280 720] 1920 [1920 1080] 2560 [2560 1440]}
                           (parse-long (subs % 8)))) *command-line-args*)
      river-1440? (and water? (some #{"--river-1440"} *command-line-args*))
      water-sites {[49 36] :river [49 70] :lake [33 68] :fall [90 79] :oasis}
      centers (case mode
                ("water-natural" "water-polished" "water-study" "water-baseline") [[49 36] [49 70] [33 68] [90 79]]
                "world-v7" [[49 36] [66 56] [73 7] [87 72] [23 25]]
                "world-v7-pan" [[49 36] [66 56] [73 7] [87 72] [23 25]]
                "forest-world" (conj (mapv forest/camera-centers [:leaf :pine]) [73 7])
                "forest-study" (mapv forest/camera-centers [:leaf :pine])
                "relief-integrated" [[17 76] [87 68] [95 81]]
                ("relief-prior" "relief-natural") [[17 86] [87 68] [95 81]]
                ("relief-before" "relief-after") [[17 86] [87 68]]
                "desert-canyon" [[87 68] [95 81] [89 79]]
                "mountain-lake" [[24 69] [20 67] [17 88]]
                "massive-mountain" [[20 57] [10 72] [17 88]]
                "world-v6" [[43 25] [14 64] [68 55] [87 87]]
                "world-v5" [[43 25] [17 62] [68 55]]
                [[43 25]])]
  (SwingUtilities/invokeAndWait
   #(doseq [center (cond focus [focus] mountain-720? [[17 76]] mixed-1440? [[73 7]]
                         river-1440? [[49 36]] :else centers)
            [w h] (cond resolution [resolution] mountain-720? [[1280 720]] (or mixed-1440? river-1440?) [[2560 1440]]
                        :else [[1280 720] [1920 1080] [2560 1440]])]
      (let [scenery (cond
                      water? (cond-> rendering/game-scenery
                               (not= mode "water-baseline") (assoc :water-study (water-sites center))
                               (= mode "water-natural") (assoc :water-revision :natural)
                               (= mode "water-polished") (assoc :water-revision :polished))
                      (= mode "forest-study")
                      (assoc rendering/game-scenery :forest-study
                             (if (= center (:leaf forest/camera-centers)) :leaf :pine))
                      (or world-v7? (#{"relief-integrated" "forest-world"} mode))
                      rendering/game-scenery
                      :else (when (#{"relief-after" "relief-prior" "relief-natural"} mode)
                      {:terrain-relief (cond-> {:scene (if (= 17 (first center)) :mountain :canyon)
                                                :view :lit}
                                         (= mode "relief-natural") (assoc :revision :natural))}))]
        (println (benchmark w h center scenery
                            (if (or world-v7? water? mountain-720? (#{"forest-study" "forest-world"} mode)) 200 60)
                            preload? pan?))))))
