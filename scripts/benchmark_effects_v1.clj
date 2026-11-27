(require '[settlement.effects :as effects]
         '[settlement.layout :as layout]
         '[settlement.rendering :as rendering])
(import '[java.awt.image BufferedImage])

(defn benchmark [w h]
  (let [screen (BufferedImage. w h BufferedImage/TYPE_INT_RGB)
        current (volatile! nil)
        prepared (volatile! nil)
        samples (vec
                 (for [i (range 100)]
                   (let [x (+ 43 (* 0.16 (mod i 20)))
                         player {:x x :y 25 :team :blue}
                         camera (rendering/camera player w h)
                         sparks (effects/enqueue [] 0
                                  (for [dy (range -2 2) dx (range -2 1)]
                                    {:kind :debris :cell [(+ 46 dx) (+ 25 dy)]}))
                         start (System/nanoTime)
                         _ (vswap! prepared rendering/prepare-viewport layout/initial-objects camera nil)
                         _ (vswap! current rendering/render-viewport {1 player} camera {} @prepared (inc (quot i 3)))
                         g (.createGraphics screen)]
                     (try
                       (.drawImage g ^BufferedImage @current 0 0 nil)
                       (effects/draw! g sparks {1 player} camera 0 0 320)
                       (finally (.dispose g)))
                     (/ (- (System/nanoTime) start) 1e6))))
        warm (vec (sort (drop 24 samples)))]
    {:size [w h] :samples (count warm) :active-effects 12
     :median-ms (warm 38) :p95-ms (warm 72) :max-ms (peek warm)}))

(doseq [[w h] [[1280 720] [1920 1080] [2560 1440]]]
  (println (benchmark w h)))
