(require '[clojure.java.io :as io]
         '[settlement.config :as config]
         '[settlement.rendering :as rendering]
         '[settlement.layout :as layout]
         '[settlement.road-art :as road-art]
         '[settlement.visual-v6 :as v6])
(import '[java.awt.image BufferedImage]
        '[javax.imageio ImageIO])

(assert v6/enabled? "World V6 must be active when baking terrain")
(defn bake-layer! [layer draw!]
  (let [span (* rendering/terrain-chunk-cells config/tile-size)]
  (doseq [y (range (quot rendering/world-pixel-height span))
          x (range (quot rendering/world-pixel-width span))
          :when (or (not= layer :ground) (rendering/owns-ground-chunk? x y))]
    (let [image (BufferedImage. span span BufferedImage/TYPE_INT_RGB)
          graphics (.createGraphics image)
          ox (* x span)
          oy (* y span)
          path (io/file "resources" (rendering/terrain-chunk-path layer x y))]
      (try
        (.translate graphics (- ox) (- oy))
        (draw! graphics {:x ox :y oy :width span :height span})
        (finally (.dispose graphics)))
      (io/make-parents path)
      (ImageIO/write image "png" path)))))

(when-not (= ["--ground-only"] (vec *command-line-args*))
  (bake-layer! :terrain rendering/draw-terrain-tiles!)
  (let [mask (BufferedImage. (quot rendering/world-pixel-width 2)
                             (quot rendering/world-pixel-height 2) BufferedImage/TYPE_BYTE_BINARY)
        raster (.getRaster mask)
        span (quot config/tile-size 2)]
    (doseq [y (range config/world-height) x (range config/world-width)
            :let [^BufferedImage image (road-art/image [x y])]
            :when image
            v (range span) u (range span)
            :when (neg? (.getRGB image (* 2 u) (* 2 v)))]
      (.setSample raster (+ (* x span) u) (+ (* y span) v) 0 1))
    (ImageIO/write mask "png" (io/file "resources" (rendering/terrain-chunk-path "roads" "mask")))))
(bake-layer! :ground #(rendering/draw-ground-layers! %1 layout/initial-objects %2))
(println {:baked-world config/world-seed})
