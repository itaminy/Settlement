(require '[clojure.java.io :as io]
         '[settlement.gui :as gui]
         '[settlement.layout :as layout]
         '[settlement.landscape :as landscape]
         '[settlement.rendering :as rendering]
         '[settlement.config :as config])
(import '[javax.imageio ImageIO]
        '[javax.swing JFrame SwingUtilities WindowConstants]
        '[java.awt Color Font GraphicsEnvironment RenderingHints]
        '[java.awt.image BufferedImage])

(def output (io/file "docs/previews/world_v3"))
(.mkdirs output)

(def regions
  {"forest" [12 17] "deciduous" [37 36] "mountains" [17 65] "desert" [89 73]
   "snow" [18 87] "river" [48 25] "lake" [44 72] "wetlands" [66 55]
   "waterfall" [53 9] "village" [32 25]})

(defn write-image [image name]
  (ImageIO/write image "png" (io/file output (str name ".png"))))

(defn swing-previews []
  (SwingUtilities/invokeAndWait
   (fn []
     (let [desktop? (not (GraphicsEnvironment/isHeadless))
           window (when desktop? (JFrame. "Settlement - visual_v3"))]
       (println "Swing desktop:" desktop?)
       (try
         (when window
           (.setDefaultCloseOperation window WindowConstants/DISPOSE_ON_CLOSE)
           (.setSize window 912 679)
           (.setVisible window true)
           (.validate window))
         (doseq [[name [x y]] regions]
           (let [panel (gui/create-map-panel x y)
                 [px py] (first (for [r (range 8) dx (range (- r) (inc r))
                                      dy (range (- r) (inc r))
                                      :let [p [(+ x dx) (+ y dy)]]
                                      :when (and (config/in-bounds? p) (not (layout/initial-objects p)))]
                                  p))
                 players (if px {1 {:x px :y py :team :blue :name ""}} {})
                 canvas (BufferedImage. 896 640 BufferedImage/TYPE_INT_ARGB)
                 graphics (.createGraphics canvas)]
             (when window
               (.setContentPane window panel)
               (.validate window))
             (.setSize panel 896 640)
             (gui/update-map panel players layout/initial-objects)
             (try (.paint panel graphics) (finally (.dispose graphics)))
             (write-image canvas name)))
         (finally (when window (.dispose window))))))))

(defn- overview []
  (let [mini 10
        step 20
        tile config/tile-size
        result (BufferedImage. (* mini config/world-width) (* mini config/world-height)
                               BufferedImage/TYPE_INT_ARGB)
        graphics (.createGraphics result)
        labels [["CONIFER FOREST" 12 11] ["DECIDUOUS FOREST" 35 37]
                ["MOUNTAINS" 23 60] ["SNOWFIELDS" 24 97] ["DESERT" 83 90]
                ["WETLANDS" 69 55] ["PLAINS" 77 45] ["RIVER" 53 13] ["LAKE" 50 71]
                ["BLUE START" 23 26] ["RED START" 76 26]]]
    (try
      (.setRenderingHint graphics RenderingHints/KEY_INTERPOLATION
                         RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
      (doseq [y (range 0 config/world-height step) x (range 0 config/world-width step)
              :let [w (min step (- config/world-width x)) h (min step (- config/world-height y))
                    image (rendering/render-scene
                           {} layout/initial-objects {:x (* x tile) :y (* y tile)
                                                      :width (* w tile) :height (* h tile)})]]
        (.drawImage graphics image (* x mini) (* y mini) (* w mini) (* h mini) nil))
      (.setFont graphics (Font. "Dialog" Font/BOLD 13))
      (doseq [[label x y] labels
              :let [width (.stringWidth (.getFontMetrics graphics) label)
                    left (- (* x mini) (quot width 2)) top (* y mini)]]
        (.setColor graphics (Color. 22 28 31 210))
        (.fillRoundRect graphics (- left 5) (- top 14) (+ width 10) 20 7 7)
        (.setColor graphics (Color. 246 233 193))
        (.drawString graphics label left top))
      (finally (.dispose graphics)))
    (write-image result "overview")))

(time (swing-previews))
(time (overview))

(let [camera (rendering/camera {:x 48 :y 25} 896 640)
      render #(rendering/render-scene {} layout/initial-objects camera)]
  (dotimes [_ 5] (render))
  (let [measurements (sort (repeatedly 60 #(let [start (System/nanoTime)]
                                             (render)
                                             (/ (- (System/nanoTime) start) 1e6))))]
    (println {:viewport-ms-median (nth measurements 30) :viewport-ms-p95 (nth measurements 56)
              :visible-tiles (count (rendering/visible-cells camera 0))
              :tiles-with-margin (count (rendering/visible-cells camera rendering/visual-culling-margin))})))
