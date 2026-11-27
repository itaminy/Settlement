(ns settlement.minimap
  (:require [settlement.buildings :as buildings]
            [settlement.config :as config]
            [settlement.landscape :as landscape]
            [settlement.layout :as layout]
            [settlement.massive-mountain :as mountain]
            [settlement.desert-canyon :as canyon]
            [settlement.ui-theme :as theme]
            [settlement.rendering :as rendering])
  (:import [java.awt Color Font Graphics2D RenderingHints]
           [java.awt.geom Rectangle2D$Double]
           [java.awt.image BufferedImage]
           [java.awt.event KeyEvent]
           [javax.swing AbstractAction JComponent KeyStroke]))

(def ^:private margin 8)
(def ^:private border 2)
(def ^:private legend-height 24)

(defn bounds [width height]
  (let [edge (min width height)
        factor ({:small 0.75 :normal 1.0 :large 1.25} (:minimap @theme/settings))
        size (min (- edge (* 2 margin)) (- height legend-height (* 2 margin))
                  (max 100 (min 400 (int (* factor (min 320 (quot (* 220 edge) 768)))))))]
    (when (> size (* 2 border))
      [(- width size margin) margin size])))

(defn visible? [panel]
  (true? (.getClientProperty panel ::visible)))

(defn hit? [panel px py]
  (when (visible? panel)
    (when-let [[x y size] (bounds (.getWidth panel) (.getHeight panel))]
      (and (<= x px) (< px (+ x size)) (<= y py) (< py (+ y size legend-height))))))

(defn install! [panel key-host]
  (.putClientProperty panel ::visible true)
  (.put (.getInputMap key-host JComponent/WHEN_IN_FOCUSED_WINDOW)
        (KeyStroke/getKeyStroke KeyEvent/VK_M 0 true) "minimap")
  (.put (.getActionMap key-host) "minimap"
        (proxy [AbstractAction] []
          (actionPerformed [_]
            (.putClientProperty panel ::visible (not (visible? panel)))
            (.repaint panel))))
  (.put (.getInputMap key-host JComponent/WHEN_IN_FOCUSED_WINDOW)
        (KeyStroke/getKeyStroke KeyEvent/VK_N 0 true) "minimap-size")
  (.put (.getActionMap key-host) "minimap-size"
        (proxy [AbstractAction] []
          (actionPerformed [_]
            (let [next ({:small :normal :normal :large :large :small} (:minimap @theme/settings))]
              (try (theme/save! (assoc @theme/settings :minimap next))
                   (catch Exception _ (swap! theme/settings assoc :minimap next))))
            (.repaint panel)))))

(def ^:private biome-colors
  {:plains 0x9ba65c :conifer 0x315640 :deciduous 0x557b43
   :mountains 0x787478 :desert 0xc39455 :snow 0xdce6e6 :swamp 0x657058})

(defn- terrain-color [cell]
  (cond
    (landscape/bridge-cells cell) 0xe5bc83
    (landscape/wet-cells cell) 0x397fb0
    (canyon/bridges cell) 0xf4c388
    (canyon/routes cell) 0xe6ae72
    (canyon/pits cell) 0x693d38
    (canyon/walls cell) 0xa15a39
    (= mountain/summit cell) 0xfff2be
    (mountain/route-cells cell) 0xd1b18a
    (mountain/platforms cell) 0xb69d7a
    (mountain/blocks cell) (if (< (second cell) 80) 0x75716b 0xc5d1cf)
    (layout/road-cells cell) (case (layout/road-material cell)
                               :stone 0xb6a889 :sand 0x997343 :forest 0xa9885b
                               :mountain 0xb1a092 0xa67b53)
    :else (biome-colors (landscape/biome cell))))

(def landscape-image
  (delay
    (let [image (BufferedImage. config/world-width config/world-height BufferedImage/TYPE_INT_RGB)]
      (doseq [[x y :as cell] (landscape/cells)]
        (.setRGB image x y (terrain-color cell)))
      image)))

(def ^:private team-colors
  {:blue (Color. 74 182 255) :red (Color. 255 90 81) :bandits (Color. 225 137 255)})

(defn structures [objects]
  (into [] (keep (fn [[position {:keys [team kind] :as entry}]]
                   (when (and (buildings/building? entry) (#{:blue :red} team) (pos? (:hp entry 0)))
                     [position team (get-in buildings/catalog [kind :size])])))
        objects))

(defn camera-box [{:keys [x y width height]} size]
  (let [sx (/ (double size) rendering/world-pixel-width)
        sy (/ (double size) rendering/world-pixel-height)]
    [(* x sx) (* y sy) (* width sx) (* height sy)]))

(defn draw! [^Graphics2D graphics panel camera entities marks]
  (when (visible? panel)
    (when-let [[x y outer] (bounds (.getWidth panel) (.getHeight panel))]
      (let [size (- outer (* 2 border))
            sx (/ (double size) config/world-width)
            sy (/ (double size) config/world-height)
            ^Graphics2D g (.create graphics)]
        (try
          (.setColor g (Color. 22 27 19 170))
          (.fillRect g x y (+ outer 2) (+ outer 2))
          (.setColor g theme/metal)
          (.fillRect g x (+ y outer) outer legend-height)
          (.setColor g theme/inset)
          (.fillRect g (+ x 2) (+ y outer) (- outer 4) (- legend-height 2))
          (.setFont g (Font. "Dialog" Font/BOLD (if (< outer 160) 8 10)))
          (doseq [[label color lx ly] [["● Игрок" theme/blue 5 9]
                                       ["● Бот" theme/red (quot outer 2) 9]
                                       ["● Бандиты" (Color. 225 137 255) 5 19]
                                       ["□ Камера" theme/cream (quot outer 2) 19]]]
            (.setColor g color)
            (.drawString g label (+ x lx) (+ y outer ly)))
          (.setColor g theme/gold)
          (.fillRect g x y outer outer)
          (.translate g (double (+ x border)) (double (+ y border)))
          (.clipRect g 0 0 size size)
          (.setRenderingHint g RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
          (.drawImage g ^BufferedImage @landscape-image 0 0 size size nil)
          (doseq [[[bx by] team [w h]] marks]
            (let [ax (int (* bx sx)) ay (int (* by sy))
                  width (max 3 (int (Math/ceil (* w sx))))
                  height (max 3 (int (Math/ceil (* h sy))))]
              (.setColor g Color/BLACK)
              (.fillRect g (dec ax) (dec ay) (+ width 2) (+ height 2))
              (.setColor g (team-colors team))
              (.fillRect g ax ay width height)))
          (doseq [{:keys [x y team]} (vals entities)
                  :let [color (team-colors team)]
                  :when color]
            (let [radius (if (= :bandits team) 3 4)
                  cx (int (* (+ x 0.5) sx)) cy (int (* (+ y 0.5) sy))
                  left (- cx radius) top (- cy radius)]
              (.setColor g Color/BLACK)
              (.fillOval g (dec left) (dec top) (+ (* radius 2) 2) (+ (* radius 2) 2))
              (.setColor g color)
              (.fillOval g left top (* radius 2) (* radius 2))))
          (let [[vx vy vw vh] (camera-box camera size)]
            (.setColor g Color/WHITE)
            (.draw g (Rectangle2D$Double. (min (- size 1.5) (+ vx 0.5))
                                         (min (- size 1.5) (+ vy 0.5))
                                         (max 1.0 (- vw 1))
                                         (max 1.0 (- vh 1)))))
          (finally (.dispose g)))))))
