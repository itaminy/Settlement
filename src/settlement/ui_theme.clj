(ns settlement.ui-theme
  (:require [clojure.edn :as edn] [clojure.java.io :as io] [clojure.string :as str])
  (:import [java.awt Color Cursor Font GraphicsEnvironment Dimension Container RenderingHints]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [javax.swing JButton JLabel JPanel BorderFactory SwingConstants ImageIcon]
           [javax.swing.event ChangeListener]))

(def wood (Color. 42 34 29))
(def inset (Color. 31 29 26))
(def metal (Color. 95 86 71))
(def gold (Color. 209 171 95))
(def cream (Color. 245 232 205))
(def muted (Color. 185 171 146))
(def blue (Color. 103 170 216))
(def red (Color. 208 106 93))
(def defaults {:fullscreen false :resolution "1280×720" :scale 100 :fps 60
               :animations true :particles true :damage-numbers true :minimap :normal
               :names true})
(def ^:dynamic *settings-file* ".settlement/ui.edn")
(def settings (atom defaults))
(def resolutions ["896×640" "1280×720" "1920×1080" "2560×1440"])

(defn normalize [data]
  (reduce-kv (fn [result key allowed]
               (let [v (get data key)]
                 (if (contains? (set allowed) v) (assoc result key v) result)))
             defaults
             {:fullscreen [true false] :resolution resolutions :scale [85 100 115 130]
              :fps [30 60 120] :animations [true false] :particles [true false]
              :damage-numbers [true false] :minimap [:small :normal :large] :names [true false]}))

(defn load! []
  (reset! settings (try (normalize (edn/read-string (slurp *settings-file* :encoding "UTF-8")))
                        (catch Exception _ defaults))))

(defn save! [values]
  (let [values (normalize values)
        path (io/file *settings-file*)]
    (io/make-parents path)
    (spit path (pr-str values) :encoding "UTF-8")
    (reset! settings values)))

(defn scaled [n] (max 1 (int (* n (/ (:scale @settings) 100.0)))))

(def ^:private wood-tile (delay (ImageIO/read (io/resource "images/ui_v1/wood.png"))))
(def ^:private logo (delay (ImageIO/read (io/resource "images/ui_v1/wordmark.png"))))

(defn wood-panel [layout]
  (proxy [JPanel] [layout]
    (paintComponent [g]
      (proxy-super paintComponent g)
      (let [tile ^BufferedImage @wood-tile]
        (doseq [y (range 0 (.getHeight this) 64), x (range 0 (.getWidth this) 64)]
          (.drawImage g tile x y nil))))))

(defn wordmark []
  (let [image (BufferedImage. (scaled 332) (scaled 82) BufferedImage/TYPE_INT_ARGB)
        g (.createGraphics image)]
    (try (.setRenderingHint g RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
         (.drawImage g ^BufferedImage @logo 0 0 (.getWidth image) (.getHeight image) nil)
         (finally (.dispose g)))
    (doto (JLabel. (ImageIcon. image) SwingConstants/CENTER)
      (.setToolTipText "Settlement"))))

(defn rescale! [root old-scale new-scale]
  (when (not= old-scale new-scale)
    (let [ratio (/ new-scale (double old-scale))]
      (letfn [(visit [component]
                (when-let [font (.getFont component)]
                  (.setFont component (.deriveFont font (float (* ratio (.getSize2D font))))))
                (when (instance? Container component)
                  (run! visit (.getComponents ^Container component))))]
        (visit root)
        (.revalidate root)
        (.repaint root)))))
(defn frame-border [padding]
  (BorderFactory/createCompoundBorder
    (BorderFactory/createLineBorder metal 2)
    (BorderFactory/createCompoundBorder
      (BorderFactory/createLineBorder gold 1)
      (BorderFactory/createEmptyBorder padding padding padding padding))))

(defn label
  ([text size] (label text size cream))
  ([text size color]
   (doto (JLabel. text SwingConstants/CENTER)
     (.setForeground color)
     (.setFont (Font. "Dialog" Font/BOLD (scaled size))))))

(defn button [text]
  (let [button (doto (JButton. text)
                 (.setFont (Font. "Dialog" Font/BOLD (scaled 15)))
                 (.setOpaque true) (.setFocusPainted false) (.setRolloverEnabled true)
                 (.setCursor (Cursor/getPredefinedCursor Cursor/HAND_CURSOR))
                 (.setBorder (frame-border 6)))
        update! (fn []
                  (let [model (.getModel button)
                        bg (cond (not (.isEnabled model)) (Color. 49 46 43)
                                 (.isPressed model) (Color. 71 50 29)
                                 (or (.isRollover model) (.hasFocus button)) (Color. 100 73 45)
                                 :else (Color. 65 48 37))]
                    (.setBackground button bg)
                    (.setForeground button (if (.isEnabled model) cream muted))))]
    (.addChangeListener (.getModel button) (reify ChangeListener (stateChanged [_ _] (update!))))
    (update!)
    button))

(defn style-window! [frame]
  (let [device (.getDefaultScreenDevice (GraphicsEnvironment/getLocalGraphicsEnvironment))
        [w h] (map parse-long (str/split (:resolution @settings) #"×"))
        screen (.getBounds (.getDefaultConfiguration device))]
    (if (:fullscreen @settings)
      (.setFullScreenWindow device frame)
      (do (when (= frame (.getFullScreenWindow device)) (.setFullScreenWindow device nil))
          (.setSize frame (min w (.width screen)) (min h (.height screen)))))))
