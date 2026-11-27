(ns settlement.hotbar
  (:require [clojure.java.io :as io]
            [settlement.config :as config]
            [settlement.inventory :as inventory]
            [settlement.ui-theme :as theme])
  (:import [java.awt Color Dimension Font RenderingHints]
           [java.awt.event KeyEvent MouseAdapter MouseEvent MouseWheelListener]
           [javax.imageio ImageIO]
           [javax.swing AbstractAction JComponent JPanel KeyStroke]))

(def slot-count 9)
(def ^:private nominal-size 56)
(def ^:private gap 4)
(def ^:private padding 5)
(def ^:private slot-items (into inventory/item-order (repeat (- slot-count (count inventory/item-order)) nil)))

(def ^:private icons
  (delay (into {} (for [[item file] {:wood "log_bundle" :stone "stone_pile"
                                    :axe "axe" :pickaxe "pickaxe"}
                        :let [path (if config/world-v7?
                                     (str "images/world_v7_props/" (.replace ^String file "_" "-") ".png")
                                     (str "images/visual_v3/resources/" file ".png"))]]
                    [item (ImageIO/read (io/resource path))]))))

(defn item-image [item] (@icons item))

(defn slot-bounds [width height]
  (let [size (max 0 (min (theme/scaled nominal-size) (- height (* 2 padding))
                         (quot (- width (* 2 padding) (* (dec slot-count) gap)) slot-count)))
        span (+ (* slot-count size) (* (dec slot-count) gap))
        left (quot (- width span) 2)
        top (quot (- height size) 2)]
    (if (pos? size)
      (mapv (fn [index] [(+ left (* index (+ size gap))) top size size]) (range slot-count))
      [])))

(defn- slot-at [panel ^MouseEvent event]
  (let [x (.getX event) y (.getY event)]
    (first (keep-indexed (fn [index [left top width height]]
                           (when (and (<= left x) (< x (+ left width))
                                      (<= top y) (< y (+ top height)))
                             index))
                         (slot-bounds (.getWidth panel) (.getHeight panel))))))

(defn- draw-count! [graphics amount left top size]
  (let [label (str amount)]
    (loop [font-size (max 1 (min 16 (quot size 3)))]
      (.setFont graphics (Font. "Monospaced" Font/BOLD font-size))
      (if-let [[x y width ascent] (let [metrics (.getFontMetrics graphics)
                                       width (.stringWidth metrics label)]
                                   (when (<= width (- size 8))
                                     [(- (+ left size) 5 width) (- (+ top size) 5)
                                      width (.getAscent metrics)]))]
        (do (.setColor graphics (Color. 18 23 21 206))
            (.fillRect graphics (- x 2) (- y ascent) (+ width 4) (+ ascent 3))
            (.setColor graphics (Color. 18 23 21))
            (.drawString graphics label (inc x) (inc y))
            (.setColor graphics (Color. 249 241 216))
            (.drawString graphics label x y))
        (when (> font-size 1) (recur (dec font-size)))))))

(defn- draw-slot! [graphics index [x y size _] selected items]
  (let [item (slot-items index)
        amount (get items item 0)
        active? (= index selected)]
    (.setColor graphics (Color. 13 19 17 218))
    (.fillRect graphics x y size size)
    (.setColor graphics (if active? (Color. 244 220 152) theme/metal))
    (.drawRect graphics x y (dec size) (dec size))
    (when (> size 12)
      (.setColor graphics (if active? (Color. 159 133 77) (Color. 53 66 49)))
      (.drawRect graphics (inc x) (inc y) (- size 3) (- size 3))
      (when active?
        (.setColor graphics theme/gold)
        (.fillRect graphics (+ x 4) (+ y 2) (- size 8) 2))
      (.setColor graphics (Color. 174 181 150))
      (.setFont graphics (Font. "Monospaced" Font/BOLD (min 11 (quot size 4))))
      (.drawString graphics (str (inc index)) (+ x 5) (+ y (min 13 (quot size 3))))
      (when (pos? amount)
        (let [source (@icons item)
              scale (/ (* size 0.82) (max (.getWidth source) (.getHeight source)))
              width (max 1 (int (Math/round (* scale (.getWidth source)))))
              height (max 1 (int (Math/round (* scale (.getHeight source)))))]
          (.drawImage graphics source (+ x (quot (- size width) 2))
                      (+ y (quot (- size height) 2) (- (quot size 11)))
                      width height nil))
        (draw-count! graphics amount x y size)))))

(defn create-panel [items]
  (let [selected (atom 0)
        contents (atom items)
        panel (proxy [JPanel] []
                (getPreferredSize []
                  (Dimension. (+ (* slot-count (theme/scaled nominal-size)) (* (dec slot-count) gap) (* 2 padding))
                              (+ (theme/scaled nominal-size) (* 2 padding))))
                (paintComponent [graphics]
                  (proxy-super paintComponent graphics)
                  (let [g (.create graphics)]
                    (try
                      (.setRenderingHint g RenderingHints/KEY_INTERPOLATION
                                         RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
                      (doseq [[index bounds] (map-indexed vector (slot-bounds (.getWidth this) (.getHeight this)))]
                        (draw-slot! g index bounds @selected @contents))
                      (finally (.dispose g)))))
                (getToolTipText
                  ([] (proxy-super getToolTipText))
                  ([event]
                   (when-let [index (slot-at this event)]
                     (get inventory/item-names (slot-items index) "Свободная ячейка")))))
        select! (fn [index] (reset! selected index) (.repaint panel))
        wheel (reify MouseWheelListener
                (mouseWheelMoved [_ event]
                  (when-not (zero? (.getWheelRotation event))
                    (select! (mod (+ @selected (.getWheelRotation event)) slot-count))
                    (.consume event))))]
    (.setOpaque panel false)
    (.setFocusable panel false)
    (.setToolTipText panel "")
    (.addMouseListener panel (proxy [MouseAdapter] []
                               (mousePressed [event]
                                 (when (= MouseEvent/BUTTON1 (.getButton event))
                                   (when-let [index (slot-at panel event)]
                                     (select! index)
                                     (.consume event))))))
    (.addMouseWheelListener panel wheel)
    (doseq [index (range slot-count)]
      (let [action (str "hotbar-" (inc index))]
        (doseq [first-key [KeyEvent/VK_1 KeyEvent/VK_NUMPAD1]]
          (.put (.getInputMap panel JComponent/WHEN_IN_FOCUSED_WINDOW)
                (KeyStroke/getKeyStroke (+ first-key index) 0) action))
        (.put (.getActionMap panel) action
              (proxy [AbstractAction] [] (actionPerformed [_] (select! index))))))
    {:panel panel :selected selected :items contents :wheel wheel}))

(defn update-items! [{:keys [panel items]} contents]
  (when (not= contents @items)
    (reset! items contents)
    (.repaint panel)))
