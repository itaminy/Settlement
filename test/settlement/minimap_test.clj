(ns settlement.minimap-test
  (:require [clojure.test :refer [deftest is]]
            [settlement.building-ui :as building-ui]
            [settlement.client :as client]
            [settlement.gui :as gui]
            [settlement.landscape :as landscape]
            [settlement.minimap :as minimap]
            [settlement.rendering :as rendering])
  (:import [java.awt.event MouseEvent KeyEvent]
           [javax.swing JLabel JProgressBar JComponent KeyStroke SwingUtilities]))

(deftest map-layout-and-terrain-cache
  (is (= [668 8 220] (minimap/bounds 896 768)))
  (is (= [212 8 100] (minimap/bounds 320 180)))
  (is (= [1603 8 309] (minimap/bounds 1920 1080)))
  (is (= [2232 8 320] (minimap/bounds 2560 1440)))
  (is (nil? (minimap/bounds 100 10)))
  (let [image @minimap/landscape-image
        sample (fn [positions] (let [[x y] (first positions)] (.getRGB image x y)))
        colors (map #(sample (filter (fn [cell] (= % (landscape/biome cell))) (landscape/cells)))
                    [:plains :mountains :desert :snow])]
    (is (identical? image @minimap/landscape-image))
    (is (= 4 (count (distinct colors))))
    (is (not= (sample landscape/bridge-cells) (sample (keys landscape/water-blocks))))))

(deftest camera-uses-world-pixels-and-preserves-fractional-position
  (let [size 216
        [x y w h] (minimap/camera-box (rendering/camera {:x 42.25 :y 66.5} 896 768) size)]
    (is (< 0 x (+ x w) size))
    (is (< 0 y (+ y h) size))
    (is (not= x (Math/floor x)))
    (is (= [0.0 0.0 216.0 216.0]
           (minimap/camera-box (rendering/camera {:x 0 :y 0} 6400 6400) size)))))

(deftest structure-markers-follow-confirmed-snapshots
  (let [house {:type :building :team :blue :kind :house :hp 200}
        tower {:type :building :team :red :kind :watchtower :hp 50}
        objects {[2 3] house [8 9] tower [4 5] :tree [6 7] (assoc tower :team nil)}]
    (is (= #{[[2 3] :blue [2 2]] [[8 9] :red [2 1]]} (set (minimap/structures objects))))
    (is (= [[[2 3] :blue [2 2]]] (minimap/structures (assoc-in objects [[8 9] :hp] 0))))
    (is (empty? (minimap/structures {})))))

(deftest minimap-key-and-mouse-do-not-issue-map-actions
  (SwingUtilities/invokeAndWait
    (fn []
      (let [game (doto (gui/create-map-panel 50 50 1) (.setSize 896 768))
            label (JLabel.)
            {:keys [panel]}
            (gui/create-game-content game (gui/create-resource-panel {}) (JProgressBar.) label)
            calls (atom [])
            builder (building-ui/install! game {} 1 label identity (constantly nil))]
        (try
          (let [stroke (KeyStroke/getKeyStroke KeyEvent/VK_M 0 true)
                key (.get (.getInputMap panel JComponent/WHEN_IN_FOCUSED_WINDOW) stroke)
                toggle (.get (.getActionMap panel) key)
                [x y _] (minimap/bounds (.getWidth game) (.getHeight game))]
            (is (= "minimap" key))
            (is (minimap/hit? game x y))
            (with-redefs [client/async! (fn [& args] (swap! calls conj args))]
              (doseq [mode [:attack :repair :build]]
                (let [click (MouseEvent. game MouseEvent/MOUSE_CLICKED 0 0 x y 1 false MouseEvent/BUTTON1)]
                  (reset! (:state builder) {:mode mode :kind :house})
                  (.dispatchEvent game click)
                  (is (.isConsumed click))
                  (is (empty? @calls)))))
            (.actionPerformed toggle nil)
            (is (not (minimap/hit? game x y)))
            (.actionPerformed toggle nil)
            (is (minimap/hit? game x y)))
          (finally ((:close! builder))))))))
