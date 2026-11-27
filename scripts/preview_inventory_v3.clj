(require '[clojure.java.io :as io]
         '[settlement.building-ui :as building-ui]
         '[settlement.gui :as gui]
         '[settlement.layout :as layout]
         '[settlement.world :as world])
(import '[java.awt Color Dimension GraphicsEnvironment]
        '[java.awt.image BufferedImage]
        '[javax.imageio ImageIO]
        '[javax.swing JFrame JLabel JProgressBar SwingUtilities])

(def output (io/file "docs/previews/inventory_v3"))
(.mkdirs output)

(defn capture! [panel file]
  (let [image (BufferedImage. (.getWidth panel) (.getHeight panel) BufferedImage/TYPE_INT_ARGB)
        graphics (.createGraphics image)]
    (try (.printAll panel graphics) (finally (.dispose graphics)))
    (ImageIO/write image "png" (io/file output file))
    (println file (.getWidth image) (.getHeight image))))

(defn display-state [stock]
  (let [[x y] layout/spawn-position]
    (assoc (world/get-world)
           :players {1 {:x x :y y :team :blue :name "Settlement"
                        :inventory {:items stock}}})))

(SwingUtilities/invokeAndWait
 (fn []
   (when (GraphicsEnvironment/isHeadless)
     (throw (ex-info "A Swing desktop is required for these previews." {})))
   (let [[x y] layout/spawn-position
         window (JFrame. "Settlement - inventory")
         terrain (gui/create-map-panel x y 1)
         ui (gui/create-resource-panel {})
         progress (doto (JProgressBar. 0 100) (.setVisible false) (.setStringPainted true)
                    (.setForeground (Color. 132 164 83)) (.setBackground (Color. 45 59 39)))
         status (doto (JLabel. building-ui/controls-text)
                  (.setForeground (Color. 240 230 193)) (.setPreferredSize (Dimension. 600 40)))
         {:keys [panel]} (gui/create-game-content terrain ui progress status)
         contents {:wood 48 :stone 23 :axe 1 :pickaxe 1}
         show! (fn [state]
                 (gui/update-map terrain (:players state) (:objects state))
                 (gui/update-player-panels! ui state 1))]
     (try
       (.setContentPane window panel)
       (.setSize window 912 679)
       (.setLocationRelativeTo window nil)
       (.setVisible window true)
       (.validate window)
       (show! (display-state {}))
       (capture! panel "empty.png")
       (show! (display-state contents))
       (let [bar (get-in ui [:hotbar :panel])]
         (.actionPerformed (.get (.getActionMap bar) "hotbar-3") nil))
       (capture! panel "filled.png")
       (.setSize window 436 456)
       (.setString progress "E")
       (.setValue progress 65)
       (.setVisible progress true)
       (.validate window)
       (capture! panel "compact.png")
       (let [device (.getDefaultScreenDevice (GraphicsEnvironment/getLocalGraphicsEnvironment))]
         (.dispose window)
         (.setUndecorated window true)
         (.setBounds window (.getBounds (.getDefaultConfiguration device)))
         (.setVisible progress false)
         (.setVisible window true)
         (.validate window)
         (capture! panel "fullscreen.png"))
       (finally (.dispose window))))))
