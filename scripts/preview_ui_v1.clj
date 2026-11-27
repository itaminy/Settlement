(require '[clojure.java.io :as io]
         '[settlement.auth :as auth]
         '[settlement.buildings :as buildings]
         '[settlement.client :as client]
         '[settlement.credentials :as credentials]
         '[settlement.gui :as gui]
         '[settlement.layout :as layout]
         '[settlement.rendering :as rendering]
         '[settlement.server :as server]
         '[settlement.ui-theme :as theme]
         '[settlement.world :as world])
(import '[java.awt Frame Container GraphicsEnvironment]
        '[java.awt.image BufferedImage]
        '[java.awt.event MouseEvent KeyEvent]
        '[java.net InetAddress ServerSocket]
        '[java.nio.file Files]
        '[java.nio.file.attribute FileAttribute]
        '[javax.imageio ImageIO]
        '[javax.swing AbstractButton JComboBox JDialog JFrame JLabel JPanel JTextField SwingUtilities])

(def out (io/file "docs/previews/ui_v1"))
(.mkdirs out)
(def id 9909)
(def connection (atom nil))

(defn edt! [f]
  (if (SwingUtilities/isEventDispatchThread)
    (f)
    (let [v (volatile! nil)]
      (SwingUtilities/invokeAndWait #(vreset! v (f)))
      @v)))

(defn poll [title f]
  (let [deadline (+ (System/nanoTime) 10000000000)]
    (loop []
      (if-let [value (f)] value
        (if (< (System/nanoTime) deadline)
          (do (Thread/sleep 35) (recur))
          (throw (ex-info "UI validation timed out" {:stage title})))))))

(defn ui-tree [component]
  (tree-seq #(instance? Container %) #(.getComponents ^Container %) component))

(defn button [root text]
  (first (filter #(and (instance? AbstractButton %) (= text (.getText %)) (.isShowing %))
                 (ui-tree root))))

(defn click! [root text]
  (let [target (poll text #(edt! (fn [] (button root text))))]
    (edt! #(.doClick target 0))))

(defn click-async! [root text]
  (let [target (poll text #(edt! (fn [] (button root text))))]
    (SwingUtilities/invokeLater #(.doClick target 0))))

(defn visible-frame [title]
  (poll title #(edt! (fn [] (first (filter (fn [frame]
                                            (and (.isVisible frame) (= title (.getTitle frame))))
                                          (Frame/getFrames)))))))

(defn owned-dialog [frame title]
  (poll title #(edt! (fn [] (first (filter (fn [w] (and (instance? JDialog w) (.isVisible w)
                                                       (= title (.getTitle ^JDialog w))))
                                          (.getOwnedWindows frame)))))))

(defn capture! [frame stem]
  (Thread/sleep 180)
  (let [image (edt! #(let [root (.getRootPane frame)
                           image (BufferedImage. (.getWidth root) (.getHeight root) BufferedImage/TYPE_INT_RGB)
                           g (.createGraphics image)]
                       (try (.printAll root g) (finally (.dispose g)))
                       image))]
    (ImageIO/write image "png" (io/file out (str stem ".png")))
    (println stem [(.getWidth image) (.getHeight image)])))

(defn dismiss! [dialog]
  (edt! #(.dispose dialog)))

(defn connect! [home port capture?]
  (click! home "Играть")
  (edt! (fn []
          (let [fields (filter #(and (instance? JTextField %) (.isShowing %)) (ui-tree home))]
            (assert (= 3 (count fields)))
            (.setText ^JTextField (last fields) (str port)))))
  (when capture? (capture! home "connection"))
  (click! home "Подключиться")
  (visible-frame "Settlement"))

(defn field-text? [frame fragment]
  (edt! #(some (fn [x] (and (instance? JLabel x) (.isShowing x)
                             (clojure.string/includes? (or (.getText x) "") fragment)))
               (ui-tree frame))))

(defn map-panel [game]
  (edt! #(first (filter (fn [x] (and (instance? JPanel x)
                                     (= id (.getClientProperty x :settlement.gui/player-id))))
                        (ui-tree game)))))

(defn action! [component key]
  (edt! #(.actionPerformed (.get (.getActionMap component) key) nil)))

(defn spot! []
  (let [positions (for [x (range 17 33) y (range 19 34)] [x y])
        base @world/world
        house-site (first (for [[x y :as p] positions
                                :when (and (not (get-in base [:objects [(dec x) y]]))
                                           (= :ok (:status (buildings/validate
                                                            (update-in base [:players id] assoc :x (dec x) :y y)
                                                            id :house p))))]
                            [(dec x) y p]))]
    (assert house-site)
    (let [[x y p] house-site]
      (dosync (alter world/world update-in [:players id] assoc :x x :y y))
      p)))

(defn pause! [game]
  (action! (.getRootPane game) "pause")
  (poll :pause #(edt! (fn [] (.isVisible (.getGlassPane game))))))

(defn resize! [frame w h]
  (edt! #(do (.setSize frame w h) (.setLocationRelativeTo frame nil) (.validate frame)))
  (Thread/sleep 110)
  (assert (edt! #(let [map (map-panel frame)] (and (<= 400 (.getWidth map)) (<= 220 (.getHeight map)))))))

(defn result! [winner]
  (dosync
    (alter world/world assoc :match {:phase :finished :winner winner
                                     :scores {:blue (if (= winner :blue) 100 40)
                                              :red (if (= winner :red) 100 65)}}
           :match-ended-at (world/combat-time))))

(defn verify-ui! [port]
  (gui/show-window id "Ратмир")
  (let [home (visible-frame "Settlement · Главное меню")]
    (capture! home "menu")
    (doseq [[name stem] [["Настройки" "settings"] ["Управление" "controls"]]]
      (click-async! home name)
      (let [dialog (owned-dialog home name)]
        (capture! dialog stem)
        (dismiss! dialog)))
    (doseq [[resolution fps width] [["896×640" 120 896] ["1280×720" 60 1280]]]
      (click-async! home "Настройки")
      (let [window (owned-dialog home "Настройки")]
        (edt! #(let [choices (vec (filter (fn [component] (instance? JComboBox component)) (ui-tree window)))]
                 (.setSelectedItem ^JComboBox (choices 0) resolution)
                 (.setSelectedItem ^JComboBox (choices 2) fps)))
        (click! window "Сохранить")
        (poll :settings-applied #(and (= fps (:fps @theme/settings))
                                      (= width (edt! (fn [] (.getWidth home))))))
        (assert (= resolution (:resolution (theme/load!))))))
    (let [game (connect! home port true)
          panel (map-panel game)
          original-position ((juxt :x :y) (get-in @world/world [:players id]))]
      (assert (= 1 (count (:players @world/world))))
      (poll :connected #(field-text? game "Подключено"))
      (world/change-player-items! id {:wood 24 :stone 12 :axe 1})
      (capture! game "hud")
      (action! panel "inventory")
      (let [dialog (owned-dialog game "Инвентарь")]
        (capture! dialog "inventory")
        (edt! #(.setVisible dialog false)))
      (let [[gx gy] (spot!)]
        (Thread/sleep 380)
        (action! panel "building-menu")
        (capture! game "construction")
        (let [entry (poll :house #(edt! (fn []
                                         (first (filter (fn [x] (and (instance? javax.swing.JMenuItem x)
                                                                      (clojure.string/includes? (.getText x) "Деревянный дом")))
                                                        (mapcat ui-tree (.getComponents (.getLayeredPane game))))))))]
          (edt! #(.doClick entry 0)))
        (edt! #(let [camera (.getClientProperty panel :settlement.gui/camera)
                     [cx cy] (rendering/raster-origin camera)]
                 (.dispatchEvent panel (MouseEvent. panel MouseEvent/MOUSE_MOVED
                                                   (System/currentTimeMillis) 0
                                                   (int (- (+ (* gx 64) 32) cx))
                                                   (int (- (+ (* gy 64) 32) cy)) 0 false))))
        (poll :server-build-check #(field-text? game "ЛКМ — построить"))
        (capture! game "placement")
        (action! panel "cancel-building"))
      (pause! game)
      (assert (not (edt! #(.isEnabled (.get (.getActionMap panel) "building-menu")))))
      (let [before (:match-duration-ms (world/get-world))]
        (Thread/sleep 210)
        (assert (> (:match-duration-ms (world/get-world)) before)))
      (capture! game "pause")
      (click! game "Продолжить игру")
      (resize! game 896 640)
      (capture! game "compact")
      (resize! game 1280 720)
      (resize! game 1920 1080)
      (let [device (.getDefaultScreenDevice (GraphicsEnvironment/getLocalGraphicsEnvironment))]
        (edt! #(.setFullScreenWindow device game))
        (capture! game "fullscreen")
        (edt! #(.setFullScreenWindow device nil)))
      (resize! game 1280 720)
      (result! :blue)
      (poll :victory #(field-text? game "Победа!"))
      (capture! game "victory")
      (let [before (world/match-id @world/world)]
        (click! game "Новая игра")
        (poll :restart #(> (world/match-id @world/world) before))
        (assert (= {:blue 0 :red 0} (get-in @world/world [:match :scores]))))
      (result! :red)
      (poll :defeat #(field-text? game "Поражение!"))
      (capture! game "defeat")
      (click! game "В главное меню")
      (visible-frame "Settlement · Главное меню")
      (poll :closed #(not (contains? (:sessions @auth/state) id)))
      (let [rejoined (connect! home port false)]
        (assert (= 1 (count (:players @world/world))))
        (assert (= original-position ((juxt :x :y) (get-in @world/world [:players id]))))
        (.close (:socket @connection))
        (poll :connection-lost #(field-text? rejoined "Связь потеряна"))
        (pause! rejoined)
        (click! rejoined "В главное меню")
        (visible-frame "Settlement · Главное меню")
        (poll :disconnected #(not (contains? (:sessions @auth/state) id)))
        (let [third-game (connect! home port false)]
          (assert (= 1 (count (:players @world/world))))
          (edt! #(.dispose third-game))))
      (edt! #(.dispose home))
      (println :authorization-pause-restart-reconnect-verified))))

(let [dir (.toFile (Files/createTempDirectory "settlement-stage9-" (make-array FileAttribute 0)))
      original-connect client/connect]
  (with-redefs [credentials/*credentials-file* (str (io/file dir "credentials.edn"))
                theme/*settings-file* (str (io/file dir "ui.edn"))
                client/connect (fn [host port] (reset! connection (original-connect host port)))]
    (with-open [listener (ServerSocket. 0 16 (InetAddress/getLoopbackAddress))]
      (let [acceptor (future (server/serve! listener))]
        (try
          (verify-ui! (.getLocalPort listener))
          (finally
            (edt! #(doseq [frame (Frame/getFrames)] (.dispose frame)))
            (.close listener)
            (deref acceptor 3000 nil)
            (shutdown-agents)))))))
