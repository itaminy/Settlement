(ns settlement.gui
  (:require [settlement.client :as client]
            [settlement.config :as config]
            [settlement.rendering :as rendering]
            [settlement.inventory :as inventory]
            [settlement.inventory-ui :as inventory-ui]
            [settlement.gathering-ui :as gathering-ui]
            [settlement.building-ui :as building-ui])
  (:import [javax.swing JFrame JDialog JPanel JLabel SwingUtilities
            AbstractAction KeyStroke Timer JOptionPane JProgressBar]
           [java.awt Color Dimension BorderLayout FlowLayout Font GraphicsEnvironment]
           [java.awt.event ActionListener KeyEvent WindowAdapter]))

(def map-size rendering/map-size)

(defn update-map [panel players objects]
  (.putClientProperty panel ::state {:players players :objects objects})
  (.repaint panel))

(defn create-map-panel
  ([player-x player-y] (create-map-panel player-x player-y nil))
  ([player-x player-y player-id]
   (let [panel
         (proxy [JPanel] []
           (paintComponent [graphics]
             (proxy-super paintComponent graphics)
             (when-let [state (.getClientProperty this ::state)]
               (let [local-id (.getClientProperty this ::player-id)
                     player (get-in state [:players local-id] {:x player-x :y player-y})
                     camera (rendering/camera player (.getWidth this) (.getHeight this))
                     key [state camera]
                     cached (.getClientProperty this ::render-cache)
                     image (if (= key (:key cached))
                             (:image cached)
                             (let [image (rendering/render-scene (:players state) (:objects state) camera)]
                               (.putClientProperty this ::render-cache {:key key :image image})
                               image))]
                 (.putClientProperty this ::camera camera)
                 (.putClientProperty this ::scene image)
                 (let [ox (quot (- (.getWidth this) (:width camera)) 2)
                       oy (quot (- (.getHeight this) (:height camera)) 2)]
                   (.drawImage graphics image ox oy nil)
                   (when-let [overlay (.getClientProperty this ::overlay)]
                     (overlay graphics camera ox oy)))))))]
     (.putClientProperty panel ::player-id player-id)
     (.setBackground panel (Color. 40 55 35))
     (.setPreferredSize panel (Dimension. config/viewport-width config/viewport-height))
     (update-map panel {} {})
     panel)))

(declare update-resource-panel update-player-panels!)

(defn player-resources [state player-id]
  (inventory/resources (inventory/player-inventory (get-in state [:players player-id]))))

(defn refresh! [panel connection resource-ui player-id]
  (client/async! connection :state {:type :state}
    (fn [response]
      (when (= :ok (:status response))
        (update-map panel (get-in response [:world :players]) (get-in response [:world :objects]))
        (update-player-panels! resource-ui (:world response) player-id)))))

(defn cancel-local-gather! [panel]
  (when-let [controller (.getClientProperty panel ::gather-controller)]
    ((:release! controller))))

(defn bind-key [panel connection player-id key-code direction]
  (let [key-name (str "move-" (name direction))]
    (.put (.getInputMap panel JPanel/WHEN_IN_FOCUSED_WINDOW) (KeyStroke/getKeyStroke key-code 0) key-name)
    (.put (.getActionMap panel) key-name
      (proxy [AbstractAction] []
        (actionPerformed [_]
          (cancel-local-gather! panel)
          (client/async! connection [:move direction]
            {:type :move :player-id player-id :direction direction}
            (fn [response]
              (when (= :ok (:status response))
                (update-map panel (get-in response [:world :players]) (get-in response [:world :objects]))))))))))

(defn bind-gather-key [panel connection resource-ui player-id]
  (let [progress (or (:progress-bar resource-ui) (JProgressBar. 0 100))
        controller (gathering-ui/create-controller connection player-id progress
                                                   #(refresh! panel connection resource-ui player-id))
        timer (Timer. 50 (proxy [ActionListener] [] (actionPerformed [_] ((:tick! controller)))))]
    (.putClientProperty panel ::gather-controller (assoc controller :timer timer))
    (doseq [[released? key action] [[false "gather" (:press! controller)] [true "gather-release" (:release! controller)]]]
      (.put (.getInputMap panel JPanel/WHEN_IN_FOCUSED_WINDOW)
            (KeyStroke/getKeyStroke KeyEvent/VK_E 0 released?) key)
      (.put (.getActionMap panel) key (proxy [AbstractAction] [] (actionPerformed [_] (action)))))
    (.start timer)
    timer))

(defn start-map-updater [panel connection resource-ui player-id]
  (let [timer (Timer. 1000 (proxy [ActionListener] []
                           (actionPerformed [_] (refresh! panel connection resource-ui player-id))))]
    (.start timer) timer))

;; Создание панели ресурсов
(defn create-resource-panel [resources]
  (let [panel (JPanel. (FlowLayout. FlowLayout/LEFT 20 10))
        separator (JLabel. "|")
        wood-label (JLabel.
                    (str "Моё дерево: " (:wood resources 0)))
        stone-label (JLabel.
                     (str "Мой камень: " (:stone resources 0)))]

    (.setBackground panel (Color. 65 85 55))

    (doseq [label [wood-label separator stone-label]]
      (.setForeground label Color/WHITE)
      (.setFont label (Font. "Arial" Font/BOLD 16))
      (.add panel label))

    {:panel panel
     :wood-label wood-label
     :stone-label stone-label}))


;; Обновление количества ресурсов
(defn update-resource-panel [resource-ui resources]
  (.setText (:wood-label resource-ui)
            (str "Моё дерево: " (:wood resources 0)))

  (.setText (:stone-label resource-ui)
            (str "Мой камень: " (:stone resources 0))))

(defn update-player-panels! [resource-ui state player-id]
  (let [contents (inventory/player-inventory (get-in state [:players player-id]))]
    (update-resource-panel resource-ui (inventory/resources contents))
    (when-let [ui (:inventory-ui resource-ui)]
      (inventory-ui/update-panel! ui contents))))

(defn bind-inventory-key [component toggle!]
  (.put (.getInputMap component JPanel/WHEN_IN_FOCUSED_WINDOW)
        (KeyStroke/getKeyStroke KeyEvent/VK_I 0) "inventory")
  (.put (.getActionMap component) "inventory"
        (proxy [AbstractAction] [] (actionPerformed [_] (toggle!)))))

;; Создание игрового окна
(defn- confirm-dialog [message]
  (= JOptionPane/YES_OPTION
     (JOptionPane/showConfirmDialog nil message "Settlement" JOptionPane/YES_NO_OPTION)))

(defn- authorization-message [code]
  (case code
    :map-full "На карте нет свободной клетки для нового персонажа."
    :already-connected "Этот персонаж уже подключён в другом окне."
    :player-exists "Персонаж с этим ID уже существует. Для входа нужен его сохранённый токен."
    :invalid-credentials "Данные входа не подходят. Персонаж не изменён."
    :credentials-save-failed "Данные входа не сохранены. Подключение закрыто."
    :credentials-unavailable "Не удалось прочитать данные входа или связаться с сервером."
    :cancelled "Создание нового персонажа отменено."
    :stale-server "Сервер был перезапущен. Повторите подключение."
    :invalid-registration "Нужны положительный числовой ID и непустое имя."
    "Не удалось выполнить вход."))

(defn bind-craft-buttons! [connection player-id resource-ui]
  (let [inventory-panel (:inventory-ui resource-ui)]
    (doseq [[tool button] (:craft-buttons inventory-panel)]
      (.addActionListener button
        (proxy [ActionListener] []
          (actionPerformed [_]
            (client/async! connection [:craft tool] {:type :craft :player-id player-id :tool tool}
              (fn [result]
                (if (= :ok (:status result))
                  (do (update-player-panels! resource-ui (:world result) player-id)
                      (.setText (:message inventory-panel) "Инструмент создан."))
                  (.setText (:message inventory-panel)
                            (if (= :insufficient-items (:code result)) "Недостаточно дерева или камня." "Не удалось создать инструмент.")))))))))
))

(defn- open-game-window [connection player-id response]
  (let [frame (JFrame. "Settlement")
        player (get-in response [:world :players player-id])
        map-panel (create-map-panel (:x player) (:y player) player-id)
        inventory-panel (inventory-ui/create-panel (inventory/player-inventory player))
        progress (doto (JProgressBar. 0 100) (.setStringPainted true) (.setString "Добыча: удерживайте E")
                       (.setForeground (Color. 132 164 83)) (.setBackground (Color. 45 59 39)) (.setVisible false))
        resource-ui (assoc (create-resource-panel (player-resources (:world response) player-id))
                           :inventory-ui inventory-panel :progress-bar progress)
        inventory-window (JDialog. frame "Инвентарь" false)
        build-label (doto (JLabel. "B — строительство") (.setForeground (Color. 240 230 193))
                          (.setPreferredSize (Dimension. 600 40)))
        footer (doto (JPanel. (BorderLayout.)) (.setBackground (Color. 45 59 39)))
        builder (building-ui/install! map-panel connection player-id build-label
                  (fn [state] (update-map map-panel (:players state) (:objects state))
                              (update-player-panels! resource-ui state player-id))
                  #(cancel-local-gather! map-panel))
        timer (start-map-updater map-panel connection resource-ui player-id)]
    (doseq [[key direction] [[KeyEvent/VK_UP :up] [KeyEvent/VK_DOWN :down]
                             [KeyEvent/VK_LEFT :left] [KeyEvent/VK_RIGHT :right]]]
      (bind-key map-panel connection player-id key direction))
    (bind-craft-buttons! connection player-id resource-ui)
    (.setContentPane inventory-window (:panel inventory-panel))
    (.setDefaultCloseOperation inventory-window JDialog/HIDE_ON_CLOSE)
    (.setResizable inventory-window false)
    (.pack inventory-window)
    (let [toggle! (fn []
                    (cancel-local-gather! map-panel)
                    (if (.isVisible inventory-window)
                      (do (.setVisible inventory-window false) (.requestFocusInWindow map-panel))
                      (do (refresh! map-panel connection resource-ui player-id)
                          (.setLocationRelativeTo inventory-window frame)
                          (.setVisible inventory-window true))))]
      (bind-inventory-key map-panel toggle!)
      (bind-inventory-key (.getRootPane inventory-window) toggle!))
    (bind-gather-key map-panel connection resource-ui player-id)
    (update-map map-panel (get-in response [:world :players]) (get-in response [:world :objects]))
    (.addWindowFocusListener frame
      (proxy [java.awt.event.WindowFocusListener] []
        (windowGainedFocus [_] nil)
        (windowLostFocus [_] (cancel-local-gather! map-panel))))
    (.setDefaultCloseOperation frame JFrame/DISPOSE_ON_CLOSE)
    (.addWindowListener frame
      (proxy [WindowAdapter] []
        (windowClosed [_]
          (.stop timer)
          ((:close! builder))
          (when-let [controller (.getClientProperty map-panel ::gather-controller)]
            ((:release! controller)) (.stop (:timer controller)))
          (.dispose inventory-window)
          (client/close-connection connection))))
    (.setLayout (.getContentPane frame) (BorderLayout.))
    (.add (.getContentPane frame) (:panel resource-ui) BorderLayout/NORTH)
    (.add (.getContentPane frame) map-panel BorderLayout/CENTER)
    (.add footer progress BorderLayout/NORTH)
    (.add footer build-label BorderLayout/SOUTH)
    (.add (.getContentPane frame) footer BorderLayout/SOUTH)
    (.pack frame)
    (let [bounds (.getMaximumWindowBounds (GraphicsEnvironment/getLocalGraphicsEnvironment))]
      (.setSize frame (min (.getWidth frame) (.width bounds))
                      (min (.getHeight frame) (.height bounds))))
    (.setLocationRelativeTo frame nil)
    (.setVisible frame true)
    (.requestFocusInWindow map-panel)))

(defn- on-edt [f]
  (if (SwingUtilities/isEventDispatchThread) (f)
    (let [result (promise)]
      (SwingUtilities/invokeAndWait #(deliver result (f)))
      @result)))

(defn show-window [player-id player-name]
  (future
    (try
      (let [connection (client/connect)]
        (try
          (let [result (client/authorize!
                        connection player-id player-name
                        #(on-edt (fn [] (confirm-dialog "Сервер перезапущен. Создать нового персонажа? Прежний мир утрачен.")))
                        #(on-edt (fn [] (confirm-dialog "Не удалось сохранить данные входа. Повторить сохранение?"))))]
            (if (= :ok (:status result))
              (let [response (client/request connection {:type :state})]
                (SwingUtilities/invokeLater #(open-game-window connection player-id response)))
              (do (client/close-connection connection)
                  (on-edt #(JOptionPane/showMessageDialog nil (authorization-message (:code result)))))))
          (catch Exception _
            (client/close-connection connection)
            (on-edt #(JOptionPane/showMessageDialog nil "Не удалось открыть игру.")))))
      (catch Exception _ (on-edt #(JOptionPane/showMessageDialog nil "Нет соединения с сервером.")))))
  nil)
