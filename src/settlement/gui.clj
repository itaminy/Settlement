(ns settlement.gui
  (:require [settlement.client :as client]
            [settlement.config :as config]
            [settlement.rendering :as rendering]
            [settlement.inventory :as inventory]
            [settlement.inventory-ui :as inventory-ui])
  (:import [javax.swing JFrame JDialog JPanel JLabel SwingUtilities
            AbstractAction KeyStroke Timer JOptionPane]
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
                 (.drawImage graphics image
                             (quot (- (.getWidth this) (:width camera)) 2)
                             (quot (- (.getHeight this) (:height camera)) 2) nil)))))]
     (.putClientProperty panel ::player-id player-id)
     (.setBackground panel (Color. 40 55 35))
     (.setPreferredSize panel (Dimension. config/viewport-width config/viewport-height))
     (update-map panel {} {})
     panel)))

(defn bind-key [panel connection player-id key-code direction]
  (let [input-map (.getInputMap panel JPanel/WHEN_IN_FOCUSED_WINDOW)
        action-map (.getActionMap panel)
        key-name (str "move-" (name direction))]

    (.put input-map
          (KeyStroke/getKeyStroke key-code 0)
          key-name)

    (.put action-map
          key-name
          (proxy [AbstractAction] []
            (actionPerformed [e]
              (let [response
                    (client/send-message
                     (:writer connection)
                     (:reader connection)
                     {:type :move
                      :player-id player-id
                      :direction direction})]

                (when (= :ok (:status response))
                  (let [players (get-in response [:world :players])
      objects (get-in response [:world :objects])]
  (update-map panel players objects)))))))))


;; Объявляем функцию обновления ресурсов
(declare update-resource-panel update-player-panels!)

(defn player-resources [state player-id]
  (inventory/resources (inventory/player-inventory (get-in state [:players player-id]))))

;; Добыча ресурсов по клавише E
(defn bind-gather-key [panel connection resource-ui player-id]
  (let [input-map (.getInputMap panel
                                javax.swing.JComponent/WHEN_IN_FOCUSED_WINDOW)
        action-map (.getActionMap panel)]

    (.put input-map
          (KeyStroke/getKeyStroke KeyEvent/VK_E 0)
          "gather")

    (.put action-map
          "gather"
          (proxy [AbstractAction] []
            (actionPerformed [e]
              (let [response
                    (client/send-message
                     (:writer connection)
                     (:reader connection)
                     {:type :gather
                      :player-id player-id})]

                (when (= :ok (:status response))
                  (let [state
                        (client/send-message
                         (:writer connection)
                         (:reader connection)
                         {:type :state})]

                    (when (= :ok (:status state))
                      (update-map panel (get-in state [:world :players]) (get-in state [:world :objects]))
                      (update-player-panels! resource-ui (:world state) player-id))))))))))


(defn start-map-updater [panel connection resource-ui player-id]
  (let [timer
        (Timer.
         1000
         (proxy [ActionListener] []
           (actionPerformed [e]
             (let [response
                   (client/send-message
                    (:writer connection)
                    (:reader connection)
                    {:type :state})]

               (when (= :ok (:status response))
                 (let [players (get-in response [:world :players])
                       objects (get-in response [:world :objects])
                       resources (player-resources (:world response) player-id)]

                   ;; Обновляем карту
                   (update-map panel players objects)

                   ;; Обновляем панель ресурсов
                   (update-player-panels! resource-ui (:world response) player-id)))))))]
    (.start timer)
    timer))



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

(defn- open-game-window [connection player-id]
  (let [frame (JFrame. "Settlement")
        response (client/request connection {:type :state})
        player (get-in response [:world :players player-id])
        map-panel (create-map-panel (:x player) (:y player) player-id)
        inventory-panel (inventory-ui/create-panel (inventory/player-inventory player))
        resource-ui (assoc (create-resource-panel (player-resources (:world response) player-id))
                           :inventory-ui inventory-panel)
        inventory-window (JDialog. frame "Инвентарь" false)
        timer (start-map-updater map-panel connection resource-ui player-id)]
    (doseq [[key direction] [[KeyEvent/VK_UP :up] [KeyEvent/VK_DOWN :down]
                             [KeyEvent/VK_LEFT :left] [KeyEvent/VK_RIGHT :right]]]
      (bind-key map-panel connection player-id key direction))
    (.setContentPane inventory-window (:panel inventory-panel))
    (.setDefaultCloseOperation inventory-window JDialog/HIDE_ON_CLOSE)
    (.setResizable inventory-window false)
    (.pack inventory-window)
    (let [toggle! (fn []
                    (if (.isVisible inventory-window)
                      (do (.setVisible inventory-window false) (.requestFocusInWindow map-panel))
                      (let [state (client/request connection {:type :state})]
                        (when (= :ok (:status state))
                          (update-player-panels! resource-ui (:world state) player-id))
                        (.setLocationRelativeTo inventory-window frame)
                        (.setVisible inventory-window true))))]
      (bind-inventory-key map-panel toggle!)
      (bind-inventory-key (.getRootPane inventory-window) toggle!))
    (bind-gather-key map-panel connection resource-ui player-id)
    (update-map map-panel (get-in response [:world :players]) (get-in response [:world :objects]))
    (.setDefaultCloseOperation frame JFrame/DISPOSE_ON_CLOSE)
    (.addWindowListener frame
      (proxy [WindowAdapter] []
        (windowClosed [_]
          (.stop timer)
          (.dispose inventory-window)
          (client/close-connection connection))))
    (.setLayout (.getContentPane frame) (BorderLayout.))
    (.add (.getContentPane frame) (:panel resource-ui) BorderLayout/NORTH)
    (.add (.getContentPane frame) map-panel BorderLayout/CENTER)
    (.pack frame)
    (let [bounds (.getMaximumWindowBounds (GraphicsEnvironment/getLocalGraphicsEnvironment))]
      (.setSize frame (min (.getWidth frame) (.width bounds))
                      (min (.getHeight frame) (.height bounds))))
    (.setLocationRelativeTo frame nil)
    (.setVisible frame true)
    (.requestFocusInWindow map-panel)))

(defn show-window [player-id player-name]
  (SwingUtilities/invokeLater
   (fn []
     (try
       (let [connection (client/connect)]
         (try
           (let [result (client/authorize!
                         connection player-id player-name
                         #(confirm-dialog "Сервер перезапущен. Прежний прогресс утрачен. Создать нового персонажа?")
                         #(confirm-dialog "Не удалось сохранить данные входа. Повторить сохранение? При отказе доступ к новому персонажу может быть потерян."))]
             (if (= :ok (:status result))
               (open-game-window connection player-id)
               (do
                 (client/close-connection connection)
                 (JOptionPane/showMessageDialog nil
                   (authorization-message (:code result)) "Settlement" JOptionPane/ERROR_MESSAGE))))
           (catch Exception _
             (client/close-connection connection)
             (JOptionPane/showMessageDialog nil "Не удалось открыть игру."))))
       (catch Exception _
         (JOptionPane/showMessageDialog nil "Нет соединения с сервером."))))))
