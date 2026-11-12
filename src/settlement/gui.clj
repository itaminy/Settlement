(ns settlement.gui
  (:require [settlement.client :as client]
            [settlement.rendering :as rendering])
  (:import [javax.swing JFrame JPanel JLabel SwingUtilities
            AbstractAction KeyStroke Timer JOptionPane]
           [java.awt Color Dimension BorderLayout FlowLayout Font RenderingHints GraphicsEnvironment]
           [java.awt.event ActionListener KeyEvent WindowAdapter]))

(def map-size rendering/map-size)

(defn update-map [panel players objects]
  (.putClientProperty panel ::scene (rendering/render-scene players objects))
  (.repaint panel))

(defn create-map-panel [_player-x _player-y]
  (let [panel
        (proxy [JPanel] []
          (paintComponent [graphics]
            (proxy-super paintComponent graphics)
            (when-let [image (.getClientProperty this ::scene)]
              (let [size (min (.getWidth this) (.getHeight this))
                    x (quot (- (.getWidth this) size) 2)
                    y (quot (- (.getHeight this) size) 2)
                    drawing (.create graphics)]
                (try
                  (.setRenderingHint drawing RenderingHints/KEY_INTERPOLATION
                                     RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
                  (.drawImage drawing image x y size size nil)
                  (finally (.dispose drawing)))))))]
    (.setBackground panel (Color. 40 55 35))
    (.setPreferredSize panel (Dimension. rendering/canvas-size rendering/canvas-size))
    (update-map panel {} {})
    panel))

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
(declare update-resource-panel)

(defn player-resources [state player-id]
  (get-in state [:players player-id :collected] {:wood 0 :stone 0}))

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
                      (update-resource-panel
                       resource-ui
                       (player-resources (:world state) player-id)))))))))))


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
                   (update-resource-panel resource-ui resources)))))))]
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

;; Создание игрового окна
(defn- confirm-dialog [message]
  (= JOptionPane/YES_OPTION
     (JOptionPane/showConfirmDialog nil message "Settlement" JOptionPane/YES_NO_OPTION)))

(defn- authorization-message [code]
  (case code
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
        map-panel (create-map-panel (:x player) (:y player))
        resource-ui (create-resource-panel (player-resources (:world response) player-id))
        timer (start-map-updater map-panel connection resource-ui player-id)]
    (doseq [[key direction] [[KeyEvent/VK_UP :up] [KeyEvent/VK_DOWN :down]
                             [KeyEvent/VK_LEFT :left] [KeyEvent/VK_RIGHT :right]]]
      (bind-key map-panel connection player-id key direction))
    (bind-gather-key map-panel connection resource-ui player-id)
    (update-map map-panel (get-in response [:world :players]) (get-in response [:world :objects]))
    (.setDefaultCloseOperation frame JFrame/DISPOSE_ON_CLOSE)
    (.addWindowListener frame
      (proxy [WindowAdapter] []
        (windowClosed [_]
          (.stop timer)
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
