
(ns settlement.gui
  (:require [settlement.world :as world]
            [settlement.client :as client])
  (:import [javax.swing JFrame JPanel JLabel SwingConstants SwingUtilities
          AbstractAction KeyStroke Timer ImageIcon]
         [javax.swing.border LineBorder]
         [java.awt Color Dimension GridLayout BorderLayout FlowLayout Font]
         [java.awt.image BufferedImage]
         [java.awt.event ActionEvent ActionListener KeyEvent]))

;; Размер карты
(def map-size 12)
;; Загрузка изображений игровых объектов
;; Загрузка и масштабирование изображений
(defn load-icon [path width height]
  (let [image (.getImage (ImageIcon. path))
        scaled (.getScaledInstance
                image
                width
                height
                java.awt.Image/SCALE_REPLICATE)]
    (ImageIcon. scaled)))

(def tree-icon
  (load-icon "resources/images/tree.png" 44 44))

(def stone-icon
  (load-icon "resources/images/stone.png" 44 44))

(def house-icon
  (load-icon "resources/images/house.png" 44 44))

;; Текстуры поверхности карты
(def grass-icon
  (load-icon "resources/images/grass.png" 44 44))

(def dirt-icon
  (load-icon "resources/images/dirt.png" 44 44))


;; Объединение текстуры поверхности и игрового объекта
(defn create-tile-icon [background-icon object-icon]
  (let [image (BufferedImage.
               44 44
               BufferedImage/TYPE_INT_ARGB)
        graphics (.createGraphics image)]

    ;; Сначала рисуем траву или землю
    (.drawImage graphics
                (.getImage background-icon)
                0 0 nil)

    ;; Затем рисуем объект поверх поверхности
    (when object-icon
      (.drawImage graphics
                  (.getImage object-icon)
                  0 0 nil))

    (.dispose graphics)

    (ImageIcon. image)))

(defn update-map [panel players objects]
  (let [cells (.getComponents panel)]
    (doseq [i (range (count cells))]
      (let [cell (aget cells i)
            x (mod i map-size)
            y (quot i map-size)
            player (first
                    (filter
                     (fn [[id p]]
                       (and (= x (:x p))
                            (= y (:y p))))
                     players))
            object (get objects [x y])]

        (.setBackground cell
          (if player
            (Color. 100 170 100)
            (Color. 225 235 215)))


;; Выбираем поверхность и объект
(let [background-icon
      (if (or (= y 6)
              (and (= x 4) (<= 3 y 8)))
        dirt-icon
        grass-icon)

      object-icon
      (case object
        :tree tree-icon
        :stone stone-icon
        :house house-icon
        nil)]

  ;; Рисуем поверхность и объект вместе
  (.setIcon cell
    (create-tile-icon
      background-icon
      (when-not player object-icon))))


;; Отображаем номер игрока
(.setText cell
  (if player
    (str (first player))
    ""))))))

;; Создание игрового поля
(defn create-map-panel [player-x player-y]
  (let [panel (JPanel. (GridLayout. map-size map-size))]

    (dotimes [i (* map-size map-size)]
      (let [cell (JLabel. "" SwingConstants/CENTER)]

        (.setOpaque cell true)
        (.setHorizontalAlignment cell SwingConstants/CENTER)
        (.setVerticalAlignment cell SwingConstants/CENTER)
        (.setHorizontalTextPosition cell SwingConstants/CENTER)
        (.setVerticalTextPosition cell SwingConstants/CENTER)

        (let [x (mod i map-size)
            y (quot i map-size)]
            (if (and (= x player-x)
                (= y player-y))
        (.setBackground cell (Color. 100 170 100))
        (.setBackground cell (Color. 225 235 215))))

        (.setBorder cell (LineBorder. (Color. 170 180 160)))
        (.setPreferredSize cell (Dimension. 45 45))

        (let [x (mod i map-size)
            y (quot i map-size)]
        (when (and (= x player-x)
             (= y player-y))
          (.setText cell "P")))

        (.add panel cell)))

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
                      (update-resource-panel
                       resource-ui
                       (get-in state [:world :resources])))))))))))


;; Обновление количества ресурсов
(defn update-resource-panel [resource-ui resources]
  (.setText (:wood-label resource-ui)
            (str "Дерево: " (:wood resources 0)))

  (.setText (:stone-label resource-ui)
            (str "Камень: " (:stone resources 0))))


(defn start-map-updater [panel connection resource-ui]
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
                       resources (get-in response [:world :resources])]

                   ;; Обновляем карту
                   (update-map panel players objects)

                   ;; Обновляем панель ресурсов
                   (update-resource-panel resource-ui resources)))))))]
    (.start timer)))



;; Создание панели ресурсов
(defn create-resource-panel [resources]
  (let [panel (JPanel. (FlowLayout. FlowLayout/LEFT 20 10))
        wood-label (JLabel.
                    (str "Дерево: " (:wood resources 0)))
        stone-label (JLabel.
                     (str "Камень: " (:stone resources 0)))]

    (.setBackground panel (Color. 65 85 55))

    (doseq [label [wood-label stone-label]]
      (.setForeground label Color/WHITE)
      (.setFont label (Font. "Arial" Font/BOLD 16))
      (.add panel label))

    {:panel panel
     :wood-label wood-label
     :stone-label stone-label}))


;; Обновление количества ресурсов
(defn update-resource-panel [resource-ui resources]
  (.setText (:wood-label resource-ui)
            (str "Дерево: " (:wood resources 0)))

  (.setText (:stone-label resource-ui)
            (str "Камень: " (:stone resources 0))))

;; Создание игрового окна
(defn show-window [player-id player-name]
  (SwingUtilities/invokeLater
   (fn []
     (let [frame (JFrame. "Settlement")
           connection (client/connect)

           registration
           (client/send-message
            (:writer connection)
            (:reader connection)
            {:type :register
             :player-id player-id
             :name player-name})

           response
           (client/send-message
            (:writer connection)
            (:reader connection)
            {:type :state})

            player (get-in (:world response) [:players player-id])
            map-panel (create-map-panel (:x player) (:y player))

            resource-ui (create-resource-panel
            (get-in response [:world :resources]))]
            (bind-key map-panel connection player-id KeyEvent/VK_UP :up)
            (bind-key map-panel connection player-id KeyEvent/VK_DOWN :down)
            (bind-key map-panel connection player-id KeyEvent/VK_LEFT :left)
            (bind-key map-panel connection player-id KeyEvent/VK_RIGHT :right)
            (start-map-updater map-panel connection resource-ui)
            (bind-gather-key map-panel connection resource-ui player-id)

       (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
       (.setLayout (.getContentPane frame)
            (BorderLayout.))

        (.add (.getContentPane frame)
          (:panel resource-ui)
          BorderLayout/NORTH)

        (.add (.getContentPane frame)
          map-panel
          BorderLayout/CENTER)
       (.setFocusable map-panel true)
       
       (.pack frame)
       (.setLocationRelativeTo frame nil)
       (.setVisible frame true)
       (.requestFocusInWindow map-panel)))))