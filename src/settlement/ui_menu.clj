(ns settlement.ui-menu
  (:require [settlement.ui-theme :as theme])
  (:import [java.awt BorderLayout GridLayout GridBagLayout GridBagConstraints Insets Color Dimension]
           [javax.swing JPanel JButton JLabel JTextField JComboBox JCheckBox JOptionPane BorderFactory SwingConstants JDialog]
           [java.awt.event ActionListener]))

(defn on-click! [button f]
  (.addActionListener button (reify ActionListener (actionPerformed [_ _] (f))))
  button)

(defn card [title]
  (let [panel (doto (theme/wood-panel (BorderLayout. 0 12))
                (.setBackground theme/wood) (.setBorder (theme/frame-border 18)))]
    (.add panel (theme/label title 24) BorderLayout/NORTH)
    panel))

(defn column []
  (doto (JPanel. (GridLayout. 0 1 0 7)) (.setOpaque false)))

(defn card-overlay [panel]
  (let [overlay (doto (JPanel. (GridBagLayout.)) (.setOpaque false))]
    (.add overlay panel (GridBagConstraints.))
    overlay))

(defn controls-panel []
  (let [panel (card "Управление")
        rows (column)]
    (doseq [[keys action] [["WASD / ↑ ↓ ← →" "Движение"]
                           ["E (удерживать)" "Добыча"]
                           ["B · ЛКМ" "Выбрать и разместить постройку"]
                           ["F / R · ЛКМ" "Атака / ремонт"]
                           ["I" "Инвентарь и крафт"]
                           ["1–9 / колесо" "Выбор ячейки"]
                           ["M / N / V" "Мини-карта / размер / эффекты"]
                           ["Esc" "Отменить размещение или открыть паузу"]]]
      (let [row (doto (JPanel. (BorderLayout. 14 0)) (.setOpaque false))]
        (.add row (doto (theme/label keys 13 theme/gold) (.setHorizontalAlignment SwingConstants/LEFT))
              BorderLayout/WEST)
        (.add row (doto (theme/label action 13) (.setHorizontalAlignment SwingConstants/RIGHT))
              BorderLayout/EAST)
        (.add rows row)))
    (.add panel rows BorderLayout/CENTER)
    panel))

(defn settings-panel []
  (let [current @theme/settings
        panel (card "Настройки")
        rows (column)
        choices [[:resolution "Размер окна" theme/resolutions]
                 [:scale "Масштаб интерфейса, %" [85 100 115 130]]
                 [:fps "Ограничение FPS" [30 60 120]]
                 [:minimap "Размер мини-карты" ["Малая" "Обычная" "Большая"]]]
        switches [[:fullscreen "Полноэкранный режим"] [:animations "Декоративные анимации"]
                  [:particles "Частицы"] [:damage-numbers "Числа урона"] [:names "Имена персонажей"]]
        inputs (into {} (for [[key label values] choices
                                    :let [field (doto (JComboBox. (to-array values))
                                                  (.setBackground theme/inset) (.setForeground theme/cream))
                                    row (doto (JPanel. (GridLayout. 1 2 14 0)) (.setOpaque false))]]
                          (do (.setSelectedItem field (if (= key :minimap)
                                                       ({:small "Малая" :normal "Обычная" :large "Большая"} (current key))
                                                       (current key)))
                              (.add row (theme/label label 13 theme/muted))
                              (.add row field)
                              (.add rows row)
                              [key field])))
        checks (into {} (for [[key label] switches
                              :let [field (doto (JCheckBox. label (boolean (current key)))
                                            (.setForeground theme/cream) (.setOpaque false))]]
                          (do (.add rows field) [key field])))]
    (.add panel rows BorderLayout/CENTER)
    {:panel panel
     :values (fn [] (into (into {} (for [[key field] checks] [key (.isSelected field)]))
                          (for [[key field] inputs
                                :let [value (.getSelectedItem field)]]
                            [key (if (= key :minimap)
                                   ({"Малая" :small "Обычная" :normal "Большая" :large} value) value)])))}))

(defn- dialog [owner title panel]
  (doto (JDialog. ^java.awt.Frame owner title true)
    (.setContentPane panel) (.setResizable false)
    (.setDefaultCloseOperation JDialog/DISPOSE_ON_CLOSE)))

(defn show-settings! [owner]
  (let [{:keys [panel values]} (settings-panel)
        previous (:scale @theme/settings)
        window (dialog owner "Настройки" panel)
        actions (doto (JPanel. (GridLayout. 1 2 8 0)) (.setOpaque false))]
    (.add actions (on-click! (theme/button "Отмена") #(.dispose window)))
    (.add actions (on-click! (theme/button "Сохранить")
                    #(try (theme/save! (values))
                          (.dispose window)
                          (theme/rescale! (.getRootPane owner) previous (:scale @theme/settings))
                          (theme/style-window! owner)
                          (catch Exception _ (JOptionPane/showMessageDialog window "Не удалось сохранить настройки.")))))
    (.add panel actions BorderLayout/SOUTH)
    (.pack window) (.setLocationRelativeTo window owner) (.setVisible window true)))

(defn show-controls! [owner]
  (let [panel (controls-panel)
        window (dialog owner "Управление" panel)]
    (.add panel (on-click! (theme/button "Закрыть") #(.dispose window)) BorderLayout/SOUTH)
    (.pack window) (.setLocationRelativeTo window owner) (.setVisible window true)))

(defn home-card [actions]
  (let [panel (doto (theme/wood-panel (BorderLayout. 0 13))
                (.setBackground theme/wood) (.setBorder (theme/frame-border 22)))
        rows (column)
        title (theme/wordmark)]
    (.add panel title BorderLayout/NORTH)
    (doseq [[key text] [[:play "Играть"] [:settings "Настройки"] [:controls "Управление"]
                        [:about "Об игре"] [:quit "Выход"]]]
      (.add rows (on-click! (theme/button text) (actions key))))
    (.add panel rows BorderLayout/CENTER)
    panel))

(defn connection-card [name host port submit back]
  (let [panel (card "Подключение")
        fields (column)
        name-field (JTextField. name 22)
        host-field (JTextField. host 22)
        port-field (JTextField. (str port) 22)
        message (theme/label " " 12 theme/muted)
        connect (theme/button "Подключиться")
        buttons (doto (JPanel. (GridLayout. 1 2 8 0)) (.setOpaque false))]
    (doseq [[caption field] [["Имя игрока" name-field] ["Адрес сервера" host-field] ["Порт" port-field]]]
      (let [row (doto (JPanel. (BorderLayout. 10 0)) (.setOpaque false))]
        (.add row (doto (theme/label caption 13 theme/muted) (.setPreferredSize (Dimension. 110 28))) BorderLayout/WEST)
        (.add row field BorderLayout/CENTER)
        (.add fields row)))
    (let [go (fn [] (submit {:name (.strip (.getText name-field)) :host (.strip (.getText host-field))
                             :port (parse-long (.strip (.getText port-field)))
                             :button connect :message message}))]
      (on-click! connect go)
      (doseq [field [name-field host-field port-field]] (on-click! field #(when (.isEnabled connect) (go)))))
    (.add fields message)
    (.add buttons (on-click! (theme/button "Назад") back))
    (.add buttons connect)
    (.add panel fields BorderLayout/CENTER)
    (.add panel buttons BorderLayout/SOUTH)
    {:panel panel :button connect :message message}))
