(ns settlement.building-ui
  (:require [settlement.client :as client]
            [settlement.buildings :as buildings]
            [settlement.inventory :as inventory]
            [settlement.rendering :as rendering])
  (:import [javax.swing JPopupMenu JMenuItem KeyStroke AbstractAction JPanel Timer JComponent ToolTipManager]
           [java.awt Color Font AlphaComposite RenderingHints]
           [java.awt.event KeyEvent MouseAdapter MouseMotionAdapter ActionListener]
           [java.beans PropertyChangeListener]))

(def controls-text "B — строительство · F — атака · R — ремонт")
(defonce ^:private command-sequences (atom {}))

(def ^:private action-modes
  {:attack {:type :attack-building :seq-key :attack-seq :server-seq :last-attack-seq
            :hint "Атака: выберите вражескую постройку."}
   :repair {:type :repair-building :seq-key :repair-seq :server-seq :last-repair-seq
            :hint "Ремонт: выберите союзную постройку."}})

(def ^:private action-messages
  {:too-far "Встаньте рядом с постройкой."
   :insufficient-items "Для ремонта нужно 1 дерево."
   :attack-cooldown "Следующий удар возможен через секунду после предыдущего."
   :building-not-found "Постройка не найдена."
   :stale-building "Постройка уже изменилась. Выберите цель снова."
   :building-at-full-hp "Постройка уже полностью исправна."
   :match-finished "Матч завершён."
   :stale-attack "Номер команды обновлён. Нажмите ещё раз."
   :attack-seq-conflict "Номер команды обновлён. Нажмите ещё раз."
   :stale-repair "Номер команды обновлён. Нажмите ещё раз."
   :repair-seq-conflict "Номер команды обновлён. Нажмите ещё раз."
   :connection-failed "Связь с сервером потеряна."})

(defn- action-message [mode result]
  (cond
    (:destroyed? result) "Постройка разрушена."
    (= :ok (:status result)) (if (= :attack mode)
                               (str "Удар нанесён. Прочность: " (:hp result) " HP.")
                               (str "Восстановлено " (:restored-hp result) " HP."))
    (= :invalid-target-team (:code result)) (if (= :attack mode)
                                              "Атаковать можно только вражеские постройки."
                                              "Ремонтировать можно только союзные постройки.")
    :else (get action-messages (:code result) "Не удалось выполнить действие.")))

(defn- building-tooltip [{:keys [kind team hp max-hp]}]
  (when (and kind (some? hp) (some? max-hp))
    (str (get-in buildings/catalog [kind :name]) " ("
         (get {:blue "синие" :red "красные"} team "без команды") ") — " hp "/" max-hp " HP")))

(def messages
  {:out-of-bounds "Постройка выходит за границы мира."
   :object-blocked "Место занято объектом или постройкой."
   :player-blocked "На участке находится игрок."
   :protected-area "Дорожки, стартовая область и важные подходы защищены."
   :too-far "Подойдите ближе: не более двух клеток."
   :insufficient-items "Недостаточно ресурсов."
   :blocks-access "Будет перекрыт доступ к игроку, ресурсу или важной зоне."
   :queue-full "Сеть занята. Повторите попытку."})

(defn screen-to-cell [camera panel-width panel-height sx sy]
  (let [x (- sx (quot (- panel-width (:width camera)) 2))
        y (- sy (quot (- panel-height (:height camera)) 2))]
    (when (and (<= 0 x) (< x (:width camera)) (<= 0 y) (< y (:height camera)))
      [(quot (+ x (:x camera)) rendering/tile-size)
       (quot (+ y (:y camera)) rendering/tile-size)])))

(defn draw-preview! [graphics camera ox oy kind position status]
  (let [g (.create graphics)
        entry {:type :building :kind kind}
        {:keys [image x y]} (rendering/building-placement position entry false)
        color (case status :ok (Color. 85 210 110) :pending (Color. 190 165 80) (Color. 235 80 65))]
    (try
      (.translate g (- ox (:x camera)) (- oy (:y camera)))
      (.clipRect g (:x camera) (:y camera) (:width camera) (:height camera))
      (.setRenderingHint g RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
      (.setComposite g (AlphaComposite/getInstance AlphaComposite/SRC_OVER (float 0.45)))
      (.drawImage g image (int x) (int y) nil)
      (.setColor g color)
      (doseq [[cx cy] (buildings/cells position entry)]
        (.fillRect g (* cx rendering/tile-size) (* cy rendering/tile-size) rendering/tile-size rendering/tile-size))
      (.setComposite g AlphaComposite/SrcOver)
      (doseq [[cx cy] (buildings/cells position entry)]
        (.drawRect g (* cx rendering/tile-size) (* cy rendering/tile-size) (dec rendering/tile-size) (dec rendering/tile-size)))
      (finally (.dispose g)))))

(defn install!
  ([panel connection player-id label apply-world! cancel-gather!]
   (install! panel connection player-id label apply-world! cancel-gather! {}))
  ([panel connection player-id label apply-world! cancel-gather! server-sequences]
  (swap! command-sequences update player-id
         #(merge-with max % (select-keys server-sequences [:attack-seq :repair-seq])))
  (let [state (atom nil) cursor (atom nil) latest-key (atom nil) busy (atom false) closed (atom false)
        menu (JPopupMenu.)
        cursor-cell (fn [snapshot]
                      (when @cursor
                        (let [camera (rendering/camera (get-in snapshot [:players player-id])
                                                       (.getWidth panel) (.getHeight panel))]
                          (apply screen-to-cell camera (.getWidth panel) (.getHeight panel) @cursor))))
        target-context (fn []
                         (let [snapshot (.getClientProperty panel :settlement.gui/state)
                               position (cursor-cell snapshot)]
                           (assoc (buildings/target-at (:objects snapshot) position) :position position)))
        update-tooltip! (fn []
                          (.putClientProperty panel JComponent/TOOL_TIP_TEXT_KEY
                                              (building-tooltip (:building (target-context)))))
        context (fn []
                  (when-let [kind (:kind @state)]
                    (let [snapshot (.getClientProperty panel :settlement.gui/state)
                          position (cursor-cell snapshot)]
                      {:kind kind :position position
                       :key [(:generation @state) position snapshot] :snapshot snapshot})))
        current? (fn [key] (and (not @closed) (= key @latest-key)))
        cancel! (fn [] (reset! latest-key nil) (reset! state nil) (.setVisible menu false)
                  (.setText label controls-text) (.repaint panel))
        toggle! (fn [mode]
                  (let [enable? (not= mode (:mode @state))]
                    (cancel!)
                    (when enable?
                      (cancel-gather!)
                      (reset! state {:mode mode :generation (random-uuid)})
                      (.requestFocusInWindow panel)
                      (.setText label (get-in action-modes [mode :hint])))))
        select! (fn [kind]
                  (cancel-gather!)
                  (reset! latest-key nil)
                  (reset! state {:mode :build :kind kind :generation (random-uuid)})
                  (.setVisible menu false) (.requestFocusInWindow panel) (.repaint panel))
        check! (fn []
                 (when-let [{:keys [kind position key snapshot]} (context)]
                   (reset! latest-key key)
                   (let [cost (get-in buildings/catalog [kind :cost])
                         items (:items (inventory/player-inventory (get-in snapshot [:players player-id])))
                         result (when (= key (:checked-key @state)) (:result @state))]
                     (.setText label
                       (str "<html>" (get-in buildings/catalog [kind :name]) " — цена: " (:wood cost 0) " дерева, " (:stone cost 0)
                            " камня; у вас: " (:wood items) " / " (:stone items) ".<br>"
                            (cond (nil? position) "Наведите курсор на карту."
                                  (nil? result) "Проверка места…"
                                  (= :ok (:status result)) "ЛКМ — построить, Esc — отмена."
                                  :else (get messages (:code result) "Недопустимое место.")) "</html>"))
                     (when (and position (not= key (:checked-key @state)))
                       (client/async! connection :build-check
                         {:type :build-check :player-id player-id :building-type kind :position position}
                         (fn [result] (swap! state assoc :checked-key key :result result) (.repaint panel))
                         #(current? key) false)))))
        place! (fn []
                 (when-let [{:keys [kind position key]} (context)]
                   (when (and position (= key (:checked-key @state)) (= :ok (get-in @state [:result :status]))
                              (compare-and-set! busy false true))
                     (cancel-gather!)
                     (let [generation (:generation @state)]
                       (when-not
                         (client/async! connection :build
                           {:type :build :player-id player-id :building-type kind :position position}
                           (fn [result]
                             (reset! busy false)
                             (when (:world result) (apply-world! (:world result)))
                             (when (= generation (:generation @state))
                               (swap! state dissoc :checked-key :result)
                               (.setText label (if (= :ok (:status result)) "Постройка готова."
                                                 (get messages (:code result) "Не удалось построить."))))
                             (.repaint panel)))
                         (reset! busy false)
                         (.setText label (:queue-full messages)))))))
        act! (fn []
               (let [{:keys [mode generation]} @state
                     {:keys [type seq-key server-seq]} (action-modes mode)
                     {:keys [position building]} (target-context)
                     id (:building-id building)]
                 (cond
                   (not (uuid? id)) (.setText label (:building-not-found action-messages))
                   (not (#{:blue :red} (:team building)))
                   (.setText label (action-message mode {:code :invalid-target-team}))
                   (compare-and-set! busy false true)
                   (let [path [player-id seq-key]
                         number (get-in (swap! command-sequences update-in path (fnil inc 0)) path)
                         message {:type type :player-id player-id :position position :building-id id seq-key number}]
                     (cancel-gather!)
                     (when-not (client/async! connection :building-action message
                                 (fn [result]
                                   (reset! busy false)
                                   (when-let [last-seq (server-seq result)]
                                     (swap! command-sequences update-in path max last-seq))
                                   (when-not @closed
                                     (when-let [world (:world result)] (apply-world! world))
                                     (when (= generation (:generation @state))
                                       (.setText label (action-message mode result))))))
                       (reset! busy false)
                       (.setText label (:queue-full messages)))))))
        move-cursor! (fn [event]
                       (reset! cursor [(.getX event) (.getY event)])
                       (reset! latest-key nil)
                       (update-tooltip!)
                       (.repaint panel))
        motion (proxy [MouseMotionAdapter] []
                 (mouseMoved [event] (move-cursor! event))
                 (mouseDragged [event] (move-cursor! event)))
        mouse (proxy [MouseAdapter] []
                (mouseEntered [event] (move-cursor! event))
                (mouseExited [_] (reset! cursor nil) (update-tooltip!) (.repaint panel))
                (mouseClicked [event] (when (= 1 (.getButton event))
                                       (reset! cursor [(.getX event) (.getY event)])
                                       (if (action-modes (:mode @state)) (act!) (place!)))))
        snapshot-listener (reify PropertyChangeListener
                            (propertyChange [_ _] (update-tooltip!)))
        timer (Timer. 150 (proxy [ActionListener] [] (actionPerformed [_] (check!))))]
    (.setBackground menu (Color. 45 59 39))
    (doseq [kind [:fence :house]]
      (let [spec (buildings/catalog kind)
            item (JMenuItem. (str (:name spec) " — " (get-in spec [:cost :wood]) " дерева"
                                 (when-let [stone (get-in spec [:cost :stone])] (str " + " stone " камня"))))]
        (.setOpaque item true)
        (.setBackground item (Color. 78 86 52))
        (.setForeground item (Color. 245 236 201))
        (.setFont item (Font. "Monospaced" Font/BOLD 12))
        (.addActionListener item (proxy [ActionListener] [] (actionPerformed [_] (select! kind))))
        (.add menu item)))
    (doseq [[code release? name action]
            [[KeyEvent/VK_B false "building-menu" #(do (cancel-gather!) (cancel!) (.show menu panel 12 12))]
             [KeyEvent/VK_ESCAPE false "cancel-building" cancel!]
             [KeyEvent/VK_F true "attack-mode" #(toggle! :attack)]
             [KeyEvent/VK_R true "repair-mode" #(toggle! :repair)]]]
      (let [stroke (KeyStroke/getKeyStroke code 0 release?)
            handler (proxy [AbstractAction] [] (actionPerformed [_] (action)))]
        (.put (.getInputMap panel JPanel/WHEN_IN_FOCUSED_WINDOW) stroke name)
        (.put (.getActionMap panel) name handler)
        (when release?
          (.put (.getInputMap menu JPanel/WHEN_IN_FOCUSED_WINDOW) stroke name)
          (.put (.getActionMap menu) name handler))))
    (.putClientProperty panel :settlement.gui/overlay
      (fn [graphics camera ox oy]
        (when-let [{:keys [kind position key]} (context)]
          (when position
            (draw-preview! graphics camera ox oy kind position
              (if (= key (:checked-key @state)) (get-in @state [:result :status]) :pending))))))
    (.addMouseMotionListener panel motion) (.addMouseListener panel mouse)
    (.registerComponent (ToolTipManager/sharedInstance) panel)
    (.addPropertyChangeListener panel ":settlement.gui/state" snapshot-listener)
    (.start timer)
    {:state state :cursor cursor :select! select! :cancel! cancel! :check! check! :place! place! :menu menu
     :close! (fn [] (reset! closed true) (cancel!) (.stop timer)
               (.removePropertyChangeListener panel ":settlement.gui/state" snapshot-listener)
               (.unregisterComponent (ToolTipManager/sharedInstance) panel)
               (.putClientProperty panel JComponent/TOOL_TIP_TEXT_KEY nil)
               (.removeMouseMotionListener panel motion) (.removeMouseListener panel mouse)
               (.putClientProperty panel :settlement.gui/overlay nil))})))
