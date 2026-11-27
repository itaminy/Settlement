(ns settlement.building-ui
  (:require [settlement.client :as client]
            [settlement.buildings :as buildings]
            [settlement.bandits :as bandits]
            [settlement.inventory :as inventory]
            [settlement.minimap :as minimap]
            [settlement.effects :as effects]
            [settlement.ui-theme :as theme]
            [settlement.inventory-ui :as inventory-ui]
            [settlement.rendering :as rendering])
  (:import [javax.swing JPopupMenu JMenuItem KeyStroke AbstractAction JPanel Timer JComponent ToolTipManager ImageIcon]
           [java.awt Color Font AlphaComposite RenderingHints]
           [java.awt.event KeyEvent MouseAdapter MouseMotionAdapter ActionListener]
           [java.beans PropertyChangeListener]))

(def controls-text "B — строительство · F — атака · R — ремонт")
(def descriptions
  {:fence "Ограждает участок"
   :house "Приносит очки поселению"
   :watchtower "Укрепляет застройку и приносит очки"
   :sawmill "Постройка поселения за дерево и камень"
   :mine "Постройка поселения с высокой стоимостью камня"})
(defonce ^:private command-sequences (atom {}))

(def ^:private action-modes
  {:attack {:type :attack-building :seq-key :attack-seq :server-seq :last-attack-seq
            :hint "Атака — выберите бандита или вражескую постройку."}
   :repair {:type :repair-building :seq-key :repair-seq :server-seq :last-repair-seq
            :hint "Ремонт: выберите союзную постройку."}})

(def ^:private action-messages
  {:too-far "Встаньте рядом с целью."
   :insufficient-items "Для ремонта нужно 1 дерево."
   :attack-cooldown "Следующий удар возможен через секунду после предыдущего."
   :building-not-found "Постройка не найдена."
   :stale-building "Постройка уже изменилась. Выберите цель снова."
   :bandit-not-found "Бандит уже исчез."
   :stale-bandit "Бандит переместился. Выберите цель снова."
   :player-downed "Дождитесь возрождения."
   :building-at-full-hp "Постройка уже полностью исправна."
   :not-building-owner "Ремонтировать постройку может только её владелец."
   :match-finished "Матч завершён."
   :stale-attack "Номер команды обновлён. Нажмите ещё раз."
   :attack-seq-conflict "Номер команды обновлён. Нажмите ещё раз."
   :stale-repair "Номер команды обновлён. Нажмите ещё раз."
   :repair-seq-conflict "Номер команды обновлён. Нажмите ещё раз."
   :connection-failed "Связь с сервером потеряна."})

(defn- action-message [mode result]
  (cond
    (and (:bandit-id result) (:destroyed? result)) "Бандит побеждён. Награда получена."
    (:bandit-id result) (str "Бандит ранен, осталось " (:hp result) " HP.")
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
        y (- sy (quot (- panel-height (:height camera)) 2))
        [left top] (rendering/raster-origin camera)]
    (when (and (<= 0 x) (< x (:width camera)) (<= 0 y) (< y (:height camera)))
      [(quot (+ x left) rendering/tile-size) (quot (+ y top) rendering/tile-size)])))

(defn draw-preview! [graphics camera ox oy kind position status]
  (let [g (.create graphics)
        entry {:type :building :kind kind}
        {:keys [image x y]} (rendering/building-placement position entry false)
        [left top] (rendering/raster-origin camera)
        color (case status :ok (Color. 85 210 110) :pending (Color. 190 165 80) (Color. 235 80 65))]
    (try
      (.clipRect g ox oy (:width camera) (:height camera))
      (.translate g (int (- ox left)) (int (- oy top)))
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
        entries (atom {})
        cursor-cell (fn [snapshot]
                      (when (and @cursor (not (apply minimap/hit? panel @cursor)))
                        (let [camera (or (.getClientProperty panel :settlement.gui/camera)
                                         (rendering/camera (get-in snapshot [:players player-id])
                                                           (.getWidth panel) (.getHeight panel)))]
                          (apply screen-to-cell camera (.getWidth panel) (.getHeight panel) @cursor))))
        target-context (fn []
                         (let [snapshot (.getClientProperty panel :settlement.gui/state)
                               position (cursor-cell snapshot)
                               visual (.getClientProperty panel :settlement.gui/visual-bandits)
                               camera (.getClientProperty panel :settlement.gui/camera)
                               picked (when (and position visual camera @cursor)
                                        (let [[left top] (rendering/raster-origin camera)
                                              [sx sy] @cursor
                                              x (+ left (- sx (quot (- (.getWidth panel) (:width camera)) 2)))
                                              y (+ top (- sy (quot (- (.getHeight panel) (:height camera)) 2)))]
                                          (some (fn [{:keys [id foot-y] mx :x}]
                                                  (when (and (<= (abs (- x mx)) 19)
                                                             (<= (- foot-y 58) y (+ foot-y 3)))
                                                    (get-in snapshot [:bandits id])))
                                                (reverse (sort-by :foot-y (rendering/player-markers visual))))))
                               npc (or (when (bandits/alive? picked) picked)
                                       (some #(when (and (bandits/alive? %)
                                                         (= position (bandits/position %))) %)
                                             (vals (:bandits snapshot))))]
                           (if npc {:bandit npc :position (bandits/position npc)}
                               (assoc (buildings/target-at (:objects snapshot) position) :position position))))
        update-tooltip! (fn []
                          (let [{:keys [bandit building]} (target-context)]
                            (.putClientProperty panel JComponent/TOOL_TIP_TEXT_KEY
                                                (if bandit (str "Бандит — " (:hp bandit) "/" (:max-hp bandit) " HP")
                                                    (building-tooltip building)))))
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
                             (when (= :ok (:status result))
                               (.putClientProperty panel :settlement.gui/confirmed-action
                                                   ["Строительство" (+ (effects/now-ms) 1200)]))
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
                     {:keys [position building bandit]} (target-context)
                     attacking-bandit? (and bandit (= mode :attack))
                     id (if attacking-bandit? (:id bandit) (:building-id building))]
                 (cond
                   (not (uuid? id)) (.setText label (:building-not-found action-messages))
                   (and (not attacking-bandit?) (not (#{:blue :red} (:team building))))
                   (.setText label (action-message mode {:code :invalid-target-team}))
                   (compare-and-set! busy false true)
                   (let [path [player-id seq-key]
                         number (get-in (swap! command-sequences update-in path (fnil inc 0)) path)
                         message {:type (if attacking-bandit? :attack-bandit type)
                                  :player-id player-id :position position
                                  (if attacking-bandit? :bandit-id :building-id) id seq-key number}]
                     (cancel-gather!)
                     (when-not (client/async! connection :building-action message
                                 (fn [result]
                                   (reset! busy false)
                                   (when-let [last-seq (server-seq result)]
                                     (swap! command-sequences update-in path max last-seq))
                                   (when-not @closed
                                     (when (= :ok (:status result))
                                       (.putClientProperty panel :settlement.gui/confirmed-action
                                                           [(if (= mode :repair) "Ремонт" "Атака")
                                                            (+ (effects/now-ms) 1200)]))
                                     (let [before (get-in (.getClientProperty panel :settlement.gui/state)
                                                          [:players player-id])
                                           after (get-in result [:world :players player-id])
                                           response (if (and attacking-bandit? (:destroyed? result) before after)
                                                      (assoc result :awards
                                                        (map (fn [resource]
                                                               [resource (- (get (inventory/resources (inventory/player-inventory after)) resource 0)
                                                                            (get (inventory/resources (inventory/player-inventory before)) resource 0))])
                                                             [:wood :stone]))
                                                      result)]
                                       (effects/add! panel (effects/confirmed-effects player-id (:type message) response)))
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
                (mouseClicked [event]
                  (if (minimap/hit? panel (.getX event) (.getY event))
                    (.consume event)
                    (when (= 1 (.getButton event))
                      (reset! cursor [(.getX event) (.getY event)])
                      (if (action-modes (:mode @state)) (act!) (place!))))))
        snapshot-listener (reify PropertyChangeListener
                            (propertyChange [_ _] (update-tooltip!)))
        timer (Timer. 150 (proxy [ActionListener] [] (actionPerformed [_] (check!))))]
    (.setBackground menu theme/wood)
    (.setBorder menu (theme/frame-border 4))
    (doseq [kind [:fence :house :watchtower :sawmill :mine]]
      (let [spec (buildings/catalog kind)
            item (JMenuItem.)
            art (:image (rendering/building-placement [0 0] {:kind kind :team :blue} false))]
        (.setIcon item (ImageIcon. (rendering/scale-image art 52 52)))
        (.setText item (format "<html><b>%s</b><br>%s<br>Дерево %d · Камень %d · %d HP · %d очк.</html>"
                               (:name spec) (descriptions kind) (get-in spec [:cost :wood] 0)
                               (get-in spec [:cost :stone] 0) (:max-hp spec) (:points spec)))
        (.setOpaque item true)
        (.setBackground item theme/wood)
        (.setForeground item theme/cream)
        (.setFont item (Font. "Dialog" Font/PLAIN (theme/scaled 12)))
        (.addActionListener item (proxy [ActionListener] [] (actionPerformed [_] (select! kind))))
        (.add menu item)
        (swap! entries assoc kind item)))
    (doseq [[code release? name action]
            [[KeyEvent/VK_B false "building-menu" #(do (cancel-gather!) (cancel!)
                                                      (let [world (.getClientProperty panel :settlement.gui/state)
                                                            data (inventory/player-inventory (get-in world [:players player-id]))]
                                                        (doseq [[kind item] @entries
                                                                :let [available? (inventory-ui/affordable? (:items data)
                                                                                                            (get-in buildings/catalog [kind :cost]))]]
                                                          (.setEnabled item available?)
                                                          (.setToolTipText item (if available? "Укажите площадку на карте."
                                                                                  "Недостаточно ресурсов."))))
                                                      (.show menu panel 12 12))]
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
    (.addPropertyChangeListener panel ":settlement.gui/camera" snapshot-listener)
    (.start timer)
    {:state state :cursor cursor :select! select! :cancel! cancel! :check! check! :place! place! :menu menu
     :close! (fn [] (reset! closed true) (cancel!) (.stop timer)
               (.removePropertyChangeListener panel ":settlement.gui/state" snapshot-listener)
               (.removePropertyChangeListener panel ":settlement.gui/camera" snapshot-listener)
               (.unregisterComponent (ToolTipManager/sharedInstance) panel)
               (.putClientProperty panel JComponent/TOOL_TIP_TEXT_KEY nil)
               (.removeMouseMotionListener panel motion) (.removeMouseListener panel mouse)
               (.putClientProperty panel :settlement.gui/overlay nil))})))
