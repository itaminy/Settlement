(ns settlement.gui
  (:require [settlement.client :as client]
            [settlement.config :as config]
            [settlement.layout :as layout]
            [settlement.rendering :as rendering]
            [settlement.inventory :as inventory]
            [settlement.hotbar :as hotbar]
            [settlement.minimap :as minimap]
            [settlement.effects :as effects]
            [settlement.ui-menu :as menu]
            [settlement.ui-theme :as theme]
            [settlement.bandits :as bandits]
            [settlement.movement-ui :as movement-ui]
            [settlement.inventory-ui :as inventory-ui]
            [settlement.gathering-ui :as gathering-ui]
            [settlement.building-ui :as building-ui])
  (:import [javax.swing JFrame JDialog JPanel JLabel SwingUtilities
            AbstractAction KeyStroke Timer JOptionPane JProgressBar JButton SwingConstants BorderFactory]
           [java.awt Color Dimension BorderLayout FlowLayout Font GraphicsEnvironment GridLayout
            GridBagLayout GridBagConstraints]
           [java.awt.event ActionListener KeyEvent WindowAdapter]))

(def map-size rendering/map-size)

(defn- animation-time [] (quot (System/nanoTime) 1000000))

(defn- visible-entities [players roster]
  (into (into {} (filter (comp bandits/standing? val)) players)
        (for [[id entity] roster :when (bandits/alive? entity)]
          [id (assoc entity :name "Бандит")])))

(defn update-map
  ([panel players objects] (update-map panel players objects nil))
  ([panel players objects roster]
   (.putClientProperty panel ::motion
     (rendering/update-character-motion (.getClientProperty panel ::motion)
                                        (visible-entities players roster) (animation-time)))
   (.putClientProperty panel ::state (cond-> {:players players :objects objects}
                                     roster (assoc :bandits roster)))
   (when-not (identical? objects (.getClientProperty panel ::minimap-objects))
     (.putClientProperty panel ::minimap-objects objects)
     (.putClientProperty panel ::minimap-marks (minimap/structures objects)))
   (.repaint panel)))

(defn set-map-scenery! [panel scenery]
  (.putClientProperty panel ::scenery scenery)
  (.repaint panel))

(defn prime-map-viewport! [panel prepared]
  (.putClientProperty panel ::render-cache {:prepared prepared}))

(defn create-map-panel
  ([player-x player-y] (create-map-panel player-x player-y nil))
  ([player-x player-y player-id]
   (let [animation-timer (Timer. 16 nil)
         panel
         (proxy [JPanel] []
           (addNotify []
             (proxy-super addNotify)
             (.start animation-timer))
           (removeNotify []
             (.stop animation-timer)
             (proxy-super removeNotify))
           (paintComponent [graphics]
             (proxy-super paintComponent graphics)
             (when-let [state (.getClientProperty this ::state)]
               (let [now (animation-time)
                     motion (.getClientProperty this ::motion)
                     players (rendering/visual-players (visible-entities (:players state) (:bandits state)) motion now)
                     local-id (.getClientProperty this ::player-id)
                     player (or (get players local-id) (get-in state [:players local-id])
                                {:x player-x :y player-y})
                     camera (rendering/camera player (.getWidth this) (.getHeight this))
                     character-poses (rendering/character-poses motion now)
                     scenery (.getClientProperty this ::scenery)
                     animate? (and (.isShowing this) (:animations @theme/settings) (effects/enabled? this))
                     water-phase (if animate? (quot now 95) 0)
                     key [players (:objects state) camera character-poses scenery water-phase (:names @theme/settings)]
                     cached (.getClientProperty this ::render-cache)
                     deferred? (and (.isShowing this) (:image cached)
                                    (= [(:width camera) (:height camera)]
                                       [(.getWidth (:image cached)) (.getHeight (:image cached))])
                                    (< (- now (:at cached)) (.getDelay animation-timer))
                                    (not= key (:key cached)))
                     image (if (or deferred? (= key (:key cached)))
                             (:image cached)
                             (let [prepared (rendering/prepare-viewport (:prepared cached) (:objects state) camera scenery)
                                   image (rendering/render-viewport (:image cached) players camera character-poses prepared water-phase)]
                               (.putClientProperty this ::render-cache {:key key :image image :prepared prepared :at now})
                               image))
                     painted-camera (if deferred? (get-in cached [:key 2]) camera)
                     painted-players (if deferred? (get-in cached [:key 0]) players)]
                 (.putClientProperty this ::deferred-frame deferred?)
                 (.putClientProperty this ::camera painted-camera)
                 (.putClientProperty this ::water-frame (when animate? water-phase))
                 (.putClientProperty this ::visual-bandits
                   (select-keys painted-players (keys (:bandits state))))
                 (.putClientProperty this ::painted-motion? (boolean (rendering/moving? motion now)))
                 (.putClientProperty this ::scene image)
                 (let [ox (quot (- (.getWidth this) (:width camera)) 2)
                       oy (quot (- (.getHeight this) (:height camera)) 2)]
                   (.drawImage graphics image ox oy nil)
                   (rendering/draw-building-impacts! graphics
                     (get-in (.getClientProperty this ::render-cache) [:prepared :layers])
                     (effects/events this) painted-camera ox oy now)
                   (effects/draw! graphics (effects/events this) painted-players painted-camera ox oy now)
                   (when-let [overlay (.getClientProperty this ::overlay)]
                     (overlay graphics painted-camera ox oy))
                   (minimap/draw! graphics this painted-camera painted-players
                                  (.getClientProperty this ::minimap-marks)))))))]
     (.addActionListener animation-timer
       (proxy [ActionListener] []
         (actionPerformed [_]
           (let [delay (int (Math/ceil (/ 1000.0 (:fps @theme/settings))))]
             (when (not= delay (.getDelay animation-timer)) (.setDelay animation-timer delay)))
           (when (or (effects/tick! panel (animation-time))
                     (.getClientProperty panel ::deferred-frame)
                     (.getClientProperty panel ::painted-motion?)
                     (rendering/moving? (.getClientProperty panel ::motion) (animation-time))
                     (when-let [phase (.getClientProperty panel ::water-frame)]
                       (and (effects/enabled? panel) (not= phase (quot (animation-time) 95)))))
             (.repaint panel)))))
     (.putClientProperty panel ::player-id player-id)
     (.setBackground panel (Color. 40 55 35))
     (.setPreferredSize panel (Dimension. config/viewport-width config/viewport-height))
     (update-map panel {} {})
     panel)))

(declare update-player-panels!)
(declare update-match-screen!)

(defn player-resources [state player-id]
  (inventory/resources (inventory/player-inventory (get-in state [:players player-id]))))

(defn- apply-world! [panel resource-ui state player-id]
  (let [view (:match-view resource-ui)
        current (when view @view)
        id (:match-id state 0)
        finished? (= :finished (get-in state [:match :phase]))]
    (when (or (nil? current)
              (> id (:id current))
              (and (= id (:id current)) (or finished? (not (:finished? current)))))
      (when (and current (> id (:id current)))
        (when-let [clear! (:clear-movement! resource-ui)] (clear!))
        (.putClientProperty panel ::confirmed-action nil)
        (.putClientProperty panel ::motion nil))
      (let [previous (.getClientProperty panel ::effect-world)]
        (if (and previous (not= (:match-id previous) (:match-id state)))
          (.putClientProperty panel :settlement.effects/events [])
          (effects/add! panel (effects/snapshot-effects previous state)))
        (.putClientProperty panel ::effect-world state))
      (update-map panel (:players state) (:objects state) (:bandits state))
      (when resource-ui
        (update-player-panels! resource-ui state player-id))
      (when view
        (reset! (:match-id resource-ui) id)
        (let [screen (:match-screen resource-ui)
              enable! (:set-gameplay! resource-ui)
              previous (:previous-match state)
              missed? (and current (> id (:id current)) (not finished?)
                           (not (:finished? current)) (= (:id current) (:id previous)))
              holding? (and (= id (:id current)) (:holding? current) (not finished?))
              next-view {:id id :finished? finished? :holding? (boolean (or missed? holding?))}]
          (when (not= current next-view)
            (reset! view next-view)
            (enable! (not (or finished? (:holding? next-view)))))
          (cond
            missed?
            (do
              (update-match-screen! screen (assoc state :match-id (:id previous) :match (:result previous)
                                                       :match-duration-ms (:duration-ms previous)
                                                       :bandit-stats (:bandit-stats previous))
                                    player-id)
              (.setEnabled (:button screen) false)
              (.setText (:message screen) "Новый матч уже начался.")
              (doto (Timer. 2500 (proxy [ActionListener] []
                                   (actionPerformed [_]
                                     (when (= next-view @view)
                                       (swap! view assoc :holding? false)
                                       (update-match-screen! screen state player-id)
                                       (enable! true)))))
                (.setRepeats false)
                (.start)))
            (not holding?) (update-match-screen! screen state player-id)))))))

(defn refresh! [panel connection resource-ui player-id]
  (let [receive! (fn [response]
                   (if (= :ok (:status response))
                     (apply-world! panel resource-ui (:world response) player-id)
                     (when (= :connection-failed (:code response))
                       (when-let [label (:connection-label resource-ui)]
                         (.setText label "● Связь потеряна")
                         (.setForeground label theme/red))
                       (when-let [offline! (:offline! resource-ui)] (offline!)))))]
    (if (some-> connection :queue :closed deref)
      (when-not (.getClientProperty panel ::offline)
        (.putClientProperty panel ::offline true)
        (receive! {:status :error :code :connection-failed}))
      (client/async! connection :state {:type :state} receive!))))

(defn cancel-local-gather! [panel]
  (when-let [controller (.getClientProperty panel ::gather-controller)]
    ((:release! controller))))

(defn bind-movement! [panel connection resource-ui player-id]
  (let [controller (movement-ui/create-controller connection player-id
                     #(cancel-local-gather! panel) #(apply-world! panel resource-ui % player-id)
                     animation-time)]
    (movement-ui/bind-keys! panel controller)
    controller))

(defn bind-gather-key [panel connection resource-ui player-id]
  (let [progress (or (:progress-bar resource-ui) (JProgressBar. 0 100))
        controller (gathering-ui/create-controller connection player-id progress
                     #(refresh! panel connection resource-ui player-id)
                     (fn [command reply]
                       (when (#{:cancel :gather-finish} command) (effects/clear-gather! panel))
                       (effects/add! panel (effects/confirmed-effects player-id command reply))))
        timer (Timer. 50 (proxy [ActionListener] [] (actionPerformed [_] ((:tick! controller)))))]
    (.putClientProperty panel ::gather-controller (assoc controller :timer timer))
    (doseq [[released? key action] [[false "gather" (:press! controller)] [true "gather-release" (:release! controller)]]]
      (.put (.getInputMap panel JPanel/WHEN_IN_FOCUSED_WINDOW)
            (KeyStroke/getKeyStroke KeyEvent/VK_E 0 released?) key)
      (.put (.getActionMap panel) key (proxy [AbstractAction] [] (actionPerformed [_] (action)))))
    (.start timer)
    timer))

(defn start-map-updater [panel connection resource-ui player-id]
  (let [timer (Timer. config/snapshot-interval-ms (proxy [ActionListener] []
                           (actionPerformed [_] (refresh! panel connection resource-ui player-id))))]
    (.start timer) timer))

(defn format-duration [ms]
  (if (number? ms)
    (let [seconds (quot ms 1000)] (format "%02d:%02d" (quot seconds 60) (mod seconds 60)))
    "—"))

(defn- meter [color]
  (doto (JProgressBar. 0 100)
    (.setBackground theme/inset) (.setForeground color)
    (.setPreferredSize (Dimension. 100 8))
    (.setBorderPainted false)))

(defn create-resource-panel [items]
  (let [panel (doto (theme/wood-panel (GridLayout. 1 5 12 0))
                (.setBackground theme/wood) (.setBorder (theme/frame-border 5)))
        blue-label (theme/label "Синие: 0" 13 theme/blue)
        red-label (theme/label "Красные: 0" 13 theme/red)
        goal-label (theme/label "Цель: 100" 11 theme/muted)
        clock (theme/label "—" 17 theme/gold)
        hp-label (theme/label "HP —" 13)
        action (theme/label "Ожидание" 11 theme/muted)
        connection (theme/label "● Подключение" 11 theme/muted)
        blue-meter (meter theme/blue)
        red-meter (meter theme/red)
        hp-meter (meter (Color. 129 167 90))]
    (doseq [[top bottom] [[blue-label blue-meter] [red-label red-meter]
                         [clock goal-label] [hp-label hp-meter] [connection action]]]
      (let [column (doto (JPanel. (BorderLayout. 0 3)) (.setOpaque false))]
        (.add column top BorderLayout/CENTER)
        (.add column bottom BorderLayout/SOUTH)
        (.add panel column)))
    {:panel panel :blue-label blue-label :red-label red-label :goal-label goal-label
     :clock clock :hp-label hp-label :action-label action :connection-label connection
     :blue-meter blue-meter :red-meter red-meter :hp-meter hp-meter
     :hotbar (hotbar/create-panel (merge (:items inventory/empty-inventory) items))}))

(defn update-player-panels! [resource-ui state player-id]
  (let [contents (inventory/player-inventory (get-in state [:players player-id]))
        scores (get-in state [:match :scores])]
    (hotbar/update-items! (:hotbar resource-ui) (:items contents))
    (.setText (:blue-label resource-ui) (str "Синие: " (:blue scores 0)))
    (.setText (:red-label resource-ui) (str "Красные: " (:red scores 0)))
    (.setValue (:blue-meter resource-ui) (:blue scores 0))
    (.setValue (:red-meter resource-ui) (:red scores 0))
    (.setText (:clock resource-ui) (format-duration (:match-duration-ms state)))
    (when (get-in state [:players player-id])
      (.setText (:connection-label resource-ui) "● Подключено")
      (.setForeground (:connection-label resource-ui) theme/cream))
    (when-let [label (:hp-label resource-ui)]
      (let [player (get-in state [:players player-id])
            [action until] (when-let [panel (:map-panel resource-ui)]
                             (.getClientProperty panel ::confirmed-action))]
        (.setValue (:hp-meter resource-ui) (:hp player (:hp config/player-combat)))
        (.setText (:action-label resource-ui)
                  (cond (:respawn-at player) "Возрождение"
                        (contains? (:gathering-players state) player-id) "Добыча"
                        (= :finished (get-in state [:match :phase])) "Матч завершён"
                        (and until (> until (animation-time))) action
                        :else "Готов"))
        (.setText label (if (:respawn-at player) "Возрождение…"
                          (str "HP " (:hp player (:hp config/player-combat))
                               "/" (:hp config/player-combat))))))
    (when-let [ui (:inventory-ui resource-ui)]
      (inventory-ui/update-panel! ui contents))))

(defn create-game-content [map-panel resource-ui progress build-label]
  (let [content (JPanel. (BorderLayout.))
        footer (doto (JPanel. (BorderLayout.)) (.setBackground theme/wood))
        {:keys [panel wheel]} (:hotbar resource-ui)]
    (.setHorizontalAlignment build-label SwingConstants/CENTER)
    (.setFont build-label (Font. "Dialog" Font/PLAIN (theme/scaled 12)))
    (.setPreferredSize build-label (Dimension. 400 29))
    (.addMouseWheelListener map-panel wheel)
    (.add footer progress BorderLayout/NORTH)
    (.add footer panel BorderLayout/CENTER)
    (.add footer build-label BorderLayout/SOUTH)
    (.add content (:panel resource-ui) BorderLayout/NORTH)
    (.add content map-panel BorderLayout/CENTER)
    (.add content footer BorderLayout/SOUTH)
    (minimap/install! map-panel content)
    (.put (.getInputMap content JPanel/WHEN_IN_FOCUSED_WINDOW)
          (KeyStroke/getKeyStroke KeyEvent/VK_V 0 true) "effects")
    (.put (.getActionMap content) "effects"
          (proxy [AbstractAction] [] (actionPerformed [_] (effects/toggle! map-panel))))
    {:panel content :footer footer}))

(defn bind-inventory-key [component toggle!]
  (.put (.getInputMap component JPanel/WHEN_IN_FOCUSED_WINDOW)
        (KeyStroke/getKeyStroke KeyEvent/VK_I 0) "inventory")
  (.put (.getActionMap component) "inventory"
        (proxy [AbstractAction] [] (actionPerformed [_] (toggle!)))))

;; Создание игрового окна
(defn- confirm-dialog [message]
  (= JOptionPane/YES_OPTION
     (JOptionPane/showConfirmDialog nil message "Settlement" JOptionPane/YES_NO_OPTION)))

(defn create-match-screen [map-panel]
  (let [overlay (proxy [JPanel] [(GridBagLayout.)]
                  (paintComponent [graphics]
                    (proxy-super paintComponent graphics)
                    (.setColor graphics (Color. 18 25 16 185))
                    (.fillRect graphics 0 0 (.getWidth this) (.getHeight this))))
        cream theme/cream
        label (fn [size] (doto (JLabel. "" SwingConstants/CENTER)
                           (.setForeground cream)
                           (.setFont (Font. "Arial" Font/BOLD size))))
        title (label 32)
        winner (label 20)
        score (label 17)
        duration (label 14)
        stats (label 13)
        message (label 13)
        button (theme/button "Новая игра")
        back (theme/button "В главное меню")
        card (doto (theme/wood-panel (GridLayout. 0 1 0 10))
                 (.setBackground theme/wood)
                 (.setBorder (theme/frame-border 20)))
        bounds (GridBagConstraints.)]
    (doseq [component [title winner score duration stats button back message]] (.add card component))
    (.setOpaque overlay false)
    (.add overlay card)
    (.setVisible overlay false)
    (.setLayout map-panel (GridBagLayout.))
    (set! (.weightx bounds) 1.0)
    (set! (.weighty bounds) 1.0)
    (set! (.fill bounds) GridBagConstraints/BOTH)
    (.add map-panel overlay bounds)
    {:panel overlay :title title :winner winner :score score :duration duration :stats stats :message message
     :button button :back back :match-id (atom nil)}))

(defn update-match-screen! [screen state player-id]
  (let [{:keys [phase winner scores]} (:match state)
        match-id (:match-id state 0)
        finished? (= :finished phase)]
    (when finished?
      (when (not= match-id @(:match-id screen))
        (reset! (:match-id screen) match-id)
        (.setText (:message screen) " ")
        (.setEnabled (:button screen) true))
      (.setText (:title screen)
                (if (= winner (get-in state [:players player-id :team])) "Победа!" "Поражение!"))
      (.setText (:winner screen) (get {:blue "Победили синие" :red "Победили красные"} winner))
      (.setText (:score screen) (str "Синие: " (:blue scores 0) "    Красные: " (:red scores 0)))
      (.setText (:duration screen) (str "Время матча  " (format-duration (:match-duration-ms state))))
      (.setText (:stats screen) (if-let [stats (:bandit-stats state)]
                                 (str "Побеждено бандитов  " (:bandits-killed stats 0)
                                      "   ·   Разрушено ими  " (:buildings-destroyed stats 0))
                                 " ")))
    (.setVisible (:panel screen) finished?)))

(defn- bind-restart! [screen connection panel resource-ui player-id]
  (let [button (:button screen)
        message (:message screen)]
    (.addActionListener button
      (proxy [ActionListener] []
        (actionPerformed [_]
          (let [id @(:match-id screen)]
            (.setEnabled button false)
            (.setText message "Запуск нового матча…")
            (when-not
              (client/async! connection :restart-match {:type :restart-match :match-id id}
                (fn [result]
                  (cond
                    (= :ok (:status result)) (apply-world! panel resource-ui (:world result) player-id)
                    (#{:stale-match :match-active} (:code result)) (refresh! panel connection resource-ui player-id)
                    :else (when (= id @(:match-id screen))
                            (.setText message "Не удалось начать новый матч.")
                            (.setEnabled button true)))))
              (.setText message "Связь недоступна. Повторите попытку.")
              (.setEnabled button true))))))))

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
    :match-finished "Матч завершён. Дождитесь начала нового матча."
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
                  (do (if-let [panel (:map-panel resource-ui)]
                        (apply-world! panel resource-ui (:world result) player-id)
                        (update-player-panels! resource-ui (:world result) player-id))
                      (.setText (:message inventory-panel) "Инструмент создан."))
                  (.setText (:message inventory-panel)
                            (if (= :insufficient-items (:code result)) "Недостаточно дерева или камня." "Не удалось создать инструмент.")))))))))
))

(defn pause-card [actions]
  (let [panel (menu/card "Пауза")
        rows (menu/column)]
    (doseq [[key text] [[:resume "Продолжить игру"] [:settings "Настройки"]
                        [:controls "Управление"] [:menu "В главное меню"] [:quit "Выход из игры"]]]
      (.add rows (menu/on-click! (theme/button text) (actions key))))
    (.add panel rows BorderLayout/CENTER)
    panel))

(defn open-game-window [connection player-id response on-menu on-quit]
  (if (not= config/world-seed (get-in response [:world :world-seed] config/world-seed))
    (do
      (client/close-connection connection)
      (JOptionPane/showMessageDialog nil "Установите тот же SETTLEMENT_WORLD_SEED, что и на сервере."))
    (let [match-id (atom (get-in response [:world :match-id] 0))
        connection (assoc connection :match-id match-id)
        frame (doto (JFrame. "Settlement") (.setUndecorated true))
        player (get-in response [:world :players player-id])
        map-panel (doto (create-map-panel (:x player) (:y player) player-id)
                    (set-map-scenery! rendering/game-scenery))
        screen (create-match-screen map-panel)
        inventory-window (JDialog. frame "Инвентарь" false)
        inventory-panel (inventory-ui/create-panel (inventory/player-inventory player) (:team player))
        paused? (atom false)
        online? (atom true)
        finished? (atom false)
        exit-callback (atom on-menu)
        cancel-building! (atom nil)
        cancel-movement! (atom nil)
        set-gameplay! (fn [confirmed?]
                        (reset! finished? (not confirmed?))
                        (let [enabled? (and confirmed? @online? (not @paused?))]
                        (when-not enabled?
                          (cancel-local-gather! map-panel)
                          (when-let [clear! @cancel-movement!] (clear!))
                          (when-let [cancel! @cancel-building!] (cancel!))
                          (.setVisible inventory-window false))
                        (let [actions (.getActionMap map-panel)]
                          (doseq [key (.keys actions)]
                            (.setEnabled (.get actions key) enabled?)))
                        (inventory-ui/enable! inventory-panel enabled?)))
        progress (doto (JProgressBar. 0 100) (.setStringPainted true) (.setString "Добыча: удерживайте E")
                       (.setForeground (Color. 132 164 83)) (.setBackground (Color. 45 59 39)) (.setVisible false))
        resource-ui (assoc (create-resource-panel (:items (inventory/player-inventory player)))
                           :inventory-ui inventory-panel :progress-bar progress :map-panel map-panel
                           :clear-movement! #(when-let [clear! @cancel-movement!] (clear!))
                           :match-view (atom nil) :match-id match-id :match-screen screen
                           :offline! #(do (reset! online? false) (set-gameplay! (not @finished?)))
                           :set-gameplay! set-gameplay!)
        build-label (doto (JLabel. building-ui/controls-text) (.setForeground (Color. 240 230 193))
                          (.setPreferredSize (Dimension. 600 40)))
        {:keys [panel footer]} (create-game-content map-panel resource-ui progress build-label)
        builder (building-ui/install! map-panel connection player-id build-label
                  #(apply-world! map-panel resource-ui % player-id)
                  #(cancel-local-gather! map-panel)
                  (:building-sequences response))
        movement (bind-movement! map-panel connection resource-ui player-id)
        movement-timer (Timer. 16 (proxy [ActionListener] []
                                    (actionPerformed [_] ((:tick! movement)))))
        timer (start-map-updater map-panel connection resource-ui player-id)
        close! (fn [callback] (reset! exit-callback callback) (.dispose frame))
        pause-layer (proxy [JPanel] [(GridBagLayout.)]
                      (paintComponent [g]
                        (proxy-super paintComponent g)
                        (.setColor g (Color. 21 18 15 155))
                        (.fillRect g 0 0 (.getWidth this) (.getHeight this))))
        toggle-pause! (fn []
                        (swap! paused? not)
                        (.setVisible pause-layer @paused?)
                        (set-gameplay! (not @finished?))
                        (if @paused? (.requestFocusInWindow pause-layer) (.requestFocusInWindow map-panel)))
        pause (pause-card {:resume toggle-pause! :settings #(menu/show-settings! frame)
                           :controls #(menu/show-controls! frame)
                           :menu #(close! on-menu) :quit #(close! on-quit)})]
    (reset! cancel-building! (:cancel! builder))
    (menu/on-click! (:back screen) #(close! on-menu))
    (.setOpaque pause-layer false)
    (.setFocusable pause-layer true)
    (.addMouseListener pause-layer (proxy [java.awt.event.MouseAdapter] []))
    (.addMouseWheelListener pause-layer (reify java.awt.event.MouseWheelListener
                                        (mouseWheelMoved [_ event] (.consume event))))
    (.add pause-layer pause)
    (.setGlassPane frame pause-layer)
    (.remove (.getInputMap map-panel JPanel/WHEN_IN_FOCUSED_WINDOW)
             (KeyStroke/getKeyStroke KeyEvent/VK_ESCAPE 0))
    (let [root (.getRootPane frame)]
      (.put (.getInputMap root JPanel/WHEN_IN_FOCUSED_WINDOW)
            (KeyStroke/getKeyStroke KeyEvent/VK_ESCAPE 0 true) "pause")
      (.put (.getActionMap root) "pause"
            (proxy [AbstractAction] []
              (actionPerformed [_]
                (if (and (not @paused?) @(:state builder))
                  ((:cancel! builder))
                  (toggle-pause!))))))
    (reset! cancel-movement! (:clear! movement))
    (.start movement-timer)
    (bind-restart! screen connection map-panel resource-ui player-id)
    (bind-craft-buttons! connection player-id resource-ui)
    (.setContentPane inventory-window (:panel inventory-panel))
    (.setDefaultCloseOperation inventory-window JDialog/HIDE_ON_CLOSE)
    (.setResizable inventory-window false)
    (.pack inventory-window)
    (let [toggle! (fn []
                    (cancel-local-gather! map-panel)
                    ((:clear! movement))
                    (if (.isVisible inventory-window)
                      (do (.setVisible inventory-window false) (.requestFocusInWindow map-panel))
                      (do (refresh! map-panel connection resource-ui player-id)
                          (.setLocationRelativeTo inventory-window frame)
                          (.setVisible inventory-window true))))]
      (bind-inventory-key map-panel toggle!)
      (bind-inventory-key (.getRootPane inventory-window) toggle!))
    (bind-gather-key map-panel connection resource-ui player-id)
    (.addWindowFocusListener frame
      (proxy [java.awt.event.WindowFocusListener] []
        (windowGainedFocus [_] nil)
        (windowLostFocus [_] ((:clear! movement)) (cancel-local-gather! map-panel))))
    (.setDefaultCloseOperation frame JFrame/DISPOSE_ON_CLOSE)
    (.addWindowListener frame
      (proxy [WindowAdapter] []
        (windowClosed [_]
          (.stop timer)
          (.stop movement-timer)
          ((:clear! movement))
          ((:close! builder))
          (when-let [controller (.getClientProperty map-panel ::gather-controller)]
            ((:release! controller)) (.stop (:timer controller)))
          (.dispose inventory-window)
          (client/close-connection connection)
          (@exit-callback))))
    (.setContentPane frame panel)
    (.pack frame)
    (.setMinimumSize frame
      (let [insets (.getInsets frame)
            size (.getPreferredSize (:panel resource-ui))]
        (Dimension. (+ (max 400 (.width size)) (.left insets) (.right insets))
                    (+ (.height size) (.top insets) (.bottom insets)
                       (.height (.getPreferredSize footer))
                       (.height (.getPreferredSize progress)) (* 2 config/tile-size)))))
    (let [bounds (.getMaximumWindowBounds (GraphicsEnvironment/getLocalGraphicsEnvironment))]
      (.setSize frame (min (.getWidth frame) (.width bounds))
                      (min (.getHeight frame) (.height bounds))))
    (.setLocationRelativeTo frame nil)
    (.setVisible frame true)
    (theme/style-window! frame)
    (.requestFocusInWindow map-panel)
    (apply-world! map-panel resource-ui (:world response) player-id)
    {:frame frame :panel panel :map-panel map-panel :resource-ui resource-ui
     :builder builder :pause-layer pause-layer :inventory-window inventory-window :match-screen screen})))

(defn- on-edt [f]
  (if (SwingUtilities/isEventDispatchThread) (f)
    (let [result (promise)]
      (SwingUtilities/invokeAndWait #(deliver result (f)))
      @result)))

(defn show-window [player-id player-name]
  (SwingUtilities/invokeLater
   (fn []
     (theme/load!)
     (let [frame (JFrame. "Settlement · Главное меню")
           background (create-map-panel 39.5 25)
           preferences (atom {:name player-name :host "127.0.0.1" :port 5000})
           active-card (atom nil)
           game (atom nil)
           show-card! (fn [card]
                        (.removeAll background)
                        (.add background card)
                        (reset! active-card card)
                        (.revalidate background) (.repaint background))]
       (letfn [(home! []
                 (show-card! (menu/home-card {:play connection! :settings #(menu/show-settings! frame)
                                              :controls #(menu/show-controls! frame)
                                              :about #(JOptionPane/showMessageDialog frame
                                                        "Settlement · World V4.1\nСредневековая стратегия с матчами до 100 очков."
                                                        "Об игре" JOptionPane/PLAIN_MESSAGE)
                                              :quit #(.dispose frame)})))
               (return! []
                 (reset! game nil)
                 (when (.isDisplayable frame)
                   (home!) (.setVisible frame true) (theme/style-window! frame)))
               (submit! [{:keys [name host port button message]}]
                 (if (and (seq name) (seq host) port (<= 1 port 65535))
                   (let [card @active-card
                         host (if (= "localhost" (.toLowerCase host)) "127.0.0.1" host)]
                     (reset! preferences {:name name :host host :port port})
                     (.setEnabled button false)
                     (.setText message "Подключение…")
                     (future
                       (let [opened (atom nil)
                             failure! (fn [msg]
                                        (when-let [connection @opened] (client/close-connection connection))
                                        (SwingUtilities/invokeLater
                                         #(when (identical? card @active-card)
                                            (.setText message msg) (.setEnabled button true))))]
                         (try
                           (let [connection (client/connect host port)
                                 _ (reset! opened connection)
                                 result (client/authorize! connection player-id name
                                                           #(on-edt (fn [] (confirm-dialog "Сервер был перезапущен. Создать персонажа?")))
                                                           #(on-edt (fn [] (confirm-dialog "Повторить сохранение данных входа?"))))]
                             (if (= :ok (:status result))
                               (let [snapshot (client/request connection {:type :state :include-building-sequences true})]
                                 (SwingUtilities/invokeLater
                                  #(if (and (identical? card @active-card) (.isDisplayable frame))
                                     (if-let [view (open-game-window connection player-id snapshot return! (fn [] (.dispose frame)))]
                                       (do (reset! game view) (.setVisible frame false))
                                       (.setEnabled button true))
                                     (client/close-connection connection))))
                               (failure! (authorization-message (:code result)))))
                           (catch Exception _ (failure! "Нет соединения с сервером."))))))
                   (.setText message "Укажите имя, адрес и порт от 1 до 65535.")))
               (connection! []
                 (let [{:keys [name host port]} @preferences]
                   (show-card! (:panel (menu/connection-card name host port submit! home!)))))]
         (.setLayout background (GridBagLayout.))
         (update-map background {} layout/initial-objects)
         (.setContentPane frame background)
         (.setDefaultCloseOperation frame JFrame/DISPOSE_ON_CLOSE)
         (.setMinimumSize frame (Dimension. 620 470))
         (home!)
         (.pack frame)
         (.setLocationRelativeTo frame nil)
         (.setVisible frame true)
         (theme/style-window! frame)))))
  nil)

(defn -main [& [id name]]
  (show-window (if id (Long/parseLong id) 1) (or name "Игрок")))
