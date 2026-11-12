(ns settlement.building-ui
  (:require [settlement.client :as client]
            [settlement.buildings :as buildings]
            [settlement.inventory :as inventory]
            [settlement.rendering :as rendering])
  (:import [javax.swing JPopupMenu JMenuItem KeyStroke AbstractAction JPanel Timer]
           [java.awt Color Font AlphaComposite RenderingHints]
           [java.awt.event KeyEvent MouseAdapter MouseMotionAdapter ActionListener]))

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

(defn install! [panel connection player-id label apply-world! cancel-gather!]
  (let [state (atom nil) cursor (atom nil) latest-key (atom nil) busy (atom false) closed (atom false)
        menu (JPopupMenu.)
        context (fn []
                  (when-let [selection @state]
                    (let [snapshot (.getClientProperty panel :settlement.gui/state)
                          camera (rendering/camera (get-in snapshot [:players player-id]) (.getWidth panel) (.getHeight panel))
                          position (when @cursor (apply screen-to-cell camera (.getWidth panel) (.getHeight panel) @cursor))]
                      {:kind (:kind selection) :position position
                       :key [(:generation selection) position snapshot] :snapshot snapshot})))
        current? (fn [key] (and (not @closed) (= key @latest-key)))
        cancel! (fn [] (reset! latest-key nil) (reset! state nil) (.setVisible menu false)
                  (.setText label "B — строительство") (.repaint panel))
        select! (fn [kind]
                  (cancel-gather!)
                  (reset! latest-key nil)
                  (reset! state {:kind kind :generation (str (java.util.UUID/randomUUID))})
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
                     (when-not
                       (client/async! connection :build
                         {:type :build :player-id player-id :building-type kind :position position}
                         (fn [result]
                           (reset! busy false)
                           (when (:world result) (apply-world! (:world result)))
                           (swap! state #(when % (dissoc % :checked-key :result)))
                           (.setText label (if (= :ok (:status result)) "Постройка готова."
                                             (get messages (:code result) "Не удалось построить.")))
                           (.repaint panel)))
                       (reset! busy false) (.setText label (:queue-full messages))))))
        motion (proxy [MouseMotionAdapter] [] (mouseMoved [event] (reset! cursor [(.getX event) (.getY event)]) (reset! latest-key nil) (.repaint panel))
                 (mouseDragged [event] (reset! cursor [(.getX event) (.getY event)]) (reset! latest-key nil) (.repaint panel)))
        mouse (proxy [MouseAdapter] []
                (mouseExited [_] (reset! cursor nil) (.repaint panel))
                (mouseClicked [event] (when (= 1 (.getButton event))
                                       (reset! cursor [(.getX event) (.getY event)]) (place!))))
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
    (doseq [[code name action] [[KeyEvent/VK_B "building-menu" #(do (cancel-gather!) (cancel!) (.show menu panel 12 12))]
                               [KeyEvent/VK_ESCAPE "cancel-building" cancel!]]]
      (.put (.getInputMap panel JPanel/WHEN_IN_FOCUSED_WINDOW) (KeyStroke/getKeyStroke code 0) name)
      (.put (.getActionMap panel) name (proxy [AbstractAction] [] (actionPerformed [_] (action)))))
    (.putClientProperty panel :settlement.gui/overlay
      (fn [graphics camera ox oy]
        (when-let [{:keys [kind position key]} (context)]
          (when position
            (draw-preview! graphics camera ox oy kind position
              (if (= key (:checked-key @state)) (get-in @state [:result :status]) :pending))))))
    (.addMouseMotionListener panel motion) (.addMouseListener panel mouse)
    (.start timer)
    {:state state :cursor cursor :select! select! :cancel! cancel! :check! check! :place! place! :menu menu
     :close! (fn [] (reset! closed true) (cancel!) (.stop timer)
               (.removeMouseMotionListener panel motion) (.removeMouseListener panel mouse)
               (.putClientProperty panel :settlement.gui/overlay nil))}))
