(ns settlement.world)

;; Постоянная начальная раскладка: 18 деревьев, 12 камней и 3 дома.
(def initial-objects
  {[3 4] :tree  [5 5] :tree  [2 1] :tree  [5 1] :tree
   [9 1] :tree  [1 3] :tree  [5 3] :tree  [9 3] :tree
   [7 4] :tree  [10 5] :tree [1 7] :tree  [3 7] :tree
   [7 7] :tree  [10 7] :tree [2 9] :tree  [5 9] :tree
   [9 9] :tree  [7 10] :tree
   [7 2] :stone [1 1] :stone [10 2] :stone [3 2] :stone
   [2 5] :stone [8 5] :stone [5 7] :stone  [11 8] :stone
   [1 10] :stone [4 10] :stone [9 11] :stone [11 10] :stone
   [8 6] :house [6 8] :house [3 10] :house})

;; Общее состояние игрового мира
(def world
  (ref
   {:resources
    {:wood 0
     :stone 0}

    :players {}

    :objects initial-objects}))

(defn player-at? [state position]
  (boolean (some #(= position [(:x %) (:y %)]) (vals (:players state)))))

(defn free-cell? [state [x y :as position]]
  (and (<= 0 x 11) (<= 0 y 11)
       (not (contains? (:objects state) position))
       (not (player-at? state position))))

;; Поиск и создание выполняются в одной транзакции, включая внешнюю auth-транзакцию.
(defn add-player [player-id name]
  (dosync
    (when-not (contains? (:players @world) player-id)
      (when-let [[x y] (first (for [y (range 12) x (range 12)
                                    :when (free-cell? @world [x y])] [x y]))]
        (alter world assoc-in [:players player-id]
               {:name name :x x :y y :collected {:wood 0 :stone 0}})
        true))))

;; Добыча ресурсов
(defn collect-resource [player-id resource amount]
  (dosync
    (when (contains? (:players @world) player-id)
      (alter world update-in
             [:resources resource]
             + amount)

      (alter world update-in
             [:players player-id :collected resource]
             + amount)

      true)))

;; Получение текущего состояния мира
(defn get-world []
  @world)

(defn move-player-result [player-id direction]
  (dosync
    (let [state @world
          player (get-in state [:players player-id])
          delta ({:up [0 -1] :down [0 1] :left [-1 0] :right [1 0]} direction)
          fail (fn [code message] {:status :error :code code :message message})]
      (cond
        (nil? player) (fail :player-not-found "Player not found")
        (nil? delta) (fail :invalid-direction "Unknown movement direction")
        :else
        (let [[dx dy] delta
              x (+ (:x player) dx) y (+ (:y player) dy)
              position [x y]]
          (cond
            (not (and (<= 0 x 11) (<= 0 y 11)))
            (fail :out-of-bounds "Destination is outside the map")
            (contains? (:objects state) position)
            (fail :object-blocked "Destination is occupied by an object")
            (player-at? state position)
            (fail :player-blocked "Destination is occupied by another player")
            :else
            (do (alter world update-in [:players player-id] assoc :x x :y y)
                {:status :ok})))))))

;; Сохраняем прежний интерфейс для внутренних вызовов.
(defn move-player [player-id direction]
  (when (= :ok (:status (move-player-result player-id direction))) true))

(declare schedule-resource-restore)
;; Добыча ресурса с удалением объекта

(defn gather-resource [player-id]
  (let [result
        (dosync
          (if-let [player (get-in @world [:players player-id])]
            (let [{:keys [x y]} player
                  neighbors [[(inc x) y]
                             [(dec x) y]
                             [x (inc y)]
                             [x (dec y)]]
                  objects (:objects @world)
                  target (some
                          (fn [pos]
                            (when (#{:tree :stone} (get objects pos))
                              pos))
                          neighbors)]
              (if target
                (let [object (get objects target)
                      resource (case object
                                 :tree :wood
                                 :stone :stone)]
                  (alter world update-in
                         [:resources resource] + 1)
                  (alter world update-in
                         [:players player-id :collected resource] + 1)
                  (alter world update :objects dissoc target)

                  {:status :ok
                   :resource resource
                   :amount 1
                   :position target
                   :object object})

                {:status :error
                 :message "No resource nearby"}))

            {:status :error
             :message "Player not found"}))]

    ;; Планируем восстановление после завершения транзакции
    (when (= :ok (:status result))
      (schedule-resource-restore
       (:position result)
       (:object result)))

    (dissoc result :object)))



(defn restore-resource [position object]
  (dosync
    (cond
      (contains? (:objects @world) position) :exists
      (player-at? @world position) :occupied
      :else (do (alter world assoc-in [:objects position] object) :restored))))

(defn wait-for-restore [milliseconds]
  (Thread/sleep milliseconds))

(defn schedule-resource-restore [position object]
  (future
    (wait-for-restore 30000)
    (loop []
      (when (= :occupied (restore-resource position object))
        (wait-for-restore 1000)
        (recur)))))
