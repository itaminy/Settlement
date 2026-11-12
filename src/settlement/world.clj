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

;; Регистрация игрока
(defn add-player [player-id name]
  (dosync
    (when-not (contains? (:players @world) player-id)
      (alter world assoc-in
           [:players player-id]
           {:name name
            :x 0
            :y 0
            ;; Личный запас; :resources мира используется только для статистики.
            :collected {:wood 0
                        :stone 0}})
      true)))

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

;; Перемещение игрока
(defn move-player [player-id direction]
  (dosync
    (when (contains? (:players @world) player-id)
      (let [{:keys [x y]} (get-in @world [:players player-id])
            [new-x new-y] (case direction
                            :up    [x (dec y)]
                            :down  [x (inc y)]
                            :left  [(dec x) y]
                            :right [(inc x) y]
                            [x y])]
        (when (and (<= 0 new-x 11)
                   (<= 0 new-y 11))
          (alter world assoc-in
                 [:players player-id :x] new-x)
          (alter world assoc-in
                 [:players player-id :y] new-y)
          true)))))


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



;; Восстановление добытого объекта
(defn restore-resource [position object]
  (dosync
    (when-not (get-in @world [:objects position])
      (alter world assoc-in [:objects position] object))))

;; Планирование восстановления через 30 секунд
(defn schedule-resource-restore [position object]
  (future
    (Thread/sleep 30000)
    (restore-resource position object)))
