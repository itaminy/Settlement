(ns settlement.world)

;; Общее состояние игрового мира
(def world
  (ref
   {:resources
    {:wood 0
     :stone 0}

    :players {}

    :objects
    {[3 4] :tree
     [7 2] :stone
     [8 6] :house
     [5 5] :tree}}))

;; Регистрация игрока
(defn add-player [player-id name]
  (dosync
    (alter world assoc-in
           [:players player-id]
           {:name name
            :x 0
            :y 0
            :collected {:wood 0
                        :stone 0}})))

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
