(ns settlement.world)

;; Общее состояние игрового мира
(def world
  (ref
   {:resources
    {:wood 0
     :stone 0}

    :players {}}))

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