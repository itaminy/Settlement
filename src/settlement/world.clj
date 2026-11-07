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