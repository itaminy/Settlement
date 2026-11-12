(ns settlement.world
  (:require [settlement.config :as config]
            [settlement.layout :as layout]
            [settlement.inventory :as inventory]))

(def initial-objects layout/initial-objects)

;; Общее состояние игрового мира
(defonce world
  (ref
   {:resources
    {:wood 0
     :stone 0}

    :players {}

    :objects initial-objects}))

(defn migrate-inventories! []
  (dosync
    (alter world update :players
           (fn [players] (into {} (map (fn [[id player]] [id (inventory/normalize-player player)]) players)))))
  nil)

;; defonce сохраняет мир при перезагрузке namespace в работающем сервере.
(migrate-inventories!)

(defn player-at? [state position]
  (boolean (some #(= position [(:x %) (:y %)]) (vals (:players state)))))

(defn free-cell? [state position]
  (and (config/in-bounds? position)
       (not (contains? (:objects state) position))
       (not (player-at? state position))))

;; Поиск и создание выполняются в одной транзакции, включая внешнюю auth-транзакцию.
(defn add-player [player-id name]
  (dosync
    (when-not (contains? (:players @world) player-id)
      (when-let [[x y] (first (filter #(free-cell? @world %) (layout/spawn-cells)))]
        (alter world assoc-in [:players player-id]
               {:name name :x x :y y :inventory inventory/empty-inventory})
        true))))

(defn change-player-items! [player-id changes]
  (dosync
    (if-let [player (get-in @world [:players player-id])]
      (try
        (let [player (inventory/normalize-player player)
              updated (inventory/change-items (:inventory player) changes)]
          (alter world assoc-in [:players player-id] (assoc player :inventory updated))
          {:status :ok})
        (catch clojure.lang.ExceptionInfo e
          {:status :error :code (:code (ex-data e))}))
      {:status :error :code :player-not-found})))

;; Внутреннее начисление: сетевая команда :collect по-прежнему запрещена.
(defn collect-resource [player-id resource amount]
  (dosync
    (when (and (#{:wood :stone} resource) (integer? amount) (pos? amount)
               (= :ok (:status (change-player-items! player-id {resource amount}))))
      (alter world update-in [:resources resource] + amount)
      true)))

(defn get-world []
  (update @world :players
          (fn [players]
            (into {} (for [[id player] players
                           :let [player (inventory/normalize-player player)]]
                       [id (assoc player :collected (inventory/resources (:inventory player)))])))))

(defonce gathering (ref {}))
(defn now-ns [] (System/nanoTime))
(def heartbeat-timeout-ms 1500)
(defn cancel-player-gather! [id] (dosync (alter gathering dissoc id)) nil)
(defn cancel-connection-gathers! [connection]
  (dosync (alter gathering #(into {} (remove (fn [[_ action]] (= connection (:connection action))) %)))) nil)

(defn craft! [id tool]
  (if-let [cost (inventory/recipes tool)]
    (dosync (change-player-items! id (assoc (into {} (map (fn [[item n]] [item (- n)]) cost)) tool 1)))
    {:status :error :code :unknown-recipe}))

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
            (not (config/in-bounds? position))
            (fail :out-of-bounds "Destination is outside the map")
            (contains? (:objects state) position)
            (fail :object-blocked "Destination is occupied by an object")
            (player-at? state position)
            (fail :player-blocked "Destination is occupied by another player")
            :else
            (do (alter world update-in [:players player-id] assoc :x x :y y)
                (alter gathering dissoc player-id)
                {:status :ok})))))))

;; Сохраняем прежний интерфейс для внутренних вызовов.
(defn move-player [player-id direction]
  (when (= :ok (:status (move-player-result player-id direction))) true))

(declare schedule-resource-restore)

(defn- adjacent? [[x y] [tx ty]]
  (= 1 (+ (abs (- x tx)) (abs (- y ty)))))

(defn- active? [id action now]
  (let [player (get-in @world [:players id])]
    (and player
         (<= (- now (:heartbeat action)) (* heartbeat-timeout-ms 1000000))
         (= (:origin action) [(:x player) (:y player)])
         (adjacent? (:origin action) (:position action))
         (= (:object action) (get-in @world [:objects (:position action)])))))

(defn- progress [action now]
  {:status :ok :operation-id (:operation-id action) :attempt-id (:attempt-id action)
   :duration-ms (:duration-ms action)
   :elapsed-ms (min (:duration-ms action) (quot (- now (:started action)) 1000000))})

(defn start-gather! [id connection attempt-id]
  (dosync
    (let [now (now-ns) existing (get @gathering id)
          player (get-in @world [:players id])]
      (cond
        (nil? player) {:status :error :message "Player not found"}
        (not (and (string? attempt-id) (<= 1 (count attempt-id) 128))) {:status :error :code :invalid-attempt}
        (and existing (active? id existing now))
        (if (and (= connection (:connection existing)) (= attempt-id (:attempt-id existing)))
          (progress existing now) {:status :error :code :already-gathering})
        :else
        (do
          (alter gathering dissoc id)
          (let [{:keys [x y]} player
                position (some #(when (#{:tree :stone} (get-in @world [:objects %])) %)
                               [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])]
            (if-not position {:status :error :message "No resource nearby"}
              (let [object (get-in @world [:objects position])
                    items (:items (inventory/player-inventory player))
                    duration (case object :tree (if (pos? (:axe items)) 1000 3000)
                                          :stone (if (pos? (:pickaxe items)) 1500 4000))
                    action {:operation-id (str (java.util.UUID/randomUUID)) :attempt-id attempt-id
                            :connection connection :position position :object object :origin [x y]
                            :started now :heartbeat now :duration-ms duration}]
                (alter gathering assoc id action)
                (progress action now)))))))))

(defn cancel-gather! [id connection operation-id attempt-id]
  (dosync
    (let [action (get @gathering id)]
      (when (and (= connection (:connection action))
                 (or (and operation-id (= operation-id (:operation-id action)))
                     (and attempt-id (= attempt-id (:attempt-id action)))))
        (alter gathering dissoc id))
      {:status :ok :cancelled true})))

(defn update-gather! [id connection operation-id finish?]
  (let [result
        (dosync
          (let [now (now-ns) action (get @gathering id)]
            (cond
              (not (and action (= connection (:connection action)) (= operation-id (:operation-id action))))
              {:status :error :code :no-active-gather}
              (not (active? id action now))
              (do (alter gathering dissoc id) {:status :error :code :gather-cancelled})
              (not finish?)
              (do (alter gathering assoc-in [id :heartbeat] now) (progress action now))
              (< (- now (:started action)) (* (:duration-ms action) 1000000))
              (assoc (progress action now) :status :error :code :too-early)
              :else
              (let [position (:position action) object (:object action)
                    resource (case object :tree :wood :stone :stone)]
                (collect-resource id resource 1)
                (alter world update :objects dissoc position)
                ;; Исчезновение цели отменяет все конкурирующие попытки в той же транзакции.
                (alter gathering #(into {} (remove (fn [[_ a]] (= position (:position a))) %)))
                {:status :ok :completed true :resource resource :amount 1 :position position :object object}))))]
    (when (:completed result) (schedule-resource-restore (:position result) (:object result)))
    (dissoc result :object)))

;; Мгновенная добыча отключена и для старых внутренних вызовов.
(defn gather-resource [_] {:status :error :code :gather-disabled})

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
