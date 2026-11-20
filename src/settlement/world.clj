(ns settlement.world
  (:require [settlement.config :as config]
            [settlement.layout :as layout]
            [settlement.inventory :as inventory]
            [settlement.buildings :as buildings]
            [settlement.event-log :as events]))

(def initial-objects layout/initial-objects)

(def ^:private building-points {:fence 2 :house 15})
(def ^:private victory-score 100)
(def initial-match {:scores {:blue 0 :red 0} :phase :active :winner nil})

(defn- recalculate-match [{:keys [objects match] :as state}]
  (let [scores (reduce (fn [scores {:keys [team kind] :as object}]
                         (if (and (buildings/building? object) (contains? scores team))
                           (update scores team + (get building-points kind 0))
                           scores))
                       (:scores initial-match) (vals objects))
        ;; В старом мире, где обе команды уже набрали порог, побеждает лидер.
        leader (if (>= (:blue scores) (:red scores)) :blue :red)
        winner (or (:winner match)
                   (when (>= (scores leader) victory-score) leader))]
    (assoc state :match {:scores scores :phase (if winner :finished :active) :winner winner})))

(defn match-finished? [state]
  (= :finished (get-in state [:match :phase])))

(defn- match-changes [before after]
  (cond-> []
    (not= (get-in before [:match :scores]) (get-in after [:match :scores]))
    (conj :score-changed)
    (and (not (match-finished? before)) (match-finished? after))
    (conj :match-finished)))

(def ^:private match-finished-result {:status :error :code :match-finished})

;; Общее состояние игрового мира
(defonce world
  (ref
   {:resources
    {:wood 0
     :stone 0}

    :players {}

    :match initial-match

    :objects initial-objects}))

(defonce gathering (ref {}))

(defn migrate-inventories! []
  (dosync
    (alter world update :players
           (fn [players] (into {} (map (fn [[id player]] [id (inventory/normalize-player player)]) players)))))
  nil)

;; defonce сохраняет мир при перезагрузке namespace в работающем сервере.
(migrate-inventories!)

(defn- smaller-team [players]
  (let [{:keys [blue red] :or {blue 0 red 0}} (frequencies (map :team (vals players)))]
    (if (<= blue red) :blue :red)))

(defn migrate-teams! []
  (dosync
    (alter world
           (fn [{:keys [players objects] :as state}]
             (let [players (reduce (fn [assigned id]
                                     (if (nil? (get-in assigned [id :team]))
                                       (assoc-in assigned [id :team] (smaller-team assigned))
                                       assigned))
                                   players (sort (keys players)))
                   objects (reduce-kv (fn [assigned position object]
                                        (if-let [team (and (buildings/building? object)
                                                           (nil? (:team object))
                                                           (get-in players [(:owner-id object) :team]))]
                                          (assoc-in assigned [position :team] team)
                                          assigned))
                                      objects objects)]
               (recalculate-match (assoc state :players players :objects objects)))))
    (when (match-finished? @world)
      (ref-set gathering {})))
  nil)

(migrate-teams!)

(defn migrate-building-durability! []
  (dosync
    (alter world update :objects update-vals buildings/initialize-durability))
  nil)

(migrate-building-durability!)

(defn player-at? [state position]
  (boolean (some #(= position [(:x %) (:y %)]) (vals (:players state)))))

(defn free-cell? [state position]
  (and (config/in-bounds? position)
       (not (buildings/occupied? (:objects state) position))
       (not (player-at? state position))))

;; Поиск и создание выполняются в одной транзакции, включая внешнюю auth-транзакцию.
(defn add-player [player-id name]
  (dosync
    (when-not (or (match-finished? @world) (contains? (:players @world) player-id))
      (when-let [[x y] (let [accessible (buildings/spawn-accessible-cells (:objects @world))]
                        (first (filter #(and (accessible %) (free-cell? @world %)) (layout/spawn-cells))))]
        (alter world assoc-in [:players player-id]
               {:name name :x x :y y :inventory inventory/empty-inventory
                :team (smaller-team (:players @world))})
        true))))

(defn- change-items! [player-id changes]
  (dosync
    (if (match-finished? @world)
      match-finished-result
      (if-let [player (get-in @world [:players player-id])]
        (try
          (let [player (inventory/normalize-player player)
                updated (inventory/change-items (:inventory player) changes)]
            (alter world assoc-in [:players player-id] (assoc player :inventory updated))
            {:status :ok})
          (catch clojure.lang.ExceptionInfo e
            {:status :error :code (:code (ex-data e))}))
        {:status :error :code :player-not-found}))))

;; Внутреннее начисление: сетевая команда :collect по-прежнему запрещена.
(defn change-player-items! [player-id changes]
  (dosync
    (let [before (get-in @world [:players player-id])
          result (change-items! player-id changes)]
      (when (and (= :ok (:status result))
                 (not= (inventory/player-inventory before)
                       (get-in @world [:players player-id :inventory])))
        (events/append! [:inventory-changed]))
      result)))

(defn- award-resource! [player-id resource amount]
  (dosync
    (when (and (#{:wood :stone} resource) (integer? amount) (pos? amount)
               (= :ok (:status (change-items! player-id {resource amount}))))
      (alter world update-in [:resources resource] + amount)
      true)))

(defn collect-resource [player-id resource amount]
  (dosync
    (when (award-resource! player-id resource amount)
      (events/append! [:inventory-changed])
      true)))

(defn get-world []
  (update (dissoc @world :building-attacks :building-repairs) :players
          (fn [players]
            (into {} (for [[id player] players
                           :let [player (inventory/normalize-player player)]]
                       [id (assoc player :collected (inventory/resources (:inventory player)))])))))

(defn building-command-sequences [player-id]
  (let [state @world]
    {:attack-seq (get-in state [:building-attacks player-id :seq] 0)
     :repair-seq (get-in state [:building-repairs player-id :seq] 0)}))

(defn now-ns [] (System/nanoTime))
(def ^:private attack-damage 10)
(def ^:private attack-interval-ns 1000000000)

(defn- valid-building-command? [position building-id command-seq]
  (and (integer? command-seq) (<= 1 command-seq Long/MAX_VALUE)
       (uuid? building-id) (buildings/cell-coordinate? position) (config/in-bounds? position)))

(defn- apply-building-attack! [state player-id position building-id now last-hit]
  (let [player (get-in state [:players player-id])
        {:keys [anchor building] :as target} (buildings/target-at (:objects state) position)
        team (:team player)
        target-team (:team building)
        fail (fn [code] {:status :error :code code})]
    (cond
      (nil? player) (fail :player-not-found)
      (nil? target) (fail :building-not-found)
      (not= building-id (:building-id building)) (fail :stale-building)
      (not (and (#{:blue :red} team) (#{:blue :red} target-team) (not= team target-team)))
      (fail :invalid-target-team)
      (not (buildings/adjacent-to-building? player target)) (fail :too-far)
      (and last-hit (< (unchecked-subtract (long now) (long last-hit)) attack-interval-ns))
      (fail :attack-cooldown)
      :else
      (let [hp (max 0 (- (:hp building) attack-damage))
            destroyed? (zero? hp)]
        (if destroyed?
          (alter world #(recalculate-match (update % :objects dissoc anchor)))
          (alter world assoc-in [:objects anchor :hp] hp))
        (events/append! (into (cond-> [:building-attacked] destroyed? (conj :building-destroyed))
                              (match-changes state @world)))
        {:status :ok :anchor anchor :building-id building-id :hp hp :destroyed? destroyed?}))))

(defn attack-building! [player-id position building-id attack-seq]
  (dosync
    ;; Последний обработанный номер и кулдаун принадлежат игроку и переживают смену сессии.
    (let [state @world
          previous (get-in state [:building-attacks player-id])
          last-seq (:seq previous 0)
          target [position building-id]]
      (cond
        (match-finished? state) match-finished-result
        (not (valid-building-command? position building-id attack-seq))
        {:status :error :code :invalid-attack}
        (< attack-seq last-seq)
        {:status :error :code :stale-attack :last-attack-seq last-seq}
        (= attack-seq last-seq)
        (if (= target (:target previous))
          (:result previous)
          {:status :error :code :attack-seq-conflict :last-attack-seq last-seq})
        :else
        (let [now (now-ns)
              last-hit (:last-hit previous)
              result (apply-building-attack! state player-id position building-id now last-hit)]
          (alter world assoc-in [:building-attacks player-id]
                 {:seq attack-seq :target target :result result
                  :last-hit (if (= :ok (:status result)) now last-hit)})
          result)))))

(def ^:private repair-hp 10)
(def ^:private repair-payment {:wood -1})

(defn- apply-building-repair! [state player-id position building-id]
  (let [player (get-in state [:players player-id])
        {:keys [anchor building] :as target} (buildings/target-at (:objects state) position)
        team (:team player)
        {:keys [hp max-hp]} building
        fail (fn [code] {:status :error :code code})]
    (cond
      (nil? player) (fail :player-not-found)
      (nil? target) (fail :building-not-found)
      (not= building-id (:building-id building)) (fail :stale-building)
      (not (and (#{:blue :red} team) (= team (:team building)))) (fail :invalid-target-team)
      (not (buildings/adjacent-to-building? player target)) (fail :too-far)
      (>= hp max-hp) (fail :building-at-full-hp)
      :else
      (let [payment (change-items! player-id repair-payment)]
        (if (= :ok (:status payment))
          (let [restored (min repair-hp (- max-hp hp))
                new-hp (+ hp restored)]
            (alter world assoc-in [:objects anchor :hp] new-hp)
            (events/append! [:building-repaired])
            {:status :ok :anchor anchor :building-id building-id :hp new-hp :restored-hp restored})
          payment)))))

(defn repair-building! [player-id position building-id repair-seq]
  (dosync
    (let [state @world
          previous (get-in state [:building-repairs player-id])
          last-seq (:seq previous 0)
          target [position building-id]]
      (cond
        (match-finished? state) match-finished-result
        (not (valid-building-command? position building-id repair-seq))
        {:status :error :code :invalid-repair}
        (< repair-seq last-seq)
        {:status :error :code :stale-repair :last-repair-seq last-seq}
        (= repair-seq last-seq)
        (if (= target (:target previous))
          (:result previous)
          {:status :error :code :repair-seq-conflict :last-repair-seq last-seq})
        :else
        (let [result (apply-building-repair! state player-id position building-id)]
          (alter world assoc-in [:building-repairs player-id]
                 {:seq repair-seq :target target :result result})
          result)))))

(def heartbeat-timeout-ms 1500)
(defn cancel-player-gather! [id]
  (dosync
    (when (contains? @gathering id)
      (alter gathering dissoc id)
      (events/append! [:gather-cancelled])))
  nil)

(defn cancel-connection-gathers! [connection]
  (dosync
    (let [before @gathering
          remaining (into {} (remove (fn [[_ action]] (= connection (:connection action))) before))]
      (when (not= before remaining)
        (ref-set gathering remaining)
        (events/append! [:gather-cancelled]))))
  nil)

(defn craft! [id tool]
  (dosync
    (if (match-finished? @world)
      match-finished-result
      (if-let [cost (inventory/recipes tool)]
        (change-player-items! id (assoc (into {} (map (fn [[item n]] [item (- n)]) cost)) tool 1))
        {:status :error :code :unknown-recipe}))))

(defn check-build [player-id kind position]
  (let [state @world]
    (if (match-finished? state)
      match-finished-result
      (select-keys (buildings/validate state player-id kind position) [:status :code :cells :cost]))))

(defn build! [player-id kind position]
  (dosync
    (let [state @world
          result (if (match-finished? state) match-finished-result
                     (buildings/validate state player-id kind position))]
      (if-not (= :ok (:status result))
        (select-keys result [:status :code])
        (let [payment (change-items! player-id
                        (into {} (map (fn [[item amount]] [item (- amount)]) (:cost result))))]
          (if-not (= :ok (:status payment)) payment
            (let [building (buildings/initialize-durability (:entry result))
                  gathers @gathering]
              (alter world #(recalculate-match (assoc-in % [:objects position] building)))
              (if (match-finished? @world)
                (ref-set gathering {})
                (alter gathering dissoc player-id))
              (events/append! (into (cond-> [:building-created]
                                      (not= gathers @gathering) (conj :gather-cancelled))
                                    (match-changes state @world)))
              {:status :ok :position position :building building})))))))

(defn move-player-result [player-id direction]
  (dosync
    (let [state @world
          player (get-in state [:players player-id])
          delta ({:up [0 -1] :down [0 1] :left [-1 0] :right [1 0]} direction)
          fail (fn [code message] {:status :error :code code :message message})]
      (cond
        (match-finished? state) match-finished-result
        (nil? player) (fail :player-not-found "Player not found")
        (nil? delta) (fail :invalid-direction "Unknown movement direction")
        :else
        (let [[dx dy] delta
              x (+ (:x player) dx) y (+ (:y player) dy)
              position [x y]]
          (cond
            (not (config/in-bounds? position))
            (fail :out-of-bounds "Destination is outside the map")
            (buildings/occupied? (:objects state) position)
            (fail :object-blocked "Destination is occupied by an object")
            (player-at? state position)
            (fail :player-blocked "Destination is occupied by another player")
            :else
            (let [gathering? (contains? @gathering player-id)]
              (alter world update-in [:players player-id] assoc :x x :y y)
              (alter gathering dissoc player-id)
              (events/append! (cond-> [:player-moved] gathering? (conj :gather-cancelled)))
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

(defn- gather-target [objects [x y :as origin] {:keys [resource-type position]}]
  (let [matches? (fn [cell]
                   (let [object (get objects cell)]
                     (and (#{:tree :stone} object)
                          (or (nil? resource-type) (= resource-type object)))))]
    (if position
      (when (and (adjacent? origin position) (matches? position))
        position)
      (some #(when (matches? %) %)
            [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]))))

(defn start-gather!
  ([id connection attempt-id] (start-gather! id connection attempt-id {}))
  ([id connection attempt-id {:keys [resource-type position] :as target}]
   (dosync
    (let [state (ensure world)
          now (now-ns) existing (get @gathering id)
          player (get-in state [:players id])]
      (cond
        (match-finished? state) match-finished-result
        (nil? player) {:status :error :message "Player not found"}
        (not (and (string? attempt-id) (<= 1 (count attempt-id) 128))) {:status :error :code :invalid-attempt}
        (and (contains? target :resource-type) (not (#{:tree :stone} resource-type)))
        {:status :error :code :invalid-resource-type}
        (and (contains? target :position)
             (not (and (buildings/cell-coordinate? position) (config/in-bounds? position))))
        {:status :error :code :invalid-position}
        (and existing (active? id existing now))
        (if (and (= connection (:connection existing)) (= attempt-id (:attempt-id existing)))
          (if (= target (:target existing {}))
            (progress existing now)
            {:status :error :code :gather-attempt-conflict})
          {:status :error :code :already-gathering})
        :else
        (do
          (alter gathering dissoc id)
          (let [{:keys [x y]} player
                cell (gather-target (:objects state) [x y] target)]
            (if-not cell
              (do
                (when existing (events/append! [:gather-cancelled]))
                (if (seq target)
                  {:status :error :code :no-matching-resource}
                  {:status :error :message "No resource nearby"}))
              (let [object (get-in state [:objects cell])
                    items (:items (inventory/player-inventory player))
                    duration (case object :tree (if (pos? (:axe items)) 1000 3000)
                                          :stone (if (pos? (:pickaxe items)) 1500 4000))
                    action {:operation-id (str (java.util.UUID/randomUUID)) :attempt-id attempt-id
                            :connection connection :position cell :object object :origin [x y] :target target
                            :started now :heartbeat now :duration-ms duration}]
                (alter gathering assoc id action)
                (events/append! (cond-> [:gather-started] existing (conj :gather-cancelled)))
                (progress action now))))))))))

(defn cancel-gather! [id connection operation-id attempt-id]
  (dosync
    (if (match-finished? (ensure world))
      match-finished-result
      (let [action (get @gathering id)]
        (when (and (= connection (:connection action))
                   (or (and operation-id (= operation-id (:operation-id action)))
                       (and attempt-id (= attempt-id (:attempt-id action)))))
          (cancel-player-gather! id))
        {:status :ok :cancelled true}))))

(defn update-gather! [id connection operation-id finish?]
  (let [result
        (dosync
          (let [now (now-ns) action (get @gathering id)]
            (cond
              (match-finished? (ensure world)) match-finished-result
              (not (and action (= connection (:connection action)) (= operation-id (:operation-id action))))
              {:status :error :code :no-active-gather}
              (not (active? id action now))
              (do (cancel-player-gather! id) {:status :error :code :gather-cancelled})
              (not finish?)
              (do (alter gathering assoc-in [id :heartbeat] now) (progress action now))
              (< (- now (:started action)) (* (:duration-ms action) 1000000))
              (assoc (progress action now) :status :error :code :too-early)
              :else
              (let [position (:position action) object (:object action)
                    resource (case object :tree :wood :stone :stone)]
                (award-resource! id resource 1)
                (alter world update :objects dissoc position)
                ;; Исчезновение цели отменяет все конкурирующие попытки в той же транзакции.
                (alter gathering #(into {} (remove (fn [[_ a]] (= position (:position a))) %)))
                (events/append! [:gather-finished :resource-removed])
                {:status :ok :completed true :resource resource :amount 1 :position position :object object}))))]
    (when (:completed result) (schedule-resource-restore (:position result) (:object result)))
    (dissoc result :object)))

;; Мгновенная добыча отключена и для старых внутренних вызовов.
(defn gather-resource [_] {:status :error :code :gather-disabled})

(defn restore-resource [position object]
  (dosync
    (cond
      (match-finished? @world) :match-finished
      (buildings/occupied? (:objects @world) position) :exists
      (player-at? @world position) :occupied
      :else (do (alter world assoc-in [:objects position] object)
                (events/append! [:resource-restored])
                :restored))))

(defn wait-for-restore [milliseconds]
  (Thread/sleep milliseconds))

(defn schedule-resource-restore [position object]
  (future
    (wait-for-restore 30000)
    (loop []
      (when (= :occupied (restore-resource position object))
        (wait-for-restore 1000)
        (recur)))))
