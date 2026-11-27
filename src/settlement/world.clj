(ns settlement.world
  (:require [settlement.config :as config]
            [settlement.bandits :as bandits]
            [settlement.layout :as layout]
            [settlement.inventory :as inventory]
            [settlement.buildings :as buildings]
            [settlement.event-log :as events]))

(def initial-objects layout/initial-objects)

(def ^:private victory-score 100)
(def initial-match {:scores {:blue 0 :red 0} :phase :active :winner nil})
(declare combat-time)

(defn- duration-ms [state]
  (when-let [start (:match-started-at state)]
    (max 0 (- (or (:match-ended-at state) (combat-time)) start))))

(defn- recalculate-match [{:keys [objects match] :as state}]
  (let [scores (reduce (fn [scores {:keys [team kind] :as object}]
                         (if (and (buildings/building? object) (contains? scores team))
                           (update scores team + (get-in buildings/catalog [kind :points] 0))
                           scores))
                       (:scores initial-match) (vals objects))
        ;; В старом мире, где обе команды уже набрали порог, побеждает лидер.
        leader (if (>= (:blue scores) (:red scores)) :blue :red)
        winner (or (:winner match)
                   (when (>= (scores leader) victory-score) leader))]
    (cond-> (assoc state :match {:scores scores :phase (if winner :finished :active) :winner winner})
      (and winner (:match-started-at state) (nil? (:match-ended-at state)))
      (assoc :match-ended-at (combat-time)))))

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

(declare combat-time)

(defn match-id [state] (get state :match-id 0))

(defn- team-spawn-cells [team]
  (if (= :red team) @layout/red-spawn-cells (layout/spawn-cells)))

(defn- spawn-positions [players accessible]
  (reduce (fn [positions [id {:keys [team]}]]
            (let [used (set (vals positions))]
              (if-let [position (first (filter #(and (accessible %) (not (used %)))
                                              (team-spawn-cells team)))]
                (assoc positions id position)
                (reduced nil))))
          {} (sort-by key players)))

(defn restart-match! [player-id expected-id]
  (dosync
    (let [state @world
          id (match-id state)
          reject (fn [code] {:status :error :code code})]
      (cond
        (not (contains? (:players state) player-id)) (reject :player-not-found)
        (not= id expected-id) (reject :stale-match)
        (not (match-finished? state)) (reject :match-active)
        :else
        (let [accessible (buildings/spawn-accessible-cells initial-objects)
              positions (spawn-positions (:players state) accessible)]
          (if (nil? positions)
            (reject :map-full)
            (let [players (into {} (for [[pid [x y]] positions]
                                     [pid (assoc (select-keys (get-in state [:players pid]) [:name :team])
                                                 :x x :y y :inventory inventory/empty-inventory)]))]
              (ref-set world (cond-> {:players players :objects initial-objects
                              :bot-players (:bot-players state)
                              :resources {:wood 0 :stone 0} :match initial-match :match-id (inc id)
                              :previous-match {:id id :result (:match state)}}
                               (:bandits state) (assoc :bandits (bandits/new-roster (combat-time))
                                                       :bandit-stats {})
                               (:match-started-at state) (assoc :match-started-at (combat-time))
                               (:match-started-at state) (assoc-in [:previous-match :duration-ms] (duration-ms state))
                               (:bandit-stats state) (assoc-in [:previous-match :bandit-stats] (:bandit-stats state))))
              (ref-set gathering {})
              (events/append! [:match-started :score-changed])
              {:status :ok})))))))

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
  (boolean (some #(and (bandits/standing? %) (= position (bandits/position %))) (vals (:players state)))))

(defn bandit-at? [state position]
  (boolean (some #(and (bandits/alive? %) (= position (bandits/position %))) (vals (:bandits state)))))

(defn free-cell? [state position]
  (and (config/in-bounds? position)
       (not (buildings/occupied? (:objects state) position))
       (not (player-at? state position))
       (not (bandit-at? state position))))

;; Поиск и создание выполняются в одной транзакции, включая внешнюю auth-транзакцию.
(defn add-player
  ([player-id name] (add-player player-id name nil))
  ([player-id name team]
   (dosync
     (when-not (or (match-finished? @world) (contains? (:players @world) player-id))
       (let [accessible (buildings/spawn-accessible-cells (:objects @world))
             candidates (if team (team-spawn-cells team) (layout/spawn-cells))]
         (when-let [[x y] (first (filter #(and (accessible %) (free-cell? @world %)) candidates))]
           (alter world assoc-in [:players player-id]
                  {:name name :x x :y y :inventory inventory/empty-inventory
                   :team (or team (smaller-team (:players @world)))})
           (when (= :red team)
             (alter world update :bot-players (fnil conj #{}) player-id))
           true))))))

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
  (let [state @world]
    (update (cond-> (dissoc state :building-attacks :building-repairs :bot-players :bot-command-times
                            :match-started-at :match-ended-at)
              (:match-started-at state) (assoc :match-duration-ms (duration-ms state)
                                               :gathering-players (set (keys @gathering)))) :players
          (fn [players]
            (into {} (for [[id player] players
                           :let [player (inventory/normalize-player player)]]
                       [id (assoc player :collected (inventory/resources (:inventory player)))]))))))

(defn building-command-sequences [player-id]
  (let [state @world]
    {:attack-seq (get-in state [:building-attacks player-id :seq] 0)
     :repair-seq (get-in state [:building-repairs player-id :seq] 0)}))

(defn now-ns [] (System/nanoTime))
(defn combat-time [] (quot (now-ns) 1000000))

(defn bot-cooled-down? [state id type now]
  (let [times (get-in state [:bot-command-times id])
        enough-time? (fn [clock delay]
                       (if-let [then (get times clock)]
                         (>= (unchecked-subtract (long now) (long then))
                             (* 1000000 (config/bot-timing-ms delay)))
                         true))]
    (or (not (contains? (:bot-players state) id))
        (and (enough-time? :build :after-build)
             (if (= type :move) (enough-time? :move :move)
                 (enough-time? :action :action))))))

(defn record-bot-command! [id type now]
  (when (contains? (:bot-players @world) id)
    (alter world update-in [:bot-command-times id] assoc
           (if (= type :move) :move :action) now)
    (when (= type :build)
      (alter world assoc-in [:bot-command-times id :build] now))))
(def ^:private attack-damage 10)
(def ^:private attack-interval-ns 1000000000)

(defn- valid-building-command? [position building-id command-seq]
  (and (integer? command-seq) (<= 1 command-seq Long/MAX_VALUE)
       (uuid? building-id) (buildings/cell-coordinate? position) (config/in-bounds? position)))

(defn- damage-building! [state anchor building damage]
  (let [hp (max 0 (- (:hp building) damage))
        destroyed? (zero? hp)]
    (if destroyed?
      (alter world #(recalculate-match (update % :objects dissoc anchor)))
      (alter world assoc-in [:objects anchor :hp] hp))
    (events/append! (into (cond-> [:building-attacked] destroyed? (conj :building-destroyed))
                          (match-changes state @world)))
    {:status :ok :anchor anchor :building-id (:building-id building) :hp hp :destroyed? destroyed?}))

(defn- apply-bandit-attack! [state player-id position id now last-hit]
  (let [player (get-in state [:players player-id])
        npc (get-in state [:bandits id])
        fail (fn [code] {:status :error :code code})]
    (cond
      (not (bandits/standing? player)) (fail :player-downed)
      (not (bandits/alive? npc)) (fail :bandit-not-found)
      (not= position (bandits/position npc)) (fail :stale-bandit)
      (not= 1 (bandits/distance position (bandits/position player))) (fail :too-far)
      (and last-hit (< (unchecked-subtract (long now) (long last-hit)) attack-interval-ns))
      (fail :attack-cooldown)
      :else
      (let [hp (max 0 (- (:hp npc) attack-damage))
            killed? (zero? hp)
            now-ms (quot now 1000000)]
        (alter world update-in [:bandits id]
               (fn [current]
                 (if killed?
                   (-> current (dissoc :x :y)
                       (assoc :hp 0 :state :waiting :target nil
                              :respawn-at (+ now-ms (:respawn-ms config/bandits))))
                   (assoc current :hp hp :target {:player player-id} :state :chase))))
        (when killed?
          (doseq [[resource amount] (:reward config/bandits)] (award-resource! player-id resource amount))
          (alter world update-in [:bandit-stats :bandits-killed] (fnil inc 0)))
        (events/append! (cond-> [:bandit-attacked] killed? (conj :bandit-killed :inventory-changed)))
        {:status :ok :anchor position :bandit-id id :hp hp :destroyed? killed?}))))

(defn- apply-building-attack! [state player-id position building-id now last-hit]
  (let [player (get-in state [:players player-id])
        {:keys [anchor building] :as target} (buildings/target-at (:objects state) position)
        team (:team player)
        target-team (:team building)
        fail (fn [code] {:status :error :code code})]
    (cond
      (nil? player) (fail :player-not-found)
      (not (bandits/standing? player)) (fail :player-downed)
      (nil? target) (fail :building-not-found)
      (not= building-id (:building-id building)) (fail :stale-building)
      (not (and (#{:blue :red} team) (#{:blue :red} target-team) (not= team target-team)))
      (fail :invalid-target-team)
      (not (buildings/adjacent-to-building? player target)) (fail :too-far)
      (and last-hit (< (unchecked-subtract (long now) (long last-hit)) attack-interval-ns))
      (fail :attack-cooldown)
      :else
      (damage-building! state anchor building attack-damage))))

(defn- attack! [kind player-id position target-id attack-seq]
  (dosync
    ;; Последний обработанный номер и кулдаун принадлежат игроку и переживают смену сессии.
    (let [state @world
          previous (get-in state [:building-attacks player-id])
          last-seq (:seq previous 0)
          target [kind position target-id]]
      (cond
        (match-finished? state) match-finished-result
        (not (valid-building-command? position target-id attack-seq))
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
              result ((if (= kind :bandit) apply-bandit-attack! apply-building-attack!)
                      state player-id position target-id now last-hit)]
          (alter world assoc-in [:building-attacks player-id]
                 {:seq attack-seq :target target :result result
                  :last-hit (if (= :ok (:status result)) now last-hit)})
          result)))))

(defn attack-building! [player-id position target-id sequence]
  (attack! :building player-id position target-id sequence))

(defn attack-bandit! [player-id position target-id sequence]
  (attack! :bandit player-id position target-id sequence))

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
      (not (bandits/standing? player)) (fail :player-downed)
      (nil? target) (fail :building-not-found)
      (not= building-id (:building-id building)) (fail :stale-building)
      (not (and (#{:blue :red} team) (= team (:team building)))) (fail :invalid-target-team)
      (not= player-id (:owner-id building)) (fail :not-building-owner)
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
    (cond
      (match-finished? @world) match-finished-result
      (get-in @world [:players id :respawn-at]) {:status :error :code :player-downed}
      :else (if-let [cost (inventory/recipes tool)]
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
        (not (bandits/standing? player)) (fail :player-downed "Player is downed")
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
            (bandit-at? state position) (fail :object-blocked "Destination is occupied by a bandit")
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
(def ^:dynamic ^:private *restore-match-id* nil)

(defn- adjacent? [[x y] [tx ty]]
  (= 1 (+ (abs (- x tx)) (abs (- y ty)))))

(defn- active? [id action now]
  (let [player (get-in @world [:players id])]
    (and (bandits/standing? player)
         (<= (- now (:heartbeat action)) (* heartbeat-timeout-ms 1000000))
         (= (:origin action) [(:x player) (:y player)])
         (adjacent? (:origin action) (:position action))
         (= (:object action) (get-in @world [:objects (:position action)])))))

(defn active-gather-retry? [id connection attempt-id now]
  (when-let [action (get @gathering id)]
    (and (= connection (:connection action))
         (= attempt-id (:attempt-id action))
         (active? id action now))))

(defn- progress [action now]
  {:status :ok :operation-id (:operation-id action) :attempt-id (:attempt-id action)
   :position (:position action) :resource (case (:object action) :tree :wood :stone)
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
        (not (bandits/standing? player)) {:status :error :code :player-downed}
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
              (not (bot-cooled-down? @world id :gather-finish now))
              {:status :error :code :bot-cooldown}
              :else
              (let [position (:position action) object (:object action)
                    resource (case object :tree :wood :stone :stone)]
                (award-resource! id resource 1)
                (record-bot-command! id :gather-finish now)
                (alter world update :objects dissoc position)
                ;; Исчезновение цели отменяет все конкурирующие попытки в той же транзакции.
                (alter gathering #(into {} (remove (fn [[_ a]] (= position (:position a))) %)))
                (events/append! [:gather-finished :resource-removed])
                {:status :ok :completed true :resource resource :amount 1
                 :position position :object object :match-id (match-id @world)}))))]
    (when (:completed result)
      (binding [*restore-match-id* (:match-id result)]
        (schedule-resource-restore (:position result) (:object result))))
    (dissoc result :object :match-id)))

;; Мгновенная добыча отключена и для старых внутренних вызовов.
(defn gather-resource [_] {:status :error :code :gather-disabled})

(defn restore-resource [position object]
  (dosync
    (cond
      (not (#{:tree :stone} object)) :invalid-resource
      (and (some? *restore-match-id*) (not= *restore-match-id* (match-id @world))) :stale-match
      (match-finished? @world) :match-finished
      (buildings/occupied? (:objects @world) position) :exists
      (or (player-at? @world position) (bandit-at? @world position)) :occupied
      :else (do (alter world assoc-in [:objects position] object)
                (events/append! [:resource-restored])
                :restored))))

(defn wait-for-restore [milliseconds]
  (Thread/sleep milliseconds))

(defn schedule-resource-restore [position object]
  (binding [*restore-match-id* (or *restore-match-id* (match-id @world))]
    (future
      (wait-for-restore 30000)
      (loop []
        (when (= :occupied (restore-resource position object))
          (wait-for-restore 1000)
          (recur))))))

(defn initialize-bandits! []
  (dosync
    (when-not (:bandits @world)
      (let [now (combat-time)]
        (alter world assoc :bandits (bandits/new-roster now) :bandit-stats {}
               :match-started-at now)))))

(defn- resolve-bandit-hit! [target now]
  (let [state @world
        damage (:damage config/bandits)]
    (if-let [anchor (:building target)]
      (when-let [building (get-in state [:objects anchor])]
        (let [result (damage-building! state anchor building damage)]
          (alter world update-in [:bandit-stats :building-attacks] (fnil inc 0))
          (alter world update-in [:bandit-stats :building-attacks-by-team (:team building)] (fnil inc 0))
          (when (:destroyed? result)
            (alter world update-in [:bandit-stats :buildings-destroyed] (fnil inc 0)))))
      (let [id (:player target)
            player (get-in state [:players id])]
        (when (bandits/standing? player)
          (let [hp (max 0 (- (:hp player (:hp config/player-combat)) damage))
                downed? (zero? hp)
                gathering? (and downed? (contains? @gathering id))]
            (alter world update-in [:players id] assoc :hp hp)
            (alter world update-in [:bandit-stats :player-attacks] (fnil inc 0))
            (when downed?
              (alter world assoc-in [:players id :respawn-at] (+ now (:respawn-ms config/player-combat)))
              (alter gathering dissoc id))
            (events/append! (cond-> [:player-attacked] downed? (conj :player-downed)
                                     gathering? (conj :gather-cancelled)))))))))

(defn tick-bandits! [now]
  (dosync
    (when (and (:bandits @world) (not (match-finished? @world)))
      (doseq [[id player] (sort-by key (:players @world))
              :when (and (:respawn-at player) (<= (:respawn-at player) now))]
        (when-let [[x y] (first (filter #(free-cell? @world %) (team-spawn-cells (:team player))))]
          (alter world update-in [:players id] #(-> % (assoc :x x :y y :hp (:hp config/player-combat))
                                                       (dissoc :respawn-at)))
          (events/append! [:player-respawned])))
      (when-let [due (seq (sort (for [[id npc] (:bandits @world) :when (bandits/due? npc now)] id)))]
        (let [blocked (volatile! (bandits/blocked-cells @world))]
          (doseq [id due]
            (let [state @world npc (get-in state [:bandits id])]
              (when-let [{:keys [npc event attack]} (bandits/decide state npc @blocked now)]
                (let [previous (get-in state [:bandits id])]
                  (when (bandits/alive? previous) (vswap! blocked disj (bandits/position previous)))
                  (when (bandits/alive? npc) (vswap! blocked conj (bandits/position npc)))
                  (alter world assoc-in [:bandits id] npc)
                  (when event (events/append! [event]))
                  (when attack (resolve-bandit-hit! attack now)))))))))))
