(ns settlement.text-protocol
  (:require [clojure.string :as str]
            [settlement.buildings :as buildings]
            [settlement.config :as config]))

(defn- quoted [value]
  (str "\""
       (apply str (map (fn [ch]
                         (case ch
                           \" "\\\""
                           \\ "\\\\"
                           (if (or (< (int ch) 32) (#{\u0085 \u2028 \u2029} ch))
                             (format "\\u%04x" (int ch))
                             ch)))
                       value))
       "\""))

(defn- atom-value [value]
  (cond
    (nil? value) "none"
    (keyword? value) (name value)
    :else (str value)))

(defn- fields [& pairs]
  (apply str (for [[key value] (partition 2 pairs)]
               (str " " key "=" (atom-value value)))))

(defn- record-value [tag & values]
  (str tag "(" (str/join "," (map atom-value values)) ")"))

(defn- list-value [values]
  (str "[" (str/join "," values) "]"))

(defn- inventory-fields [prefix {:keys [items]}]
  (apply fields (mapcat (fn [item] [(str prefix (name item)) (get items item 0)])
                       [:wood :stone :axe :pickaxe])))

(defn- building-record [[x y] {:keys [building-id kind team owner-id hp max-hp]}]
  (record-value "building" x y building-id kind team owner-id hp max-hp))

(defn- object-record [[[x y :as position] object]]
  (cond
    (buildings/building? object) (building-record position object)
    (#{:tree :stone} object) (record-value "resource" x y object)
    :else (record-value "obstacle" x y object)))

(defn- world-fields [id {:keys [world building-sequences]}]
  (let [{:keys [players objects resources match]} world
        {:keys [scores phase winner]} match]
    (str (fields "width" config/world-width "height" config/world-height "self" id
                 "players" (list-value (for [[player-id {:keys [x y team name]}] (sort-by key players)]
                                         (record-value "player" player-id x y team (quoted name))))
                 "objects" (list-value (map object-record (sort-by key objects)))
                 "total_wood" (get resources :wood 0) "total_stone" (get resources :stone 0)
                 "blue" (get scores :blue 0) "red" (get scores :red 0)
                 "phase" (or phase :active) "winner" winner)
         (inventory-fields "inventory_" (get-in players [id :inventory]))
         (fields "attack_seq" (:attack-seq building-sequences)
                 "repair_seq" (:repair-seq building-sequences)))))

(defn- progress-fields [{:keys [operation-id attempt-id duration-ms elapsed-ms]}]
  (fields "operation_id" operation-id "attempt_id" attempt-id
          "duration_ms" duration-ms "elapsed_ms" elapsed-ms))

(defn- error-fields [{:keys [code last-attack-seq last-repair-seq] :as result}]
  (str (fields "code" (or code :command-failed))
       (cond
         (= :too-early code) (progress-fields result)
         (some? last-attack-seq) (fields "last_attack_seq" last-attack-seq)
         (some? last-repair-seq) (fields "last_repair_seq" last-repair-seq))))

(defn- success-fields [{:keys [type player-id operation-id attempt-id attack-seq repair-seq]} result]
  (let [{:keys [position building anchor building-id hp]} result]
    (case type
      :state (if (contains? result :world-version)
               (str "world-versioned" (fields "version" (:world-version result))
                    (world-fields player-id result)
                    (fields "online" (list-value (:online-players result))
                            "gathering" (list-value (:gathering-players result))))
               (str "world" (world-fields player-id result)))
      :subscribe-events (str "subscribe-events"
                             (fields "since" (:since result) "version" (:world-version result)))
      :unsubscribe-events "unsubscribe-events"
      :inventory (str "inventory" (inventory-fields "" (:inventory result)))
      :move (let [{:keys [x y]} (get-in result [:world :players player-id])]
              (str "move" (fields "x" x "y" y)))
      (:gather-start :gather-heartbeat) (str (name type) (progress-fields result))
      :gather-finish (str "gather-finish"
                          (fields "operation_id" operation-id "resource" (:resource result)
                                  "amount" (:amount result) "x" (first position) "y" (second position)))
      :gather-cancel (str "gather-cancel"
                          (fields "operation_id" operation-id "attempt_id" attempt-id))
      :build (str "build" (fields "building" (building-record position building)))
      :attack-building (str "attack-building"
                            (fields "attack_seq" attack-seq "x" (first anchor) "y" (second anchor)
                                    "building_id" building-id "hp" hp "destroyed" (:destroyed? result)))
      :repair-building (str "repair-building"
                            (fields "repair_seq" repair-seq "x" (first anchor) "y" (second anchor)
                                    "building_id" building-id "hp" hp "restored_hp" (:restored-hp result))))))

(defn write-reply [command result]
  (if (= :ok (:status result))
    (str "reply v=1 ok " (success-fields command result))
    (str "reply v=1 error" (error-fields result))))

(defn write-event [{:keys [version seq changes resync]}]
  (str "event v=1" (fields "version" version "seq" seq
                           "changes" (list-value (map name changes)) "resync" resync)))
