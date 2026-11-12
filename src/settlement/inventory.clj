(ns settlement.inventory)

(def item-order [:wood :stone :axe :pickaxe])
(def item-names {:wood "Дерево" :stone "Камень" :axe "Топор" :pickaxe "Кирка"})
(def capacity 12)
(def empty-inventory {:items {:wood 0 :stone 0 :axe 0 :pickaxe 0}})
(def recipes {:axe {:wood 3 :stone 2} :pickaxe {:wood 2 :stone 3}})

(defn player-inventory [player]
  ;; После миграции старое поле никогда не перекрывает актуальный инвентарь.
  (if (contains? player :inventory)
    (update (:inventory player) :items #(merge (:items empty-inventory) %))
    (update empty-inventory :items merge (select-keys (:collected player) [:wood :stone]))))

(defn normalize-player [player]
  (-> player (assoc :inventory (player-inventory player)) (dissoc :collected)))

(defn resources [inventory]
  (select-keys (:items inventory) [:wood :stone]))

(defn free-slots [inventory]
  (- capacity (count (filter pos? (vals (:items inventory))))))

(defn change-items
  "Pure operation for a future atomic server-side cost/reward transaction."
  [inventory changes]
  (when-not (and (map? changes)
                 (every? (fn [[item amount]]
                           (and (contains? item-names item) (integer? amount))) changes))
    (throw (ex-info "Invalid inventory change" {:code :invalid-items})))
  (let [result (reduce-kv #(update-in %1 [:items %2] + %3) inventory changes)]
    (when (some neg? (vals (:items result)))
      (throw (ex-info "Insufficient items" {:code :insufficient-items})))
    result))
