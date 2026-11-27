(ns settlement.bandits
  (:require [settlement.buildings :as buildings]
            [settlement.config :as config]
            [settlement.layout :as layout]))

(defn alive? [entity] (and (some? (:x entity)) (pos? (:hp entity 0))))
(defn standing? [player] (and player (not (:respawn-at player))))
(defn position [entity] [(:x entity) (:y entity)])
(defn distance [[x y] [tx ty]] (+ (abs (- x tx)) (abs (- y ty))))

(defn new-roster [now]
  (into {} (for [index (range (:count config/bandits))
                 :let [id (random-uuid)
                       team (if (even? index) :blue :red)
                       north? (even? (quot index 2))
                       xs (if (= team :blue) [18 30 41] [81 69 58])
                       ys (if north? [18 13 24] [32 40 27])
                       route (mapv vector xs ys)]]
             [id {:id id :team :bandits :watch-team team :hp 0 :max-hp (:hp config/bandits)
                  :state :waiting :direction :down :target nil :route route :waypoint 0
                  :respawn-at (+ now (:spawn-delay-ms config/bandits))}])))

(defn blocked-cells [world]
  (into (buildings/occupied-cells (:objects world))
        (concat (map position (filter standing? (vals (:players world))))
                (map position (filter alive? (vals (:bandits world)))))))

(defn safe-spawn [world blocked home]
  (let [players (map position (filter standing? (vals (:players world))))
        [hx hy] home]
    (first (for [r (range 18)
                 dy (range (- r) (inc r)) dx (range (- r) (inc r))
                 :when (= r (+ (abs dx) (abs dy)))
                 :let [p [(+ hx dx) (+ hy dy)]]
                 :when (and (config/in-bounds? p) (not (blocked p))
                            (every? #(>= (distance p %) (:safe-distance config/bandits)) players))]
             p))))

(defn first-step
  "Bounded BFS. Returns the first step on a shortest walkable path to a goal."
  [origin goals blocked limit]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [origin nil 0])
         visited #{origin}]
    (when-let [[cell step depth] (peek queue)]
      (if (goals cell)
        {:step step :destination cell}
        (let [neighbors (when (< depth limit)
                          (remove #(or (blocked %) (visited %)) (layout/neighbors cell)))]
          (recur (into (pop queue) (map #(vector % (or step %) (inc depth))) neighbors)
                 (into visited neighbors)))))))

(defn- targets [world npc]
  (let [origin (position npc)
        radius (:detect-radius config/bandits)
        retaliate (get-in npc [:target :player])
        attacked-by (get-in world [:players retaliate])]
    (if (and (standing? attacked-by) (<= (distance origin (position attacked-by)) (* radius 2)))
      [{:target {:player retaliate} :cells [(position attacked-by)]}]
      (for [[anchor building] (sort-by key
                                  (filter (fn [[anchor building]]
                                            (and (buildings/building? building)
                                                 (= (:watch-team npc) (:team building))
                                                 (<= (distance origin anchor) (+ radius 2))))
                                          (:objects world)))
            :let [cells (vec (buildings/cells anchor building))]
            :when (some #(<= (distance origin %) radius) cells)]
        {:target {:building anchor} :cells cells}))))

(defn- goals [cells blocked]
  (into #{} (comp (mapcat layout/neighbors) (remove blocked)) cells))

(defn- walk [npc step now state]
  (let [[x y] step
        direction ({[1 0] :right [-1 0] :left [0 1] :down [0 -1] :up}
                   [(- x (:x npc)) (- y (:y npc))])]
    (assoc npc :x x :y y :direction direction :state state
           :next-move (+ now (:move-ms config/bandits)))))

(defn- patrol [npc blocked now]
  (let [origin (position npc)
        route (:route npc)
        index (:waypoint npc)
        goal (route index)
        next-index (mod (inc index) (count route))
        near-goal (into #{} (remove blocked) (cons goal (layout/neighbors goal)))
        plan (first-step origin near-goal blocked 45)
        returning? (#{:chase :attack :search :return} (:state npc))]
    (cond
      (:step plan) (walk (assoc npc :target nil) (:step plan) now (if returning? :return :patrol))
      (nil? plan) (assoc npc :target nil :state :search :waypoint next-index
                         :next-move (+ now (:move-ms config/bandits)))
      :else (assoc npc :target nil :state :patrol :waypoint next-index
                   :next-move (+ now (:move-ms config/bandits))))))

(defn due? [npc now]
  (if (alive? npc)
    (or (<= (:next-move npc 0) now)
        (and (= :attack (:state npc)) (<= (:next-attack npc 0) now)))
    (<= (:respawn-at npc) now)))

(defn decide [world npc blocked now]
  (cond
    (not (alive? npc))
    (when (<= (:respawn-at npc) now)
      (when-let [[x y] (safe-spawn world blocked (first (:route npc)))]
        {:npc (-> npc (assoc :x x :y y :hp (:max-hp npc) :state :patrol :target nil
                             :next-move (+ now (:move-ms config/bandits)) :next-attack now)
                  (dissoc :respawn-at))
         :event :bandit-spawned}))

    (due? npc now)
    (let [origin (position npc)
          blocked (disj blocked origin)
          candidates (targets world npc)
          goal-targets (reduce (fn [result {:keys [target cells]}]
                                 (reduce #(assoc %1 %2 target) result (sort (goals cells blocked))))
                               {} candidates)
          plan (when (seq goal-targets)
                 (first-step origin (set (keys goal-targets)) blocked (* 3 (:detect-radius config/bandits))))
          target (get goal-targets (:destination plan))]
      (cond
        (:step plan)
        {:npc (walk (assoc npc :target target) (:step plan) now :chase) :event :bandit-moved}

        target
        (let [attack? (<= (:next-attack npc 0) now)
              npc (assoc npc :target target :state :attack
                         :next-move (+ now (:move-ms config/bandits)))]
          (if attack?
            {:npc (assoc npc :next-attack (+ now (:attack-ms config/bandits))
                         :next-move (+ now (min (:move-ms config/bandits) (:attack-ms config/bandits))))
             :attack target}
            {:npc npc}))

        :else
        (let [next (patrol npc blocked now)]
          {:npc next :event (when (not= origin (position next)) :bandit-moved)})))))
