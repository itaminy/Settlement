(ns settlement.mechanics-test
  (:require [clojure.test :refer :all]
            [settlement.world :as world]
            [settlement.auth :as auth]
            [settlement.server :as server]
            [settlement.test-support :as support]))

(use-fixtures :each
  (fn [f]
    (reset! support/time-ns 0)
    (with-redefs [world/world (ref {:players {} :resources {:wood 0 :stone 0} :objects {[1 0] :tree [3 0] :stone}})
                  world/gathering (ref {}) auth/state (ref (auth/new-state))
                  world/now-ns #(deref support/time-ns)
                  world/schedule-resource-restore (fn [_ _] nil)] (f))))

(defn- register! [id]
  (server/process-message id {:type :register :player-id id :name (str id)}))
(defn- start! [id]
  (server/process-message id {:type :gather-start :player-id id :attempt-id (str (java.util.UUID/randomUUID))}))
(defn- command! [id action type]
  (server/process-message id {:type type :player-id id :operation-id (:operation-id action)}))
(defn- hold! [actions ms]
  (loop [remaining ms]
    (when (pos? remaining)
      (let [step (min 250 remaining)]
        (support/advance! step)
        (doseq [[id action] actions] (command! id action :gather-heartbeat))
        (recur (- remaining step))))))
(defn- race [operations]
  (let [ready (java.util.concurrent.CountDownLatch. (count operations)) start (promise)
        tasks (mapv #(future (.countDown ready) @start (%)) operations)]
    (try
      (is (.await ready 5 java.util.concurrent.TimeUnit/SECONDS))
      (deliver start true)
      (mapv #(deref % 5000 ::timeout) tasks)
      (finally (deliver start true) (doseq [task tasks] (future-cancel task))))))

(deftest all-four-mining-durations-test
  (doseq [[object tool duration resource] [[:tree nil 3000 :wood] [:tree :axe 1000 :wood]
                                           [:stone nil 4000 :stone] [:stone :pickaxe 1500 :stone]]]
    (dosync (ref-set world/world {:players {} :resources {:wood 0 :stone 0} :objects {[1 0] object}})
            (ref-set world/gathering {}) (ref-set auth/state (auth/new-state)))
    (register! 1)
    (when tool (world/change-player-items! 1 {tool 1}))
    (let [action (start! 1)]
      (is (= duration (:duration-ms action)))
      (is (= :too-early (:code (command! 1 action :gather-finish))))
      (hold! {1 action} (dec duration))
      (is (= :too-early (:code (command! 1 action :gather-finish))))
      (is (= 0 (get-in (world/get-world) [:players 1 :inventory :items resource])))
      (hold! {1 action} 1)
      (is (:completed (command! 1 action :gather-finish)))
      (is (= 1 (get-in (world/get-world) [:players 1 :inventory :items resource])))
      (when tool (is (= 1 (get-in (world/get-world) [:players 1 :inventory :items tool])))))))

(deftest server-time-and-repeated-requests-test
  (register! 1)
  (let [start-message {:type :gather-start :player-id 1 :attempt-id "attempt" :duration-ms 1 :elapsed-ms 999999}
        action (server/process-message 1 start-message)]
    (is (= 3000 (:duration-ms action)))
    (is (= (:operation-id action) (:operation-id (server/process-message 1 start-message))))
    (is (= :already-gathering (:code (start! 1))))
    (is (= :gather-disabled (:code (server/process-message 1 {:type :gather :player-id 1}))))
    (is (= :too-early (:code (server/process-message 1 {:type :gather-finish :player-id 1
                                                      :operation-id (:operation-id action) :elapsed-ms 999999}))))
    (hold! {1 action} 3000)
    (let [restores (atom [])]
      (with-redefs [world/schedule-resource-restore (fn [position object] (swap! restores conj [position object]))]
        (is (:completed (command! 1 action :gather-finish)))
        (is (= :no-active-gather (:code (command! 1 action :gather-finish))))
        (is (= [[[1 0] :tree]] @restores))))
    (is (= 1 (get-in (world/get-world) [:players 1 :inventory :items :wood])))))

(deftest cancellation-movement-disconnect-and-timeout-test
  (register! 1)
  (doseq [cancel [(fn [action] (server/process-message 1 {:type :gather-cancel :player-id 1 :attempt-id (:attempt-id action)}))
                 (fn [_] (world/move-player 1 :down) (world/move-player 1 :up))
                 (fn [action] (support/advance! 1501) (command! 1 action :gather-heartbeat))]]
    (let [action (start! 1)]
      (cancel action)
      (is (= :error (:status (command! 1 action :gather-finish))))
      (is (empty? @world/gathering))))
  (let [action (start! 1)]
    (auth/disconnect! 1)
    (is (empty? @world/gathering))
    (is (= :not-authenticated (:code (command! 1 action :gather-finish)))))
  (is (= 0 (get-in (world/get-world) [:players 1 :inventory :items :wood]))))

(deftest vanished-or-distant-target-cancels-test
  (register! 1)
  (let [action (start! 1)]
    (dosync (alter world/world update :objects dissoc [1 0]))
    (is (= :gather-cancelled (:code (command! 1 action :gather-heartbeat))))
    (world/restore-resource [1 0] :tree)
    (is (= :no-active-gather (:code (command! 1 action :gather-finish)))))
  (let [action (start! 1)]
    (dosync (alter world/world assoc-in [:players 1 :y] 5))
    (is (= :gather-cancelled (:code (command! 1 action :gather-finish))))))

(deftest foreign-mining-and-crafting-rejected-test
  (register! 1) (register! 2)
  (world/change-player-items! 1 {:wood 5 :stone 5})
  (let [action (start! 1) before (world/get-world)]
    (doseq [type [:gather-start :gather-heartbeat :gather-finish :gather-cancel :craft]]
      (is (= :wrong-player (:code (server/process-message 2 {:type type :player-id 1
                                                            :tool :axe :attempt-id "other"
                                                            :operation-id (:operation-id action)})))))
    (is (= before (world/get-world)))
    (is (= (:operation-id action) (:operation-id (get @world/gathering 1))))))

(deftest craft-costs-and-tools-survive-reconnect-test
  (let [registration (register! 1)]
    (register! 2)
    (world/change-player-items! 1 {:wood 5 :stone 5})
    (doseq [tool [:axe :pickaxe]]
      (is (= :ok (:status (server/process-message 1 {:type :craft :player-id 1 :tool tool})))))
    (is (= {:wood 0 :stone 0 :axe 1 :pickaxe 1} (get-in (world/get-world) [:players 1 :inventory :items])))
    (is (= {:wood 0 :stone 0 :axe 0 :pickaxe 0} (get-in (world/get-world) [:players 2 :inventory :items])))
    (let [before (world/get-world)]
      (is (= :insufficient-items (:code (world/craft! 1 :axe))))
      (is (= :unknown-recipe (:code (world/craft! 1 :house))))
      (is (= before (world/get-world)))
      (auth/disconnect! 1)
      (is (= :ok (:status (server/process-message :reconnected
                           (merge (select-keys registration [:token :server-instance]) {:type :login :player-id 1})))))
      (is (= before (world/get-world))))))

(deftest tool-crafted-during-mining-applies-next-time-test
  (register! 1)
  (world/change-player-items! 1 {:wood 3 :stone 2})
  (let [action (start! 1)]
    (world/craft! 1 :axe)
    (is (= 3000 (:duration-ms (get @world/gathering 1))))
    (hold! {1 action} 1000)
    (is (= :too-early (:code (command! 1 action :gather-finish))))
    (world/cancel-player-gather! 1)
    (is (= 1000 (:duration-ms (start! 1))))))

(deftest concurrent-craft-is-atomic-test
  (register! 1)
  (world/change-player-items! 1 {:wood 3 :stone 2})
  (is (= {:ok 1 :error 1} (frequencies (map :status (race [#(world/craft! 1 :axe) #(world/craft! 1 :axe)])))))
  (is (= {:wood 0 :stone 0 :axe 1 :pickaxe 0} (get-in (world/get-world) [:players 1 :inventory :items]))))

(deftest two-miners-only-one-award-test
  (register! 1) (register! 2)
  (dosync (alter world/world update :objects dissoc [3 0]))
  (let [a (start! 1) b (start! 2) restores (atom 0)]
    (hold! {1 a 2 b} 3000)
    (with-redefs [world/schedule-resource-restore (fn [& _] (swap! restores inc))]
      (is (= {:ok 1 :error 1} (frequencies (map :status
                 (race [#(command! 1 a :gather-finish) #(command! 2 b :gather-finish)])))))
      (is (= 1 @restores)))
    (is (empty? @world/gathering))
    (is (= 1 (reduce + (for [id [1 2]] (get-in (world/get-world) [:players id :inventory :items :wood])))))))

(deftest finish-versus-cancel-linearizes-at-commit-test
  (register! 1)
  (let [action (start! 1)]
    (hold! {1 action} 3000)
    (let [[finish _] (race [#(command! 1 action :gather-finish)
                            #(command! 1 action :gather-cancel)])]
      (is (= (if (:completed finish) 1 0) (get-in (world/get-world) [:players 1 :inventory :items :wood])))
      (is (empty? @world/gathering))
      (is (= :no-active-gather (:code (command! 1 action :gather-finish)))))))

(deftest stale-operation-cannot-cancel-or-finish-new-one-test
  (register! 1)
  (let [old (start! 1)]
    (command! 1 old :gather-cancel)
    (let [new (start! 1)]
      (command! 1 old :gather-cancel)
      (is (= (:operation-id new) (:operation-id (get @world/gathering 1))))
      (is (= :no-active-gather (:code (command! 1 old :gather-finish))))
      (is (= :ok (:status (command! 1 new :gather-heartbeat)))))))
