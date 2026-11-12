(ns settlement.transport-test
  (:require [clojure.test :refer :all]
            [settlement.layout :as layout]
            [settlement.transport :as transport] [settlement.client :as client]
            [settlement.gathering-ui :as gathering-ui] [settlement.world :as world]
            [settlement.auth :as auth] [settlement.server :as server]
            [settlement.gui :as gui] [settlement.inventory :as inventory]
            [settlement.inventory-ui :as inventory-ui]
            [settlement.test-support :as support])
  (:import [javax.swing SwingUtilities JProgressBar]
           [java.util.concurrent CountDownLatch TimeUnit]))

(defn- edt! [f]
  (let [result (promise)] (SwingUtilities/invokeAndWait #(deliver result (f))) @result))

(deftest slow-network-coalesces-heartbeats-and-polls-test
  (let [entered (promise) release (promise) calls (atom []) request-threads (atom []) done (CountDownLatch. 3)
        queue (transport/create-queue (fn [message]
                  (swap! request-threads conj (SwingUtilities/isEventDispatchThread))
                  (swap! calls conj message)
                  (when (= :blocked message) (deliver entered true) @release)
                  {:status :ok}))
        callback (fn [_] (.countDown done))]
    (try
      (transport/submit! queue :blocked :blocked callback (constantly true) false)
      (is (= true (deref entered 5000 :timeout)))
      (is (transport/submit! queue :heartbeat :heartbeat callback (constantly true) false))
      (is (transport/submit! queue :state :state callback (constantly true) false))
      (dotimes [_ 100]
        (is (false? (transport/submit! queue :heartbeat :heartbeat callback (constantly true) false)))
        (is (false? (transport/submit! queue :state :state callback (constantly true) false))))
      (is (= 2 (.size (:jobs queue))))
      (deliver release true)
      (is (.await done 5 TimeUnit/SECONDS))
      (is (= [:blocked :heartbeat :state] @calls))
      (is (every? false? @request-threads))
      (finally (deliver release true) (transport/stop! queue)))))

(deftest bounded-queue-prioritizes-cancel-test
  (let [entered (promise) release (promise) calls (atom []) done (CountDownLatch. 18)
        queue (transport/create-queue (fn [message]
                  (swap! calls conj message)
                  (when (= :blocked message) (deliver entered true) @release) {:status :ok}))
        callback (fn [_] (.countDown done))]
    (try
      (transport/submit! queue :blocked :blocked callback (constantly true) false)
      (is (= true (deref entered 5000 :timeout)))
      (dotimes [i transport/queue-capacity]
        (is (transport/submit! queue i i callback (constantly true) false)))
      (is (false? (transport/submit! queue :overflow :overflow callback (constantly true) false)))
      (is (transport/submit! queue :cancel :cancel callback (constantly true) true))
      (is (= transport/queue-capacity (.size (:jobs queue))))
      (deliver release true)
      (is (.await done 5 TimeUnit/SECONDS))
      (is (= :cancel (second @calls)))
      (is (= 17 (count @calls)))
      (finally (deliver release true) (transport/stop! queue)))))

(deftest async-response-delivered-on-edt-test
  (let [result (promise)
        queue (transport/create-queue (fn [_] {:status :ok :worker-edt? (SwingUtilities/isEventDispatchThread)}))]
    (try
      (edt! #(client/async! {:queue queue} :test {:type :state}
                           (fn [response] (deliver result [response (SwingUtilities/isEventDispatchThread)]))))
      (is (= [{:status :ok :worker-edt? false} true] (deref result 5000 :timeout)))
      (finally (transport/stop! queue)))))

(deftest network-error-is-sanitized-test
  (let [result (promise) failures (atom 0)
        queue (transport/create-queue (fn [_] (throw (Exception. "private request contents"))) #(swap! failures inc))]
    (try
      (transport/submit! queue :test {} #(deliver result %) (constantly true) false)
      (is (= {:status :error :code :connection-failed} (deref result 5000 :timeout)))
      (is (= 1 @failures)) (is @(:closed queue))
      (is (false? (transport/submit! queue :again {} identity (constantly true) false)))
      (finally (transport/stop! queue)))))

(defn- with-mining [f]
  (reset! support/time-ns 0)
  (with-redefs [layout/spawn-cells support/row-major-spawn
                  world/world (ref {:players {} :objects {[1 0] :tree} :resources {:wood 0 :stone 0}})
                world/gathering (ref {}) auth/state (ref (auth/new-state))
                world/now-ns #(deref support/time-ns)
                gathering-ui/now-ms #(quot @support/time-ns 1000000)
                world/schedule-resource-restore (fn [& _] nil)]
    (server/process-message :a {:type :register :player-id 1 :name "A"}) (f)))

(deftest release-before-delayed-start-response-test
  (with-mining
   (fn []
     (let [entered (promise) release (promise) refreshed (promise)
           queue (transport/create-queue (fn [message]
                    (when (= :gather-start (:type message)) (deliver entered true) @release)
                    (server/process-message :a message)))
           progress (JProgressBar. 0 100)
           controller (gathering-ui/create-controller {:queue queue} 1 progress #(deliver refreshed true))]
       (try
         (edt! (:press! controller))
         (is (= true (deref entered 5000 :timeout)))
         (edt! (:release! controller))
         (is (nil? @(:state controller))) (is (not (.isVisible progress)))
         (is (= 0 (.getValue progress)))
         (deliver release true)
         (is (= true (deref refreshed 5000 :timeout)))
         (edt! (fn [] nil))
         (is (empty? @world/gathering)) (is (nil? @(:state controller)))
         (is (= 0 (get-in (world/get-world) [:players 1 :inventory :items :wood])))
         (finally (deliver release true) (transport/stop! queue)))))))

(deftest stale-finish-response-does-not-restart-progress-test
  (with-mining
   (fn []
     (let [committed (promise) release (promise) refreshed (promise)
           action (world/start-gather! 1 :a "local")
           queue (transport/create-queue (fn [message]
                    (let [response (server/process-message :a message)]
                      (when (= :gather-finish (:type message)) (deliver committed true) @release) response)))
           progress (JProgressBar. 0 100)
           controller (gathering-ui/create-controller {:queue queue} 1 progress #(deliver refreshed true))]
       (try
         (dotimes [_ 12] (support/advance! 250) (world/update-gather! 1 :a (:operation-id action) false))
         (reset! (:pressed controller) true)
         (reset! (:state controller) (assoc action :stamp 3000 :elapsed-ms 3000 :last-request 0))
         (edt! (:tick! controller))
         (is (= true (deref committed 5000 :timeout)))
         (edt! (:release! controller))
         (is (nil? @(:state controller))) (is (not (.isVisible progress)))
         (deliver release true)
         (is (= true (deref refreshed 5000 :timeout)))
         (edt! (fn [] nil))
         (is (nil? @(:state controller)))
         (is (= 1 (get-in (world/get-world) [:players 1 :inventory :items :wood])))
         (finally (deliver release true) (transport/stop! queue)))))))

(defn- await-state! [state predicate trigger]
  (let [result (promise) key (Object.)]
    (add-watch state key (fn [_ _ _ value] (when (predicate value) (deliver result true))))
    (try
      (trigger)
      (when (predicate @state) (deliver result true))
      (is (= true (deref result 5000 :timeout)))
      (finally (remove-watch state key)))))

(deftest held-key-progress-and-completion-test
  (with-mining
   (fn []
     (let [queue (transport/create-queue #(server/process-message :a %))
           progress (JProgressBar. 0 100) refreshed (promise)
           controller (gathering-ui/create-controller {:queue queue} 1 progress #(deliver refreshed true))]
       (try
         (await-state! (:state controller) :operation-id #(edt! (:press! controller)))
         (doseq [step (range 1 13)]
           (support/advance! 250)
           (await-state! (:state controller)
                         (fn [state] (if (= step 12) (nil? state) (= (* step 250) (:elapsed-ms state))))
                         #(edt! (:tick! controller))))
         (is (= true (deref refreshed 5000 :timeout)))
         (is (= 1 (get-in (world/get-world) [:players 1 :inventory :items :wood])))
         (is (not (.isVisible progress)))
         (edt! (:press! controller))
         (is (nil? @(:state controller)))
         (edt! (:release! controller))
         (is (false? @(:pressed controller)))
         (finally (transport/stop! queue)))))))

(deftest craft-buttons-run-off-edt-and-update-inventory-test
  (with-mining
   (fn []
     (world/change-player-items! 1 {:wood 3 :stone 2})
     (let [threads (atom [])
           queue (transport/create-queue (fn [message]
                                          (swap! threads conj (SwingUtilities/isEventDispatchThread))
                                          (server/process-message :a message)))
           ui (inventory-ui/create-panel (inventory/player-inventory (get-in (world/get-world) [:players 1])))
           resource-ui (assoc (gui/create-resource-panel {:wood 3 :stone 2}) :inventory-ui ui)
           succeeded (promise) rejected (promise)]
       (try
         (edt! #(do
                  (.addPropertyChangeListener (:message ui) "text"
                    (proxy [java.beans.PropertyChangeListener] []
                      (propertyChange [event]
                        (case (.getNewValue event)
                          "Инструмент создан." (deliver succeeded true)
                          "Недостаточно дерева или камня." (deliver rejected true)
                          nil))))
                  (gui/bind-craft-buttons! {:queue queue} 1 resource-ui)
                  (.doClick (get-in ui [:craft-buttons :axe]) 0)))
         (is (= true (deref succeeded 5000 :timeout)))
         (is (= "Количество: 1" (.getText (get-in ui [:counts :axe]))))
         (is (= "Количество: 0" (.getText (get-in ui [:counts :wood]))))
         (edt! #(.doClick (get-in ui [:craft-buttons :pickaxe]) 0))
         (is (= true (deref rejected 5000 :timeout)))
         (is (every? false? @threads))
         (is (= {:wood 0 :stone 0 :axe 1 :pickaxe 0}
                (get-in (world/get-world) [:players 1 :inventory :items])))
         (finally (transport/stop! queue)))))))
