(ns settlement.gathering-ui
  (:require [settlement.client :as client]))

(defn now-ms [] (quot (System/nanoTime) 1000000))

(defn create-controller
  ([connection player-id progress refresh!]
   (create-controller connection player-id progress refresh! (fn [_ _])))
  ([connection player-id progress refresh! effect!]
  (let [state (atom nil) pressed (atom false)
        reset-ui! (fn [] (.setValue progress 0) (.setVisible progress false))
        current? (fn [attempt] (= attempt (:attempt-id @state)))
        release! (fn []
                   (let [old @state]
                     (reset! pressed false)
                     (reset! state nil)
                     (reset-ui!)
                     (effect! :cancel nil)
                     (when old
                       (client/async! connection [:cancel (:attempt-id old)]
                         {:type :gather-cancel :player-id player-id :attempt-id (:attempt-id old)
                          :operation-id (:operation-id old)}
                         (fn [_] (refresh!)) (constantly true) true))))
        respond! (fn [attempt response]
                   (when (current? attempt)
                     (cond
                       (:completed response) (do (effect! :gather-finish response)
                                                 (reset! state nil) (reset-ui!) (refresh!))
                       (or (= :ok (:status response)) (= :too-early (:code response)))
                       (do (when (and (= :ok (:status response)) (nil? (:operation-id @state)))
                             (effect! :gather-start response))
                           (swap! state merge (select-keys response [:operation-id :elapsed-ms :duration-ms])
                                  {:stamp (now-ms)}))
                       :else (do (effect! :cancel nil) (reset! state nil) (reset-ui!) (refresh!)))))
        press! (fn []
                 (when (compare-and-set! pressed false true)
                   (let [attempt (str (java.util.UUID/randomUUID))]
                     (reset! state {:attempt-id attempt :stamp (now-ms) :last-request (now-ms)})
                     (.setValue progress 0) (.setVisible progress true)
                     (when-not (client/async! connection [:start attempt]
                                 {:type :gather-start :player-id player-id :attempt-id attempt}
                                 #(respond! attempt %) #(current? attempt) false)
                       (release!)))))
        tick! (fn []
                (when-let [{:keys [attempt-id operation-id duration-ms elapsed-ms stamp last-request]} @state]
                  (if @(:closed (:queue connection))
                    (release!)
                    (when operation-id
                      (let [elapsed (+ elapsed-ms (- (now-ms) stamp))
                            finish? (>= elapsed duration-ms)]
                        (.setValue progress (int (min 99 (* 100 (/ elapsed duration-ms)))))
                        (when (>= (- (now-ms) last-request) 250)
                          (swap! state assoc :last-request (now-ms))
                          (client/async! connection [(if finish? :finish :heartbeat) attempt-id]
                            {:type (if finish? :gather-finish :gather-heartbeat)
                             :player-id player-id :operation-id operation-id}
                            #(respond! attempt-id %) #(current? attempt-id) false)))))))]
    {:state state :pressed pressed :press! press! :release! release! :tick! tick!})))
