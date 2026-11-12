(ns settlement.test-support
  (:require [settlement.world :as world]
            [settlement.server :as server]
            [settlement.client :as client]
            [settlement.config :as config]))

(defn row-major-spawn []
  (for [y (range config/world-height) x (range config/world-width)] [x y]))

(def time-ns (atom 0))
(defn advance! [ms] (swap! time-ns + (* ms 1000000)))

(defn complete! [request id]
  (let [start (request {:type :gather-start :player-id id :attempt-id (str (java.util.UUID/randomUUID))})]
    (if-not (= :ok (:status start)) start
      (do
        (dotimes [_ (int (Math/ceil (/ (:duration-ms start) 250.0)))]
          (advance! 250)
          (request {:type :gather-heartbeat :player-id id :operation-id (:operation-id start)}))
        (dissoc (request {:type :gather-finish :player-id id :operation-id (:operation-id start)}) :completed)))))

(defn process-message
  ([message] (process-message nil message))
  ([connection message]
   (if (= :gather (:type message))
     (complete! #(server/process-message connection %) (:player-id message))
     (server/process-message connection message))))

(defn request [connection message]
  (if (= :gather (:type message))
    (complete! #(client/request connection %) (:player-id message))
    (client/request connection message)))

(defn gather! [id]
  (complete!
   (fn [message]
     (case (:type message)
       :gather-start (world/start-gather! id :test (:attempt-id message))
       :gather-heartbeat (world/update-gather! id :test (:operation-id message) false)
       :gather-finish (world/update-gather! id :test (:operation-id message) true))) id))

(defn immediate-async [connection _ message callback & _]
  (callback (process-message (or (:writer connection) :a) message))
  true)
