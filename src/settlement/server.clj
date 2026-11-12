(ns settlement.server
  (:require [settlement.world :as world]
            [settlement.auth :as auth]
            [settlement.protocol :as protocol])
  (:import [java.net ServerSocket Socket]
           [java.io BufferedReader
            InputStreamReader
            BufferedWriter
            OutputStreamWriter]))

(defn process-message
  ([message] (process-message nil message))
  ([connection {:keys [type player-id name token server-instance direction operation-id attempt-id tool building-type position]}]
   (case type
     :hello {:status :ok :server-instance (:instance @auth/state)}
     :register (if connection (auth/register! connection player-id name)
                   (auth/failure :not-authenticated))
     :login (if connection (auth/login! connection player-id token server-instance)
                (auth/failure :not-authenticated))
     :collect (auth/failure :collect-disabled)
     :state {:status :ok :world (world/get-world)}
     :gather (auth/failure :gather-disabled)
     (:move :gather-start :gather-heartbeat :gather-cancel :gather-finish :craft :build :build-check)
     (let [id (when connection (auth/owner connection))]
       (cond
         (nil? id) (auth/failure :not-authenticated)
         (not= id player-id) (auth/failure :wrong-player)
         (= type :gather-start) (dosync (ensure auth/state)
                                   (if (= id (auth/owner connection))
                                     (world/start-gather! id connection attempt-id)
                                     (auth/failure :not-authenticated)))
         (= type :gather-heartbeat) (world/update-gather! id connection operation-id false)
         (= type :gather-finish) (world/update-gather! id connection operation-id true)
         (= type :gather-cancel) (world/cancel-gather! id connection operation-id attempt-id)
         (#{:build :build-check} type)
         (dosync (ensure auth/state)
           (if (= id (auth/owner connection))
             (if (= type :build)
               (assoc (world/build! id building-type position) :world (world/get-world))
               (world/check-build id building-type position))
             (auth/failure :not-authenticated)))
         (= type :craft) (dosync (ensure auth/state)
                            (if (= id (auth/owner connection))
                              (assoc (world/craft! id tool) :world (world/get-world))
                              (auth/failure :not-authenticated)))
         :else (let [result (world/move-player-result id direction)]
                 (if (= :ok (:status result))
                   (assoc result :world (world/get-world))
                   result))))
     (auth/failure :unknown-command))))

(defn handle-client [^Socket socket]
  (let [connection (Object.)]
    (try
      (with-open [socket socket
                  reader (BufferedReader. (InputStreamReader. (.getInputStream socket) "UTF-8"))
                  writer (BufferedWriter. (OutputStreamWriter. (.getOutputStream socket) "UTF-8"))]
        (loop []
          (when-let [line (.readLine reader)]
            (let [response (try
                             (process-message connection (protocol/read-message line))
                             (catch Exception _ (auth/failure :invalid-message)))]
              (.write writer (str (protocol/write-message response) "\n"))
              (.flush writer))
            (recur))))
      (catch java.io.IOException _ nil)
      (finally (auth/disconnect! connection)))))

(defn start-server [port]
  (let [server-socket (ServerSocket. (int port))]
    (println (str "Server started on port " port))

    (while true
      (let [client (.accept server-socket)]
        (future
          (handle-client client))))))
