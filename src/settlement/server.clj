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
  ([connection {:keys [type player-id name token server-instance direction]}]
   (case type
     :hello {:status :ok :server-instance (:instance @auth/state)}
     :register (if connection (auth/register! connection player-id name)
                   (auth/failure :not-authenticated))
     :login (if connection (auth/login! connection player-id token server-instance)
                (auth/failure :not-authenticated))
     :collect (auth/failure :collect-disabled)
     :state {:status :ok :world (world/get-world)}
     (:move :gather)
     (let [id (when connection (auth/owner connection))]
       (cond
         (nil? id) (auth/failure :not-authenticated)
         (not= id player-id) (auth/failure :wrong-player)
         (= type :gather) (world/gather-resource id)
         :else (if (world/move-player id direction)
                 {:status :ok :world (world/get-world)}
                 (auth/failure :invalid-movement))))
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
