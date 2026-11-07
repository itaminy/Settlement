(ns settlement.server
  (:require [settlement.world :as world]
            [settlement.protocol :as protocol])
  (:import [java.net ServerSocket Socket]
           [java.io BufferedReader
            InputStreamReader
            BufferedWriter
            OutputStreamWriter]))

(defn process-message [message]
  (let [{:keys [type player-id name resource amount]} message]
    (case type

      :register
      (do
        (world/add-player player-id name)
        {:status :ok
         :message "Player registered"})

      :collect
      (if (world/collect-resource player-id resource amount)
        {:status :ok
         :world (world/get-world)}
        {:status :error
         :message "Player not found"})

      :state
      {:status :ok
       :world (world/get-world)}

      {:status :error
       :message "Unknown command"})))

(defn handle-client [^Socket socket]
  (with-open [socket socket
              reader (BufferedReader.
                      (InputStreamReader. (.getInputStream socket)))
              writer (BufferedWriter.
                      (OutputStreamWriter. (.getOutputStream socket)))]

    (println "Client connected:" (.getInetAddress socket))

    (loop []
      (when-let [line (.readLine reader)]
        (try
          (let [message (protocol/read-message line)
                response (process-message message)]

            (.write writer (str (protocol/write-message response) "\n"))
            (.flush writer))

          (catch Exception e
            (.write writer
                    (str (protocol/write-message
                          {:status :error
                           :message "Invalid message"}) "\n"))
            (.flush writer)))

        (recur)))))

(defn start-server [port]
  (let [server-socket (ServerSocket. (int port))]
    (println (str "Server started on port " port))

    (while true
      (let [client (.accept server-socket)]
        (future
          (handle-client client))))))