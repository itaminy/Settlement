(ns settlement.client
  (:require [settlement.protocol :as protocol]
            [settlement.credentials :as credentials])
  (:import [java.net Socket]
           [java.io BufferedReader
            InputStreamReader
            BufferedWriter
            OutputStreamWriter]))

(defn send-message [writer reader message]
  (.write writer (str (protocol/write-message message) "\n"))
  (.flush writer)
  (protocol/read-message (.readLine reader)))

(defn connect []
  (let [socket (Socket. "127.0.0.1" 5000)
        reader (BufferedReader.
                (InputStreamReader. (.getInputStream socket) "UTF-8"))
        writer (BufferedWriter.
                (OutputStreamWriter. (.getOutputStream socket) "UTF-8"))]

    {:socket socket
     :reader reader
     :writer writer}))

(defn close-connection [connection]
  (.close (:socket connection)))
  
(defn request [connection message]
  (send-message (:writer connection) (:reader connection) message))

(defn authorize!
  "Callbacks receive no secrets. A failed save can be retried on the live connection."
  [connection player-id player-name confirm-new? retry-save?]
  (try
    (let [server "127.0.0.1:5000"
          hello (request connection {:type :hello})
          instance (:server-instance hello)
          entry (credentials/load-entry server player-id)
          stale? (and entry (not= instance (:server-instance entry)))]
      (cond
        (not= :ok (:status hello)) {:status :error :code :connection-failed}
        (and stale? (not (confirm-new?))) {:status :error :code :cancelled}
        (and entry (not stale?))
        (request connection {:type :login :player-id player-id
                             :token (:token entry) :server-instance instance})
        :else
        (let [registration (request connection {:type :register :player-id player-id :name player-name})]
          (if (= :ok (:status registration))
            (loop []
              (let [saved (credentials/save-entry! server player-id instance (:token registration))]
                (cond
                  (= :ok (:status saved)) {:status :ok}
                  (retry-save?) (recur)
                  :else {:status :error :code :credentials-save-failed})))
            ;; Never return a token to UI or error handlers.
            (select-keys registration [:status :code])))))
    (catch Exception _ {:status :error :code :credentials-unavailable})))

(defn -main [& [id name]]
  (let [connection (connect)
        player-id (if id (Long/parseLong id) 1)]
    (try
      (let [result (authorize! connection player-id (or name "Kirill")
                               #(do (println "Server restarted. Create a new character? Type yes:")
                                    (= "yes" (read-line)))
                               #(do (println "Cannot save credentials. Retry? Type yes:")
                                    (= "yes" (read-line))))]
        (if (= :ok (:status result))
          (println "Authorized. Use the GUI to play.")
          (println "Authorization failed:" (:code result))))
      (finally (close-connection connection)))))
