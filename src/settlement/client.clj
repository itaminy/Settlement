(ns settlement.client
  (:require [settlement.protocol :as protocol])
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
                (InputStreamReader. (.getInputStream socket)))
        writer (BufferedWriter.
                (OutputStreamWriter. (.getOutputStream socket)))]

    {:socket socket
     :reader reader
     :writer writer}))

(defn close-connection [connection]
  (.close (:socket connection)))
  
(defn -main []
  (with-open [socket (Socket. "127.0.0.1" 5000)
              reader (BufferedReader.
                      (InputStreamReader. (.getInputStream socket)))
              writer (BufferedWriter.
                      (OutputStreamWriter. (.getOutputStream socket)))]

    (println "Connected to Settlement!")

    ;; Регистрация игрока
    (println "Registration:")
    (prn (send-message writer
                       reader
                       {:type :register
                        :player-id 1
                        :name "Kirill"}))

        ;; Проверка границы карты
    (println "Moving up:")
    (prn (send-message writer
                       reader
                       {:type :move
                        :player-id 1
                        :direction :up}))

        ;; Перемещение игрока
    (println "Moving right:")
    (prn (send-message writer
                       reader
                       {:type :move
                        :player-id 1
                        :direction :right}))

    ;; Подходим к дереву [3 4]: после предыдущего шага игрок в [1 0].
    (doseq [direction [:right :down :down :down :down]]
      (prn (send-message writer reader
                         {:type :move :player-id 1 :direction direction})))

    (println "Gathering nearby resource:")
    (prn (send-message writer reader {:type :gather :player-id 1}))

    ;; Получение состояния мира
    (println "World state:")
    (prn (send-message writer
                       reader
                       {:type :state}))))
