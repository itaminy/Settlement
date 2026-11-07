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

    ;; Добыча дерева
    (println "Collecting wood:")
    (prn (send-message writer
                       reader
                       {:type :collect
                        :player-id 1
                        :resource :wood
                        :amount 5}))

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

    ;; Получение состояния мира
    (println "World state:")
    (prn (send-message writer
                       reader
                       {:type :state}))))