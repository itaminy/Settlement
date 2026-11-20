(ns settlement.events-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is use-fixtures]]
            [settlement.auth :as auth]
            [settlement.buildings :as buildings]
            [settlement.event-log :as log]
            [settlement.event-stream :as stream]
            [settlement.layout :as layout]
            [settlement.protocol :as protocol]
            [settlement.server :as server]
            [settlement.test-support :as support]
            [settlement.world :as world])
  (:import [java.io BufferedReader BufferedWriter]
           [java.net ServerSocket Socket SocketTimeoutException]))

(use-fixtures :each
  (fn [test]
    (with-redefs [auth/state (ref (auth/new-state))
                  log/journal (log/new-log)
                  layout/spawn-cells support/row-major-spawn
                  buildings/protected-cells #{[0 0]}
                  world/world (ref {:players {} :objects {[1 0] :tree}
                                    :resources {:wood 0 :stone 0} :match world/initial-match})
                  world/gathering (ref {})
                  world/now-ns #(deref support/time-ns)
                  world/schedule-resource-restore (fn [_ _] nil)]
      (reset! support/time-ns 0)
      (test))))

(defn- open-client [^ServerSocket listener]
  (let [socket (doto (Socket. "127.0.0.1" (.getLocalPort listener)) (.setSoTimeout 5000))
        peer (.accept listener)]
    {:socket socket :task (future (server/handle-client peer))
     :in (io/reader (.getInputStream socket) :encoding "UTF-8")
     :out (io/writer (.getOutputStream socket) :encoding "UTF-8")}))

(defn- close-client [{:keys [^Socket socket task]}]
  (.close socket)
  (deref task 5000 nil)
  (future-cancel task))

(defn- read-line! [{:keys [^BufferedReader in]}]
  (.readLine in))

(defn- send-line! [{:keys [^BufferedWriter out]} line]
  (.write out (str line "\n"))
  (.flush out))

(defn- text! [client line]
  (send-line! client line)
  (read-line! client))

(defn- edn! [client command]
  (protocol/read-message (text! client (protocol/write-message command))))

(defn- register! [client id]
  (edn! client {:type :register :player-id id :name (str "Игрок " id)}))

(defn- silent? [{:keys [^Socket socket] :as client}]
  (.setSoTimeout socket 150)
  (try (read-line! client) false
       (catch SocketTimeoutException _ true)
       (finally (.setSoTimeout socket 5000))))

(deftest mixed-subscribers-and-snapshot-gap-test
  (with-open [listener (ServerSocket. 0)]
    (let [edn (open-client listener)
          text (open-client listener)
          legacy (open-client listener)]
      (try
        (is (= {:status :error :code :not-authenticated} (edn! edn {:type :subscribe-events})))
        (is (= {:status :error :code :not-authenticated} (edn! edn {:type :unsubscribe-events})))
        (let [registration (register! edn 1)]
          (is (= :ok (:status (register! text 2))))
          (is (= :ok (:status (register! legacy 3))))
          (is (= {:status :ok :mode :text :version 1}
                 (protocol/read-message (text! text "текст 1"))))
          (let [snap (edn! edn {:type :state :include-world-version true})]
            (is (= 3 (:world-version snap)))
            (is (= [1 2 3] (:online-players snap)))
            (is (re-find #"^reply v=1 ok world-versioned version=3 .* online=\[1,2,3\] gathering=\[\]$"
                         (text! text "покажи мир с версией")))
            (is (= :ok (:status (edn! legacy {:type :move :player-id 3 :direction :down}))))
            (is (= {:status :ok :since 3 :world-version 4} (edn! edn {:type :subscribe-events})))
            (is (= "reply v=1 ok subscribe-events since=3 version=4" (text! text "подпишись на события")))
            (let [line (read-line! edn)]
              (is (= {:type :event :version 4 :seq 4 :changes [:player-moved] :resync true}
                     (protocol/read-message line)))
              (is (not (.contains line (:token registration))))
              (is (not (.contains line (:server-instance registration)))))
            (is (= "event v=1 version=4 seq=4 changes=[player-moved] resync=true" (read-line! text)))
            (is (= :ok (:status (edn! legacy {:type :move :player-id 3 :direction :up}))))
            (is (= 5 (:seq (protocol/read-message (read-line! edn)))))
            (is (= "event v=1 version=5 seq=5 changes=[player-moved] resync=true" (read-line! text)))
            (is (= :out-of-bounds (:code (edn! legacy {:type :move :player-id 3 :direction :up}))))
            (is (= :ok (:status (edn! legacy {:type :move :player-id 3 :direction :down}))))
            (is (= 6 (:seq (protocol/read-message (read-line! edn)))))
            (is (= "event v=1 version=6 seq=6 changes=[player-moved] resync=true" (read-line! text)))
            (is (silent? legacy))
            (close-client legacy)
            (is (= [:player-disconnected] (:changes (protocol/read-message (read-line! edn)))))
            (is (= "event v=1 version=7 seq=7 changes=[player-disconnected] resync=true" (read-line! text)))
            (is (= "reply v=1 ok unsubscribe-events" (text! text "отпишись от событий")))
            (is (= {:status :ok} (edn! edn {:type :unsubscribe-events})))
            (is (= :ok (:status (world/move-player-result 2 :down))))
            (is (silent? text))
            (is (silent? edn))))
        (finally (doseq [client [edn text legacy]] (close-client client)))))))

(defn- register-local! [id]
  (server/process-message id {:type :register :player-id id :name (str id)}))

(deftest concurrent-changes-test
  (register-local! 1)
  (register-local! 2)
  (let [before (:version @log/journal)
        start (promise)
        tasks (mapv (fn [id]
                      (future @start
                              (dotimes [_ 10] (world/move-player-result id :down))))
                    [1 2])]
    (deliver start true)
    (doseq [task tasks] (is (not= ::timeout (deref task 5000 ::timeout))))
    (let [events (log/after @log/journal before)]
      (is (= (range (inc before) (+ before 21)) (map :seq events)))
      (is (every? #(= [:player-moved] (:changes %)) events))
      (is (= [10 10] (mapv #(get-in @world/world [:players % :y]) [1 2]))))))

(deftest gathering-and-resource-events-test
  (register-local! 1)
  (let [start (world/start-gather! 1 1 "attempt")
        operation (:operation-id start)
        version (:version @log/journal)]
    (is (= [:gather-started] (-> @log/journal :events peek :changes)))
    (is (= start (world/start-gather! 1 1 "attempt")))
    (is (= :too-early (:code (world/update-gather! 1 1 operation true))))
    (dotimes [_ 3]
      (support/advance! 1000)
      (world/update-gather! 1 1 operation false))
    (is (= version (:version @log/journal)))
    (is (:completed (world/update-gather! 1 1 operation true)))
    (is (= :restored (world/restore-resource [1 0] :tree)))
    (is (= :exists (world/restore-resource [1 0] :tree)))
    (is (= [[:gather-finished :resource-removed] [:resource-restored]]
           (map :changes (log/after @log/journal version))))))

(deftest building-and-match-events-test
  (register-local! 1)
  (register-local! 2)
  (let [target [11 10]
        building-id (random-uuid)]
    (dosync
      (alter world/world assoc-in [:players 1 :x] 10)
      (alter world/world assoc-in [:players 1 :y] 10)
      (alter world/world assoc-in [:players 2 :x] 12)
      (alter world/world assoc-in [:players 2 :y] 10)
      (alter world/world assoc-in [:objects target]
             {:kind :fence :type :building :team :red :owner-id 2
              :building-id building-id :hp 10 :max-hp 50})
      (alter world/world assoc-in [:match :scores :red] 2))
    (world/change-player-items! 2 {:wood 1})
    (let [version (:version @log/journal)]
      (is (= :ok (:status (world/repair-building! 2 target building-id 1))))
      (is (= :ok (:status (world/attack-building! 1 target building-id 1))))
      (is (= :ok (:status (world/attack-building! 1 target building-id 1))))
      (is (= :attack-cooldown (:code (world/attack-building! 1 target building-id 2))))
      (support/advance! 1000)
      (is (:destroyed? (world/attack-building! 1 target building-id 3)))
      (is (= [[:building-repaired] [:building-attacked]
              [:building-attacked :building-destroyed :score-changed]]
             (map :changes (log/after @log/journal version)))))
    (dosync
      (alter world/world update :objects into
             (for [index (range 49)]
               [[(+ 1 (mod index 25)) (+ 20 (* 3 (quot index 25)))]
                {:type :building :kind :fence :team :blue}]))
      (alter world/world assoc-in [:match :scores :blue] 98))
    (world/change-player-items! 1 {:wood 2})
    (let [result (world/build! 1 :fence target)]
      (is (= :ok (:status result)) (pr-str result)))
    (is (= [:building-created :score-changed :match-finished]
           (-> @log/journal :events peek :changes)))
    (is (= :blue (get-in @world/world [:match :winner])))))

(deftest slow-subscriber-is-disconnected-test
  (with-open [socket (Socket.)]
    (let [outgoing (stream/open-stream socket)
          task (future (stream/pump! outgoing (fn [_ event] (str (:seq event)))))]
      (try
        (reset! (:subscription outgoing) {:cursor 0 :mode :text})
        (dotimes [_ (+ stream/queue-capacity 2)] (dosync (log/append! [:player-moved])))
        (is (not= ::timeout (deref task 5000 ::timeout)))
        (is (.isClosed socket))
        (is (= stream/queue-capacity (.size (:queue outgoing))))
        (finally (future-cancel task))))))

(deftest stale-cursor-requires-snapshot-test
  (with-open [listener (ServerSocket. 0)]
    (let [client (open-client listener)]
      (try
        (register! client 1)
        (dotimes [_ log/history-limit] (dosync (log/append! [:player-moved])))
        (is (= {:status :error :code :resync-required} (edn! client {:type :subscribe-events :since 0})))
        (let [snapshot (edn! client {:type :state :include-world-version true})]
          (is (= (:world-version snapshot) (:since (edn! client {:type :subscribe-events})))))
        (finally (close-client client))))))
