(ns settlement.server
  (:require [settlement.world :as world]
            [settlement.config :as config]
            [settlement.auth :as auth]
            [settlement.event-log :as events]
            [settlement.event-stream :as stream]
            [settlement.framing :as framing]
            [settlement.inventory :as inventory]
            [settlement.protocol :as protocol]
            [settlement.text-commands :as text-commands]
            [settlement.text-protocol :as text-protocol])
  (:import [java.net ServerSocket Socket SocketException]
           [java.util.concurrent Executors ThreadFactory TimeUnit]
           [java.io BufferedInputStream
            Closeable
            BufferedWriter
            OutputStreamWriter]))

(defn- process-command
  [connection {:keys [type player-id name token server-instance direction operation-id attempt-id
                       tool building-type position building-id bandit-id attack-seq repair-seq
                       include-building-sequences include-world-version] :as message}]
   (case type
     :hello {:status :ok :server-instance (:instance @auth/state)}
     :register (if connection (auth/register! connection player-id name (:client-kind message))
                   (auth/failure :not-authenticated))
     :login (if connection (auth/login! connection player-id token server-instance)
                (auth/failure :not-authenticated))
     :restart-match (dosync
                      (ensure auth/state)
                      (if-let [id (when connection (auth/owner connection))]
                        (let [result (world/restart-match! id (:match-id message))]
                          (cond-> result
                            (= :ok (:status result)) (assoc :world (world/get-world))))
                        (auth/failure :not-authenticated)))
     :collect (auth/failure :collect-disabled)
     :state (if (and connection (or (true? include-building-sequences) (true? include-world-version)))
              (dosync
                (let [response {:status :ok :world (assoc (world/get-world) :world-seed config/world-seed)}]
                  (if-let [id (auth/owner connection)]
                    (cond-> response
                      (true? include-building-sequences)
                      (assoc :building-sequences (world/building-command-sequences id))
                      (true? include-world-version)
                      (assoc :world-version (:version @events/journal)
                             :online-players (vec (sort (keys (:sessions @auth/state))))
                             :gathering-players (vec (sort (keys @world/gathering)))))
                    (if (true? include-world-version)
                      (auth/failure :not-authenticated)
                      response))))
              {:status :ok :world (world/get-world)})
     :gather (auth/failure :gather-disabled)
     :inventory (dosync
                  (ensure auth/state)
                  (if-let [id (when connection (auth/owner connection))]
                    (if (or (nil? player-id) (= id player-id))
                      (if-let [player (get-in @world/world [:players id])]
                        {:status :ok :inventory (inventory/player-inventory player)}
                        (auth/failure :player-not-found))
                      (auth/failure :wrong-player))
                    (auth/failure :not-authenticated)))
     (:move :gather-start :gather-heartbeat :gather-cancel :gather-finish :craft :build :build-check
      :attack-building :attack-bandit :repair-building)
     (let [id (when connection (auth/owner connection))]
       (cond
         (nil? id) (auth/failure :not-authenticated)
         (not= id player-id) (auth/failure :wrong-player)
         (= type :gather-start) (dosync (ensure auth/state)
                                   (if (= id (auth/owner connection))
                                     (world/start-gather! id connection attempt-id
                                                          (select-keys message [:resource-type :position]))
                                     (auth/failure :not-authenticated)))
         (= type :gather-heartbeat) (world/update-gather! id connection operation-id false)
         (= type :gather-finish) (world/update-gather! id connection operation-id true)
         (= type :gather-cancel) (world/cancel-gather! id connection operation-id attempt-id)
         (#{:attack-building :attack-bandit :repair-building} type)
         (dosync (ensure auth/state)
           (if (= id (auth/owner connection))
             (assoc (case type
                      :attack-building (world/attack-building! id position building-id attack-seq)
                      :attack-bandit (world/attack-bandit! id position bandit-id attack-seq)
                      :repair-building (world/repair-building! id position building-id repair-seq))
                    :world (world/get-world))
             (auth/failure :not-authenticated)))
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
         :else (dosync (ensure auth/state)
                 (if (= id (auth/owner connection))
                   (let [result (world/move-player-result id direction)]
                     (if (= :ok (:status result))
                       (assoc result :world (world/get-world))
                       result))
                   (auth/failure :not-authenticated)))))
     (auth/failure :unknown-command)))

(def ^:private match-guarded-commands
  #{:move :gather-start :gather-heartbeat :gather-cancel :craft :build :build-check
    :attack-building :attack-bandit :repair-building})

(def ^:private paced-commands
  #{:move :gather-start :craft :build :attack-building :attack-bandit :repair-building})

(defn- process-paced-command [connection {:keys [type attack-seq repair-seq] :as message}]
  (if (paced-commands type)
    (dosync
      (ensure auth/state)
      (let [id (when connection (auth/owner connection))
            state @world/world
            sequence (case type (:attack-building :attack-bandit) attack-seq :repair-building repair-seq nil)
            previous (case type
                       (:attack-building :attack-bandit) (get-in state [:building-attacks id :seq] 0)
                       :repair-building (get-in state [:building-repairs id :seq] 0)
                       0)
            bot? (and (= id (:player-id message)) (contains? (:bot-players state) id))
            now (when bot? (world/now-ns))
            replay? (or (and (integer? sequence) (<= sequence previous))
                        (and bot? (= :gather-start type)
                             (world/active-gather-retry? id connection (:attempt-id message) now)))]
        (if (and bot? (not replay?) (not (world/match-finished? state))
                 (not (world/bot-cooled-down? state id type now)))
          (auth/failure :bot-cooldown)
          (let [result (process-command connection message)]
            (when (and bot? (not replay?) (= :ok (:status result)))
              (world/record-bot-command! id type (world/now-ns)))
            result))))
    (process-command connection message)))

(defn process-message
  ([message] (process-message nil message))
  ([connection {:keys [type match-id] :as message}]
   ;; Versioned GUI commands queued during the previous round cannot mutate the new one.
   ;; Gather finish already requires the old operation UUID, which is cleared on restart.
   (if (and (contains? message :match-id) (match-guarded-commands type))
     (dosync
       (if (= match-id (world/match-id (ensure world/world)))
         (process-paced-command connection message)
         (auth/failure :stale-match)))
     (process-paced-command connection message))))

(defn- dispatch-request [connection mode {:keys [subscription snapshot]} request]
  (case (:type request)
    (:subscribe-events :unsubscribe-events)
    (cond
      (nil? (auth/owner connection)) (auth/failure :not-authenticated)
      (= :unsubscribe-events (:type request))
      (do (reset! subscription nil) {:status :ok})
      @subscription (auth/failure :already-subscribed)
      :else
      (let [{:keys [version] :as journal} @events/journal
            cursor (get request :since (or @snapshot version))]
        (cond
          (not (and (integer? cursor) (not (neg? cursor)))) (auth/failure :invalid-version)
          (nil? (events/after journal cursor)) (auth/failure :resync-required)
          :else (do (reset! subscription {:cursor cursor :mode mode})
                    {:status :ok :since cursor :world-version version}))))
    (let [response (process-message connection request)]
      (when-let [version (:world-version response)]
        (reset! snapshot version))
      response)))

(defn- process-line [connection mode event-stream line]
  (try
    (if (= :text mode)
      (if-let [id (auth/owner connection)]
        (let [command (text-commands/parse-command line)]
          (if (= :error (:status command))
            [command command]
            (let [request (cond-> (assoc command :player-id id)
                            (= :state (:type command)) (assoc :include-building-sequences true))]
              [request (dispatch-request connection mode event-stream request)])))
        [nil (auth/failure :not-authenticated)])
      (if-let [[_ version] (re-matches #"текст\h+(\S+)" line)]
        [nil (cond
               (nil? (auth/owner connection)) (auth/failure :not-authenticated)
               @(:subscription event-stream) (auth/failure :already-subscribed)
               (= "1" version) {:status :ok :mode :text :version 1}
               :else (auth/failure :unsupported-version))]
        [nil (dispatch-request connection mode event-stream (protocol/read-message line))]))
    (catch Exception _ [nil (auth/failure :invalid-message)])))

(defn- encode-event [mode event]
  (if (= :text mode) (text-protocol/write-event event) (protocol/write-message event)))

(defn- write-line! [^BufferedWriter writer line]
  (.write writer (str line "\n"))
  (.flush writer))

(defn handle-client [^Socket socket]
  (let [connection (Object.)]
    (try
      (with-open [socket socket
                  reader (BufferedInputStream. (.getInputStream socket))
                  writer (BufferedWriter. (OutputStreamWriter. (.getOutputStream socket) "UTF-8"))]
        (let [event-stream (stream/open-stream socket)
              writer-task (future (stream/write-loop! event-stream #(write-line! writer %)))
              event-task (future (stream/pump! event-stream encode-event))]
          (try
            (loop [mode :edn]
              (when-let [line (framing/read-line! reader)]
                (let [[reply written]
                      (locking event-stream
                        (let [[request reply] (process-line connection mode event-stream line)
                              encoded (if (= :text mode)
                                        (text-protocol/write-reply request reply)
                                        (protocol/write-message reply))
                              written (promise)]
                          (stream/send! event-stream [encoded written])
                          [reply written]))]
                  (when (stream/await-write! event-stream written)
                    (recur (or (:mode reply) mode))))))
            (finally
              (.close socket)
              (future-cancel event-task)
              (future-cancel writer-task)))))
      (catch java.io.IOException _ nil)
      (finally (auth/disconnect! connection)))))

(defn start-bandits! []
  (world/initialize-bandits!)
  (let [executor (Executors/newSingleThreadScheduledExecutor
                  (reify ThreadFactory
                    (newThread [_ task]
                      (doto (Thread. task "settlement-bandits") (.setDaemon true)))))]
    (.scheduleWithFixedDelay executor
      ^Runnable (bound-fn []
                   (try (world/tick-bandits! (world/combat-time))
                        (catch Exception failure
                          (binding [*out* *err*]
                            (println "Bandit tick failed:" (.getMessage failure))))))
      0 50 TimeUnit/MILLISECONDS)
    (reify Closeable
      (close [_]
        (.shutdownNow executor)
        (.awaitTermination executor 5 TimeUnit/SECONDS)))))

(defn serve! [^ServerSocket listener]
  (with-open [scheduler (start-bandits!)]
    (try
      (while (not (.isClosed listener))
        (let [client (.accept listener)]
          (future (handle-client client))))
      (catch SocketException error
        (when-not (.isClosed listener) (throw error))))))

(defn start-server [port]
  (with-open [listener (ServerSocket. (int port))]
    (println (str "Server started on port " (.getLocalPort listener)))
    (serve! listener)))
