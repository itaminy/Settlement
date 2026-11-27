(require '[clojure.java.io :as io]
         '[settlement.buildings :as buildings]
         '[settlement.client :as client]
         '[settlement.server :as server]
         '[settlement.world :as world])
(import '[java.net InetAddress ServerSocket Socket]
        '[java.util.concurrent TimeUnit])

(def blue 9101)
(def red 9102)
(def vulnerable [80 25])
(def red-house [72 23])

(defn connect [port]
  (let [socket (doto (Socket. "127.0.0.1" port) (.setSoTimeout 5000))]
    {:socket socket
     :reader (io/reader (.getInputStream socket) :encoding "UTF-8")
     :writer (io/writer (.getOutputStream socket) :encoding "UTF-8")}))

(defn command! [connection message]
  (let [reply (client/request connection message)]
    (when-not (= :ok (:status reply))
      (throw (ex-info "TCP command rejected" {:type (:type message) :code (:code reply)})))
    reply))

(defn setup-fixture! []
  ;; Seed this late-game position once. Both players then act over TCP.
  (let [structure (fn [team id kind]
                    (buildings/initialize-durability
                      {:type :building :team team :owner-id id :kind kind}))
        house (structure :blue blue :house)
        buildings (merge (zipmap [[65 18] [68 18] [71 18] [74 18]]
                                 (repeatedly 4 #(assoc house :building-id (random-uuid))))
                         {[81 19] (structure :blue blue :mine)
                          vulnerable (assoc (structure :blue blue :watchtower) :hp 10)
                          red-house (structure :red red :house)})]
    (dosync
      (alter world/world
             (fn [state]
               (-> state
                   (update :objects
                           #(into (into {} (remove (fn [[[x y] _]]
                                                     (and (<= 64 x 85) (<= 17 y 32)))) %)
                                  buildings))
                   (assoc-in [:players blue :x] 73)
                   (assoc-in [:players blue :y] 25)
                   (assoc-in [:players red :x] 76)
                   (assoc-in [:players red :y] 25)
                   (assoc-in [:match :scores] {:blue 90 :red 15})))))
    (world/change-player-items! red {:wood 2})
    (world/change-player-items! blue {:wood 20 :stone 10})))

(defn start-bot [port credentials]
  (let [command (str "getenv('SCENARIO_INSTANCE',I),getenv('SCENARIO_TOKEN',T),"
                     "atom_string(I,IS),atom_string(T,TS),"
                     "run_bot(9102,login(credentials(9102,IS,TS)),limits(250,60),fail,_,R),"
                     "writeln(R),(R=stopped(defeat,_)->halt;halt(1))")
        child (ProcessBuilder. ["swipl.exe" "-q" "-s" "prolog/settlement_bot.pl" "-g" command])]
    (doto (.environment child)
      (.put "SETTLEMENT_PORT" (str port))
      (.put "SCENARIO_INSTANCE" (:server-instance credentials))
      (.put "SCENARIO_TOKEN" (:token credentials)))
    (.start (.inheritIO child))))

(defn poll [label pred]
  (let [deadline (+ (System/nanoTime) 25000000000)]
    (loop []
      (if-let [value (pred)]
        value
        (if (< (System/nanoTime) deadline)
          (do (Thread/sleep 50) (recur))
          (throw (ex-info "Scenario timeout" {:waiting-for label})))))))

(defn victory-build! [connection]
  (let [position [71 26]]
    (command! connection {:type :build :player-id blue :building-type :sawmill :position position})
    position))

(defn scenario []
  (with-open [listener (ServerSocket. 0 16 (InetAddress/getLoopbackAddress))]
    (let [acceptor (future (server/serve! listener))
          port (.getLocalPort listener)
          blue-session (connect port)]
      (try
        (command! blue-session {:type :register :player-id blue :name "Blue" :client-kind :human})
        (let [red-session (connect port)
              credentials (try (command! red-session {:type :register :player-id red :name "Red"
                                                       :client-kind :prolog})
                               (finally (client/close-connection red-session)))]
          (setup-fixture!)
          (let [target (get-in @world/world [:objects red-house :building-id])
                started (System/nanoTime)]
            (command! blue-session {:type :attack-building :player-id blue :position red-house
                                    :building-id target :attack-seq 1})
            (assert (= 190 (get-in @world/world [:objects red-house :hp])))
            (let [child (start-bot port credentials)]
              (try
                (poll :red-counterattack #(nil? (get-in @world/world [:objects vulnerable])))
                (assert (= 80 (get-in @world/world [:match :scores :blue])))
                (poll :red-repair #(= 200 (get-in @world/world [:objects red-house :hp])))
                (poll :red-resumes-development #(seq @world/gathering))
                (let [anchor (victory-build! blue-session)]
                  (assert (.waitFor child 15 TimeUnit/SECONDS))
                  (assert (zero? (.exitValue child)))
                  (assert (= :blue (get-in @world/world [:match :winner])))
                  (assert (= {:attack-seq 1 :repair-seq 1}
                             (world/building-command-sequences red)))
                  (println {:blue-victory-build anchor :match (:match @world/world)
                            :seconds (/ (- (System/nanoTime) started) 1e9)
                            :red-building-attacks 1 :red-repairs 1 :red-resumed :gather})
                  (let [id (world/match-id @world/world)]
                    (command! blue-session {:type :restart-match :player-id blue :match-id id})
                    (assert (= (inc id) (world/match-id @world/world)))
                    (assert (= {:blue 0 :red 0} (get-in @world/world [:match :scores])))
                    (assert (= 4 (count (:bandits @world/world))))
                    (let [next-bot (start-bot port credentials)]
                      (try
                        (poll :bot-resumes #(seq @world/gathering))
                        (println {:restart :passed :red-resumed :gather})
                        (finally (.destroyForcibly next-bot))))))
                (finally (when (.isAlive child) (.destroyForcibly child)))))))
        (finally
          (client/close-connection blue-session)
          (.close listener)
          (deref acceptor 3000 nil))))))

(try (scenario) (finally (shutdown-agents)))
