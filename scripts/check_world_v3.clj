(require '[settlement.server :as server]
         '[settlement.world :as world])
(import '[java.net InetAddress ServerSocket]
        '[java.util.concurrent TimeUnit])

(def commands
  {"smoke" ["-s" "prolog/smoke_test.pl" "--" "9001"]
   "bench" ["-s" "scripts/benchmark_world_v3.pl"]
   "match" ["-s" "prolog/settlement_bot.pl" "--" "9002" "8000" "1200"]})

(defn run-prolog [mode]
  (when-not (contains? commands mode)
    (throw (ex-info "Choose smoke, bench or match" {:mode mode})))
  (with-open [listener (ServerSocket. 0 16 (InetAddress/getLoopbackAddress))]
    (let [acceptor (future (server/serve! listener))
          process (ProcessBuilder. (into ["swipl.exe" "-q"] (commands mode)))]
      (try
        (.put (.environment process) "SETTLEMENT_PORT" (str (.getLocalPort listener)))
        (.inheritIO process)
        (let [started (System/nanoTime)
              child (.start process)]
          (if (.waitFor child (if (= mode "match") 1225 30) TimeUnit/SECONDS)
            (let [{:keys [match objects bandit-stats]} (world/get-world)
                  code (.exitValue child)]
              (println (cond-> {:elapsed-seconds (/ (- (System/nanoTime) started) 1e9) :match match}
                         (= mode "match")
                         (assoc :bandit-stats bandit-stats :red-buildings
                                (frequencies (for [{:keys [type team kind]} (vals objects)
                                                   :when (and (= type :building) (= team :red))]
                                               kind)))))
              (if (and (= mode "match") (not= :red (:winner match))) 1 code))
            (do (.destroyForcibly child) 124)))
        (finally (.close listener) (deref acceptor 3000 nil))))))

(try
  (System/exit (run-prolog (first *command-line-args*)))
  (finally (shutdown-agents)))
