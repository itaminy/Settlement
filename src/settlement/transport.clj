(ns settlement.transport
  (:import [java.util.concurrent LinkedBlockingDeque]))

(def queue-capacity 16)

(defn stop! [queue]
  (locking (:gate queue)
    (reset! (:closed queue) true)
    (.clear ^LinkedBlockingDeque (:jobs queue))
    (reset! (:pending queue) #{}))
  (when-let [worker @(:worker queue)] (.interrupt ^Thread worker)))

(defn create-queue
  ([request] (create-queue request (fn [] nil)))
  ([request on-failure]
   (let [queue {:jobs (LinkedBlockingDeque. queue-capacity) :pending (atom #{})
                :closed (atom false) :worker (atom nil) :gate (Object.)}
         worker (Thread.
                 (fn []
                   (try
                     (while (not @(:closed queue))
                       (let [{:keys [key message current? callback]} (.take ^LinkedBlockingDeque (:jobs queue))
                             result (if (current?)
                                      (try (request message)
                                           (catch Exception _
                                             (try (on-failure) (catch Exception _ nil))
                                             (stop! queue)
                                             {:status :error :code :connection-failed}))
                                      {:status :error :code :stale-request})]
                         (locking (:gate queue) (swap! (:pending queue) disj key))
                         (try (callback result) (catch Exception _ nil))))
                     (catch InterruptedException _ nil))) "settlement-network")]
     (.setDaemon worker true)
     (reset! (:worker queue) worker)
     (.start worker)
     queue)))

(defn submit! [queue key message callback current? priority?]
  (locking (:gate queue)
    (cond
      @(:closed queue) false
      (contains? @(:pending queue) key) false
      :else
      (let [jobs ^LinkedBlockingDeque (:jobs queue)
            job {:key key :message message :callback callback :current? current?}]
        (when (and priority? (zero? (.remainingCapacity jobs)))
          (when-let [removed (.pollLast jobs)]
            (swap! (:pending queue) disj (:key removed))
            ((:callback removed) {:status :error :code :queue-full})))
        (if (if priority? (.offerFirst jobs job) (.offerLast jobs job))
          (do (swap! (:pending queue) conj key) true)
          false)))))
