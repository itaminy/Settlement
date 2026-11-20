(ns settlement.event-stream
  (:require [settlement.event-log :as log])
  (:import [java.io IOException]
           [java.net Socket]
           [java.util.concurrent ArrayBlockingQueue TimeUnit]))

(def queue-capacity 64)
(def ^:private slow-client-ns 1000000000)

(defn open-stream [^Socket socket]
  {:queue (ArrayBlockingQueue. queue-capacity)
   :subscription (atom nil)
   :snapshot (atom nil)
   :socket socket})

(defn- close-socket! [^Socket socket]
  (try (.close socket) (catch IOException _ nil)))

(defn send! [{:keys [^ArrayBlockingQueue queue socket]} frame]
  (if (.offer queue frame)
    true
    (do (close-socket! socket) false)))

(defn await-write! [{:keys [^Socket socket]} written]
  (loop []
    (if (realized? written)
      true
      (when-not (.isClosed socket)
        (if (deref written 25 nil) true (recur))))))

(defn write-loop! [{:keys [^ArrayBlockingQueue queue ^Socket socket]} write-line!]
  (try
    (while (not (.isClosed socket))
      (when-let [[line written] (.poll queue 25 TimeUnit/MILLISECONDS)]
        (write-line! line)
        (when written (deliver written true))))
    (catch IOException _ nil)
    (finally (close-socket! socket))))

(defn pump! [{:keys [subscription ^Socket socket] :as stream} encode-event]
  (try
    (loop [full-since nil]
      (when-not (.isClosed socket)
        (let [stalled?
              (locking stream
                (when-let [{:keys [cursor mode]} @subscription]
                  (if-let [events (log/after @log/journal cursor)]
                    (let [space (.remainingCapacity ^ArrayBlockingQueue (:queue stream))]
                      (doseq [event (take space events) :while (not (.isClosed socket))]
                        (when (send! stream [(encode-event mode event) nil])
                          (swap! subscription assoc :cursor (:seq event))))
                      (and (zero? space) (seq events)))
                    (close-socket! socket))))
              now (System/nanoTime)
              full-since (when stalled? (or full-since now))]
          (if (and full-since (>= (- now full-since) slow-client-ns))
            (close-socket! socket)
            (do (Thread/sleep 25)
                (recur full-since))))))
    (finally (close-socket! socket))))
