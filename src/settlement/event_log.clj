(ns settlement.event-log)

(def history-limit 256)

(defn new-log []
  (ref {:version 0 :events []}))

(defonce journal (new-log))

(defn append! [changes]
  (alter journal
         (fn [{:keys [version events]}]
           (let [version (inc version)
                 event {:type :event :version version :seq version
                        :changes changes :resync true}]
             {:version version
              :events (if (= history-limit (count events))
                        (conj (into [] (rest events)) event)
                        (conj events event))})))
  nil)

(defn after
  "Reads an immutable journal view. Nil means that the cursor requires resynchronization."
  [{:keys [version events]} cursor]
  (let [oldest (- version (count events))]
    (when (<= oldest cursor version)
      (subvec events (int (- cursor oldest))))))
