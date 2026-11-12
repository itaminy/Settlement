(ns settlement.credentials
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [java.io RandomAccessFile]
           [java.nio.file Files StandardCopyOption AtomicMoveNotSupportedException]
           [java.nio.file.attribute FileAttribute]))

(def ^:dynamic *credentials-file* ".settlement/credentials.edn")
(def ^:private process-lock (Object.))

(defn- read-data []
  (let [file (io/file *credentials-file*)]
    (if (.exists file)
      (let [data (edn/read-string (slurp file :encoding "UTF-8"))]
        (when-not (and (map? data) (= 1 (:version data)) (map? (:servers data)))
          (throw (Exception. "Invalid credentials file")))
        data)
      {:version 1 :servers {}})))

(defn- locked [f]
  (locking process-lock
    (let [file (.getAbsoluteFile (io/file *credentials-file*))
          directory (.getParentFile file)]
      (.mkdirs directory)
      (with-open [lock-file (RandomAccessFile. (io/file directory "credentials.lock") "rw")
                  channel (.getChannel lock-file)
                  lock (.lock channel)]
        (f file)))))

(defn load-entry [server id]
  (try
    (locked (fn [_]
              (let [entry (get-in (read-data) [:servers server])
                    player (get-in entry [:players id])]
                (when player
                  (when-not (and (string? (:token player)) (string? (:server-instance entry)))
                    (throw (Exception. "Invalid credentials entry")))
                  (assoc player :server-instance (or (:server-instance player) (:server-instance entry)))))))
    (catch Exception _ (throw (ex-info "Cannot read local credentials" {:code :credentials-read-failed})))))

(defn save-entry! [server id instance token]
  (try
    (locked
     (fn [file]
       (let [data (-> (read-data)
                      (assoc-in [:servers server :server-instance] instance)
                      (assoc-in [:servers server :players id] {:token token :server-instance instance}))
             temp (Files/createTempFile (.toPath (.getParentFile file)) "credentials-" ".tmp"
                                        (make-array FileAttribute 0))]
         (try
           (spit (.toFile temp) (pr-str data) :encoding "UTF-8")
           (try
             (Files/move temp (.toPath file)
                         (into-array StandardCopyOption [StandardCopyOption/ATOMIC_MOVE StandardCopyOption/REPLACE_EXISTING]))
             (catch AtomicMoveNotSupportedException _
               (Files/move temp (.toPath file)
                           (into-array StandardCopyOption [StandardCopyOption/REPLACE_EXISTING]))))
           (finally (Files/deleteIfExists temp))))))
    {:status :ok}
    (catch Exception _ {:status :error :code :credentials-save-failed})))
