(ns settlement.auth
  (:require [settlement.world :as world])
  (:import [java.security SecureRandom MessageDigest]
           [java.util Base64 UUID]))

(defn new-state []
  {:instance (str (UUID/randomUUID)) :credentials {} :sessions {}})

(def state (ref (new-state)))

(defn- token []
  (let [bytes (byte-array 32)]
    (.nextBytes (SecureRandom.) bytes)
    (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bytes)))

(defn- digest [value]
  (.digest (MessageDigest/getInstance "SHA-256") (.getBytes ^String value "UTF-8")))

(defn failure [code] {:status :error :code code})

(defn owner [connection]
  (some (fn [[id session]] (when (= session connection) id)) (:sessions @state)))

(defn register! [connection id name]
  (if-not (and (integer? id) (pos? id) (string? name) (not (empty? name)))
    (failure :invalid-registration)
    (dosync
      (cond
        (owner connection) (failure :already-authenticated)
        (contains? (:players (world/get-world)) id) (failure :player-exists)
        (not (world/add-player id name)) (failure :map-full)
        :else
        (let [secret (token)]
          (alter state assoc-in [:credentials id] (digest secret))
          (alter state assoc-in [:sessions id] connection)
          {:status :ok :token secret :server-instance (:instance @state)})))))

(defn login! [connection id secret instance]
  (dosync
    (cond
      (not= instance (:instance @state)) (failure :stale-server)
      (owner connection) (failure :already-authenticated)
      (not (and (string? secret) (<= (count secret) 256)
                (get-in @state [:credentials id])
                (MessageDigest/isEqual (get-in @state [:credentials id]) (digest secret))))
      (failure :invalid-credentials)
      (contains? (:sessions @state) id) (failure :already-connected)
      :else (do (alter state assoc-in [:sessions id] connection) {:status :ok}))))

(defn disconnect! [connection]
  (dosync
    (world/cancel-connection-gathers! connection)
    (alter state update :sessions
           (fn [sessions] (into {} (remove #(= connection (val %)) sessions))))))
