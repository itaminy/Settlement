(ns settlement.text-commands
  (:require [clojure.string :as str]
            [settlement.config :as config])
  (:import [java.util Locale UUID]))

(def ^:private forms
  {["иди" "вверх"] [[{:type :move :direction :up} []]]
   ["иди" "вниз"] [[{:type :move :direction :down} []]]
   ["иди" "влево"] [[{:type :move :direction :left} []]]
   ["иди" "вправо"] [[{:type :move :direction :right} []]]
   ["покажи" "мир"] [[{:type :state} []]
                    [{:type :state :include-world-version true} ["с" "версией"]]]
   ["подпишись" "на"] [[{:type :subscribe-events} ["события"]]]
   ["отпишись" "от"] [[{:type :unsubscribe-events} ["событий"]]]
   ["покажи" "инвентарь"] [[{:type :inventory} []]]
   ["собери" "дерево"] [[{:type :gather-start :resource-type :tree}
                        ["попытка" :attempt-id]]
                       [{:type :gather-start :resource-type :tree}
                        ["на" :x :y "попытка" :attempt-id]]]
   ["собери" "камень"] [[{:type :gather-start :resource-type :stone}
                        ["попытка" :attempt-id]]
                       [{:type :gather-start :resource-type :stone}
                        ["на" :x :y "попытка" :attempt-id]]]
   ["продолжай" "добычу"] [[{:type :gather-heartbeat} [:operation-id]]]
   ["заверши" "добычу"] [[{:type :gather-finish} [:operation-id]]]
   ["отмени" "добычу"] [[{:type :gather-cancel} [:operation-id]]]
   ["отмени" "попытку"] [[{:type :gather-cancel} ["добычи" :attempt-id]]]
   ["построй" "забор"] [[{:type :build :building-type :fence} ["на" :x :y]]]
   ["построй" "дом"] [[{:type :build :building-type :house} ["на" :x :y]]]
   ["атакуй" "постройку"] [[{:type :attack-building}
                           ["на" :x :y "id" :building-id "номер" :attack-seq]]]
   ["почини" "постройку"] [[{:type :repair-building}
                           ["на" :x :y "id" :building-id "номер" :repair-seq]]]})

(def ^:private verbs (into #{} (map first) (keys forms)))

(defn- decimal [token]
  (when (re-matches #"-?[0-9]+" token)
    (parse-long token)))

(defn- uuid-value [token]
  (when (re-matches #"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}" token)
    (UUID/fromString token)))

(defn- argument-value [field token]
  (case field
    :x (when-let [n (decimal token)]
         (when (<= 0 n (dec config/world-width)) n))
    :y (when-let [n (decimal token)]
         (when (<= 0 n (dec config/world-height)) n))
    (:attack-seq :repair-seq) (when-let [n (decimal token)]
                               (when (pos? n) n))
    :building-id (uuid-value token)
    (:attempt-id :operation-id) (when-let [id (uuid-value token)]
                                  (str id))))

(defn- parse-form [command fields tokens]
  (loop [command command, fields (seq fields), tokens (seq tokens)]
    (cond
      (nil? fields) (cond
                      tokens :invalid-argument
                      (contains? command :x)
                      (-> command
                          (assoc :position [(:x command) (:y command)])
                          (dissoc :x :y))
                      :else command)
      (nil? tokens) :missing-argument
      :else
      (let [field (first fields)
            token (first tokens)]
        (if (string? field)
          (if (= field token)
            (recur command (next fields) (next tokens))
            :invalid-argument)
          (if-let [value (argument-value field token)]
            (recur (assoc command field value) (next fields) (next tokens))
            :invalid-argument))))))

(defn- failure [code]
  {:status :error :code code})

(defn parse-command
  "Parses one Russian command. Returns a command map or a structured error.
   The caller supplies the authenticated player ID."
  [input]
  (cond
    (not (string? input)) (failure :invalid-argument)
    (re-find #"\v" input) (failure :invalid-argument)
    :else
    (let [line (.toLowerCase ^String input Locale/ROOT)
          tokens (str/split (str/replace line #"\A\h+|\h+\z" "") #"\h+")
          [verb subject & arguments] tokens]
      (if-let [variants (forms [verb subject])]
        (let [results (map (fn [[command fields]]
                             (parse-form command fields arguments))
                           variants)]
          (or (some #(when (map? %) %) results)
              (failure (if (some #{:missing-argument} results)
                         :missing-argument
                         :invalid-argument))))
        (failure (if (and (verbs verb) (nil? subject))
                   :missing-argument
                   :unknown-command))))))
