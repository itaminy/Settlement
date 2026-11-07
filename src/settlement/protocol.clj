(ns settlement.protocol
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pprint]))

(defn read-message [message]
  (edn/read-string message))

(defn write-message [message]
  (pr-str message))