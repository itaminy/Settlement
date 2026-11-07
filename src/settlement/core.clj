(ns settlement.core
  (:require [settlement.server :as server]))

(defn -main []
  (server/start-server 5000))