(ns settlement.movement-ui
  (:require [settlement.client :as client]
            [settlement.config :as config])
  (:import [java.awt.event KeyEvent]
           [javax.swing AbstractAction JComponent KeyStroke]))

(defn create-controller [connection player-id stop-gather! apply-world! now]
  (let [state (atom {:held [] :pending? false :ready 0})
        tick! (fn []
                (let [{:keys [held pending? ready]} @state]
                  (when-let [{:keys [direction] :as press} (peek held)]
                    (when (and (not pending?) (>= (now) ready))
                      (swap! state assoc :pending? true)
                      (stop-gather!)
                      (when-not
                        (client/async! connection :move {:type :move :player-id player-id :direction direction}
                          (fn [{:keys [status world code]}]
                            (swap! state assoc :pending? false
                                   :ready (if (= :stale-request code) (now)
                                              (+ (now) config/move-animation-ms)))
                            (when (= :connection-failed code) (swap! state assoc :held []))
                            (when (= :ok status) (apply-world! world)))
                          #(= press (peek (:held @state))) (constantly true) false)
                        (swap! state assoc :pending? false :ready (+ (now) config/move-animation-ms)))))))
        press! (fn [key direction]
                 (when-not (some #(= key (:key %)) (:held @state))
                   (swap! state update :held conj {:key key :direction direction :press-id (Object.)})
                   (tick!)))]
    {:press! press!
     :release! (fn [key] (swap! state update :held #(into [] (remove (fn [press] (= key (:key press)))) %)))
     :clear! (fn [] (swap! state assoc :held []))
     :tick! tick!}))

(defn bind-keys! [panel {:keys [press! release!]}]
  (doseq [[key direction] [[KeyEvent/VK_UP :up] [KeyEvent/VK_W :up]
                           [KeyEvent/VK_DOWN :down] [KeyEvent/VK_S :down]
                           [KeyEvent/VK_LEFT :left] [KeyEvent/VK_A :left]
                           [KeyEvent/VK_RIGHT :right] [KeyEvent/VK_D :right]]
          [released? run] [[false #(press! key direction)] [true #(release! key)]]]
    (let [action (str "move-" key (when released? "-release"))]
      (.put (.getInputMap panel JComponent/WHEN_IN_FOCUSED_WINDOW)
            (KeyStroke/getKeyStroke key 0 released?) action)
      (.put (.getActionMap panel) action
            (proxy [AbstractAction] [] (actionPerformed [_] (run)))))))
