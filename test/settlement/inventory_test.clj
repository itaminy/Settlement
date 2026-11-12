(ns settlement.inventory-test
  (:require [clojure.test :refer :all]
            [settlement.inventory :as inventory]
            [settlement.inventory-ui :as inventory-ui]
            [settlement.world :as world]
            [settlement.server :as server]
            [settlement.auth :as auth]
            [settlement.client :as client]
            [settlement.gui :as gui])
  (:import [javax.swing JPanel KeyStroke]
           [java.awt.event KeyEvent]))

(use-fixtures :each
  (fn [f]
    (with-redefs [world/world (ref {:players {} :resources {:wood 0 :stone 0}
                                   :objects {[1 0] :tree [3 0] :stone}})
                  auth/state (ref (auth/new-state))
                  world/schedule-resource-restore (fn [_ _] nil)]
      (f))))

(defn- register! [connection id]
  (server/process-message connection {:type :register :player-id id :name (str id)}))

(deftest empty-inventory-test
  (register! :a 1)
  (is (= inventory/empty-inventory (get-in @world/world [:players 1 :inventory])))
  (is (= 12 (inventory/free-slots (get-in @world/world [:players 1 :inventory]))))
  (is (not (contains? (get-in @world/world [:players 1]) :collected)))
  (is (= {:wood 0 :stone 0} (get-in (world/get-world) [:players 1 :collected]))))

(deftest migration-preserves-existing-resources-test
  (dosync (alter world/world assoc :players
                 {1 {:name "Existing" :x 7 :y 8 :collected {:wood 23 :stone 9}}}))
  (world/migrate-inventories!)
  (let [player (get-in @world/world [:players 1])]
    (is (= {:items {:wood 23 :stone 9 :axe 0 :pickaxe 0}} (:inventory player)))
    (is (= ["Existing" 7 8] ((juxt :name :x :y) player)))
    (is (not (contains? player :collected)))
    (world/migrate-inventories!)
    (is (= player (get-in @world/world [:players 1]))))
  (is (true? (world/collect-resource 1 :wood 2)))
  (is (= {:wood 25 :stone 9} (get-in (world/get-world) [:players 1 :collected]))))

(deftest inventory-is-authoritative-test
  (register! :a 1)
  (dosync (alter world/world update-in [:players 1]
                 assoc :collected {:wood 999 :stone 999}))
  (is (= {:wood 0 :stone 0} (get-in (world/get-world) [:players 1 :collected])))
  (world/migrate-inventories!)
  (world/change-player-items! 1 {:wood 5 :stone 3})
  (world/change-player-items! 1 {:wood -2 :stone -1})
  (is (= {:wood 3 :stone 2} (get-in (world/get-world) [:players 1 :collected])))
  (is (= {:items {:wood 3 :stone 2 :axe 0 :pickaxe 0}}
         (get-in @world/world [:players 1 :inventory]))))

(deftest independent-gathered-inventories-test
  (register! :a 1)
  (register! :b 2)
  (is (= :ok (:status (server/process-message :a {:type :gather :player-id 1}))))
  (is (= :ok (:status (server/process-message :b {:type :gather :player-id 2}))))
  (let [state (world/get-world)]
    (is (= {:wood 1 :stone 0 :axe 0 :pickaxe 0} (get-in state [:players 1 :inventory :items])))
    (is (= {:wood 0 :stone 1 :axe 0 :pickaxe 0} (get-in state [:players 2 :inventory :items])))
    (is (= {:wood 1 :stone 1} (:resources state)))
    (doseq [id [1 2]]
      (is (= (get-in state [:players id :collected])
             (inventory/resources (get-in state [:players id :inventory])))))))

(deftest reconnect-keeps-inventory-test
  (let [registration (register! :a 1)]
    (server/process-message :a {:type :gather :player-id 1})
    (let [before (world/get-world)]
      (auth/disconnect! :a)
      (is (= :ok (:status (server/process-message :new
                           (merge (select-keys registration [:token :server-instance])
                                  {:type :login :player-id 1})))))
      (is (= before (world/get-world)))
      (is (= 1 (get-in (world/get-world) [:players 1 :inventory :items :wood]))))))

(deftest forged-inventory-commands-rejected-test
  (register! :a 1)
  (register! :b 2)
  (doseq [message [{:type :gather :player-id 1}
                   {:type :collect :player-id 1 :resource :wood :amount 500}
                   {:type :set-inventory :player-id 1 :inventory {:items {:wood 500}}}
                   {:type :change-items :player-id 1 :changes {:axe 1}}]]
    (let [before (world/get-world)]
      (is (= :error (:status (server/process-message :b message))))
      (is (= before (world/get-world)))))
  (server/process-message :b {:type :gather :player-id 2 :inventory {:items {:axe 99}}})
  (is (= 0 (get-in (world/get-world) [:players 2 :inventory :items :axe]))))

(deftest future-cost-and-reward-validation-test
  (register! :a 1)
  (world/change-player-items! 1 {:wood 5 :stone 2})
  (is (= :ok (:status (world/change-player-items! 1 {:wood -3 :stone -1 :axe 1 :pickaxe 1}))))
  (let [before (world/get-world)]
    (is (= :insufficient-items (:code (world/change-player-items! 1 {:wood -99 :axe 1}))))
    (is (= :invalid-items (:code (world/change-player-items! 1 {:gold 10}))))
    (is (= :invalid-items (:code (world/change-player-items! 1 {:wood 1.5}))))
    (is (= :player-not-found (:code (world/change-player-items! 99 {:wood 1}))))
    (is (= before (world/get-world))))
  (is (= {:wood 2 :stone 1 :axe 1 :pickaxe 1}
         (get-in (world/get-world) [:players 1 :inventory :items]))))

(deftest inventory-panel-updates-after-gather-test
  (register! :a 1)
  (javax.swing.SwingUtilities/invokeAndWait
   (fn []
     (let [contents inventory/empty-inventory
           ui (inventory-ui/create-panel contents)
           resource-ui (assoc (gui/create-resource-panel {:wood 0 :stone 0}) :inventory-ui ui)
           panel (gui/create-map-panel 0 0 1)]
       (is (= 12 (count (.getComponents (:slots ui)))))
       (is (= "Свободно слотов: 12 / 12" (.getText (:free-label ui))))
       (with-redefs [client/send-message (fn [_ _ message] (server/process-message :a message))]
         (gui/bind-gather-key panel {} resource-ui 1)
         (.actionPerformed (.get (.getActionMap panel) "gather") nil))
       (is (= "Количество: 1" (.getText (get-in ui [:counts :wood]))))
       (is (= "Количество: 0" (.getText (get-in ui [:counts :axe]))))
       (is (= "Свободно слотов: 11 / 12" (.getText (:free-label ui))))
       (is (= "Моё дерево: 1" (.getText (:wood-label resource-ui))))))))

(deftest inventory-key-toggle-test
  (let [panel (JPanel.) visible (atom false)]
    (gui/bind-inventory-key panel #(swap! visible not))
    (is (= "inventory" (.get (.getInputMap panel JPanel/WHEN_IN_FOCUSED_WINDOW)
                             (KeyStroke/getKeyStroke KeyEvent/VK_I 0))))
    (.actionPerformed (.get (.getActionMap panel) "inventory") nil)
    (is (true? @visible))
    (.actionPerformed (.get (.getActionMap panel) "inventory") nil)
    (is (false? @visible))))
