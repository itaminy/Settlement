(ns settlement.buildings-test
  (:require [clojure.test :refer :all]
            [settlement.buildings :as buildings]
            [settlement.building-ui :as building-ui]
            [settlement.world :as world]
            [settlement.auth :as auth]
            [settlement.server :as server]
            [settlement.client :as client]
            [settlement.transport :as transport]
            [settlement.layout :as layout]
            [settlement.config :as config]
            [settlement.rendering :as rendering]
            [settlement.gui :as gui]
            [settlement.test-support :as support])
  (:import [javax.swing SwingUtilities JLabel]
           [java.net ServerSocket Socket]
           [java.awt.image BufferedImage]
           [java.util.concurrent CountDownLatch TimeUnit]))

(use-fixtures :each
  (fn [f]
    (with-redefs [config/bot-timing-ms {:move 0 :action 0 :after-build 0}
                  world/world (ref {:players {} :objects {} :resources {:wood 0 :stone 0}
                                   :match world/initial-match})
                  world/gathering (ref {}) auth/state (ref (auth/new-state))
                  layout/spawn-position [0 0] layout/spawn-cells support/row-major-spawn
                  buildings/protected-cells #{[0 0]}]
      (f))))

(defn- player! [id position stock]
  (server/process-message id {:type :register :player-id id :name (str id)
                              :client-kind (if (even? id) :prolog :human)})
  (let [[x y] position] (dosync (alter world/world update-in [:players id] assoc :x x :y y)))
  (world/change-player-items! id stock))
(defn- build! [id kind position]
  (server/process-message id {:type :build :player-id id :building-type kind :position position}))
(defn- attack-command [id position building-id attack-seq]
  {:type :attack-building :player-id id :position position
   :building-id building-id :attack-seq attack-seq})
(defn- repair-command [id position building-id repair-seq]
  {:type :repair-building :player-id id :position position
   :building-id building-id :repair-seq repair-seq})
(defn- items [id] (get-in (world/get-world) [:players id :inventory :items]))
(defn- race [operations]
  (let [ready (CountDownLatch. (count operations)) start (promise)
        tasks (mapv #(future (.countDown ready) @start (%)) operations)]
    (try
      (is (.await ready 5 TimeUnit/SECONDS)) (deliver start true)
      (mapv #(deref % 5000 ::timeout) tasks)
      (finally (deliver start true) (doseq [task tasks] (future-cancel task))))))

(deftest fence-and-house-costs-test
  (player! 1 [10 10] {:wood 30 :stone 20})
  (is (= :ok (:status (build! 1 :fence [10 11]))))
  (is (= {:blue 0 :red 0} (get-in (world/get-world) [:match :scores])))
  (is (= :ok (:status (build! 1 :house [11 10]))))
  (is (= {:scores {:blue 15 :red 0} :phase :active :winner nil} (:match (world/get-world))))
  (is (= {:wood 13 :stone 10 :axe 0 :pickaxe 0} (items 1)))
  (let [objects (:objects (world/get-world))
        fence (objects [10 11])
        house (objects [11 10])]
    (is (= 2 (count objects)))
    (is (= {:type :building :kind :house :owner-id 1 :team :blue :hp 200 :max-hp 200}
           (dissoc house :building-id)))
    (is (= {:hp 50 :max-hp 50} (select-keys fence [:hp :max-hp])))
    (is (every? uuid? (map :building-id [fence house])))
    (is (not= (:building-id fence) (:building-id house)))
    (is (= #{[11 10] [12 10] [11 11] [12 11] [10 11]} (buildings/occupied-cells objects)))
    (is (not (contains? objects [12 11])))))

(deftest all-house-cells-block-movement-and-restoration-test
  (player! 1 [10 10] {:wood 15 :stone 10})
  (is (= :ok (:status (build! 1 :house [11 10]))))
  (doseq [[origin direction target] [[[10 10] :right [11 10]] [[13 10] :left [12 10]]
                                     [[10 11] :right [11 11]] [[13 11] :left [12 11]]]]
    (dosync (alter world/world update-in [:players 1] assoc :x (first origin) :y (second origin)))
    (is (= :object-blocked (:code (world/move-player-result 1 direction))))
    (is (= :exists (world/restore-resource target :tree))))
  (is (= 1 (count (:objects (world/get-world))))))

(deftest spawn-skips-entire-building-footprint-test
  (player! 1 [10 10] {:wood 15 :stone 10})
  (build! 1 :house [11 10])
  (with-redefs [layout/spawn-cells (fn [] [[12 10] [12 11] [11 11] [13 10]])]
    (is (true? (world/add-player 2 "B")))
    (is (= [13 10] ((juxt :x :y) (get-in (world/get-world) [:players 2]))))))

(deftest rejected-builds-never-charge-test
  (player! 1 [10 10] {:wood 50 :stone 50})
  (doseq [[kind position code] [[:unknown [11 10] :unknown-building]
                                 [:house [(dec config/world-width) (dec config/world-height)] :out-of-bounds] [:fence [-1 10] :out-of-bounds]
                                 [:house [12 13] :too-far] [:fence [12 11] :too-far]
                                 [:fence [10 10] :player-blocked] [:house [10 10] :player-blocked]
                                 [:house [11] :invalid-position] [:fence [1.5 1] :invalid-position]]]
    (let [before (world/get-world)]
      (is (= code (:code (build! 1 kind position))))
      (is (= before (world/get-world)))))
  (world/change-player-items! 1 {:wood -50 :stone -50})
  (let [before (world/get-world)]
    (is (= :insufficient-items (:code (build! 1 :fence [11 10]))))
    (is (= before (world/get-world)))))

(deftest distance-is-to-nearest-cell-test
  (player! 1 [14 10] {:wood 15 :stone 10})
  (is (= :ok (:status (build! 1 :house [11 10])))))

(deftest lookup-building-by-occupied-cell-test
  (let [house {:type :building :kind :house :building-id (random-uuid)}
        fence {:type :building :kind :fence :building-id (random-uuid)}
        objects {[11 10] house [15 10] fence [10 10] :house [2 2] :tree}]
    (doseq [cell [[11 10] [12 10] [11 11] [12 11]]]
      (is (= {:anchor [11 10] :building house} (buildings/target-at objects cell))))
    (is (= {:anchor [15 10] :building fence} (buildings/target-at objects [15 10])))
    (doseq [cell [[10 10] [13 11] [2 2] [-1 0] [11.5 10] nil]]
      (is (nil? (buildings/target-at objects cell))))
    (is (nil? (buildings/target-at (assoc objects [12 11] fence) [12 11])))))

(deftest interaction-distance-to-building-test
  (let [objects {[11 10] {:type :building :kind :house}
                 [15 10] {:type :building :kind :fence}}
        house (buildings/target-at objects [12 11])
        fence (buildings/target-at objects [15 10])]
    (doseq [[target player distance] [[house {:x 13 :y 11} 1]
                                      [house {:x 13 :y 12} 2]
                                      [house {:x 12 :y 12} 1]
                                      [house {:x 11 :y 10} 0]
                                      [fence {:x 14 :y 10} 1]
                                      [fence {:x 14 :y 11} 2]]]
      (is (= distance (buildings/distance-to-building player target)))
      (is (= (= 1 distance) (buildings/adjacent-to-building? player target))))))

(deftest migrate-building-durability-test
  (let [id (random-uuid)
        house {:type :building :kind :house :team :blue :building-id id :hp 17}
        fence {:type :building :kind :fence :owner-id 99}
        objects {[5 5] house [8 8] fence [9 9] :house [1 1] :tree [2 2] :stone}]
    (dosync (alter world/world assoc :objects objects))
    (let [before (world/get-world)]
      (world/migrate-building-durability!)
      (let [state (world/get-world)
            migrated (:objects state)
            new-id (get-in migrated [[8 8] :building-id])]
        (is (uuid? new-id))
        (is (not= id new-id))
        (is (= (assoc objects
                      [5 5] (assoc house :max-hp 200)
                      [8 8] (assoc fence :building-id new-id :hp 50 :max-hp 50))
               migrated))
        (is (= (dissoc before :objects) (dissoc state :objects)))
        (world/migrate-building-durability!)
        (is (= state (world/get-world)))))))

(deftest existing-objects-and-disconnected-players-block-test
  (player! 1 [10 10] {:wood 50 :stone 50})
  (doseq [object [:tree :stone :house {:type :building :kind :fence :owner-id 2}]]
    (dosync (alter world/world assoc :objects {[12 11] object}))
    (let [before (world/get-world)]
      (is (= :object-blocked (:code (build! 1 :house [11 10]))))
      (is (= before (world/get-world)))))
  (dosync (alter world/world assoc :objects {}))
  (player! 2 [12 11] {}) (auth/disconnect! 2)
  (let [before (world/get-world)]
    (is (= :player-blocked (:code (build! 1 :house [11 10]))))
    (is (= before (world/get-world)))))

(deftest build-authorization-and-server-catalog-test
  (player! 1 [10 10] {:wood 10})
  (is (= :not-authenticated (:code (server/process-message :other {:type :build :player-id 1 :building-type :fence :position [11 10]}))))
  (is (= :wrong-player (:code (server/process-message 1 {:type :build :player-id 2 :building-type :fence :position [11 10]}))))
  (let [before (world/get-world)]
    (is (= :ok (:status (server/process-message 1 {:type :build-check :player-id 1 :building-type :fence :position [11 10]}))))
    (is (= before (world/get-world))))
  (let [fake-id (random-uuid)
        result (server/process-message 1 {:type :build :player-id 1 :building-type :fence :position [11 10]
                                         :cost {:wood 0} :owner-id 99 :team :red :size [0 0]
                                         :building-id fake-id :hp 1 :max-hp 1})
        building (get-in (world/get-world) [:objects [11 10]])]
    (is (= :ok (:status result)))
    (is (= 8 (:wood (items 1))))
    (is (= building (:building result)))
    (is (not= fake-id (:building-id building)))
    (is (= {:owner-id 1 :team :blue :hp 50 :max-hp 50}
           (select-keys building [:owner-id :team :hp :max-hp])))))

(deftest concurrent-building-and-spending-test
  (player! 1 [10 10] {:wood 2}) (player! 2 [12 10] {:wood 2})
  (is (= {:ok 1 :error 1} (frequencies (map :status (race [#(build! 1 :fence [11 10]) #(build! 2 :fence [11 10])])))))
  (is (= 1 (count (:objects (world/get-world)))))
  (is (= [0 2] (sort [(:wood (items 1)) (:wood (items 2))]))))

(deftest overlapping-houses-cannot-both-commit-test
  (player! 1 [10 10] {:wood 15 :stone 10}) (player! 2 [14 10] {:wood 15 :stone 10})
  (is (= {:ok 1 :error 1} (frequencies (map :status (race [#(build! 1 :house [11 10]) #(build! 2 :house [12 10])])))))
  (is (= 4 (count (buildings/occupied-cells (:objects (world/get-world)))))))

(deftest attack-authorization-target-and-distance-test
  (player! 1 [10 10] {:wood 2})
  (player! 2 [12 11] {})
  (let [id (get-in (build! 1 :fence [11 10]) [:building :building-id])
        command (attack-command 2 [11 10] id 1)
        orphan (buildings/initialize-durability {:type :building :kind :fence})]
    (dosync (alter world/world update :objects assoc [12 12] orphan [13 11] :house))
    (is (= :not-authenticated (:code (server/process-message :unknown command))))
    (is (= :wrong-player (:code (server/process-message 1 command))))
    (is (= :invalid-target-team (:code (server/process-message 1 (assoc command :player-id 1)))))
    (is (= :too-far (:code (server/process-message 2 command))))
    (is (= :building-not-found (:code (server/process-message 2 (assoc command :position [13 11] :attack-seq 2)))))
    (is (= :invalid-target-team (:code (server/process-message 2 (assoc command :position [12 12]
                                                                      :building-id (:building-id orphan) :attack-seq 3)))))
    (dosync (alter world/world update-in [:players 2] assoc :x 12 :y 10))
    (let [result (server/process-message 2 (assoc command :attack-seq 4 :damage 500
                                                 :team :blue :owner-id 2 :hp 0 :max-hp 1))]
      (is (= :ok (:status result)))
      (is (= 40 (:hp result)))
      (is (= {:hp 40 :team :blue :owner-id 1 :max-hp 50}
             (select-keys (get-in result [:world :objects [11 10]]) [:hp :team :owner-id :max-hp]))))))

(deftest attack-cooldown-and-replay-survive-reconnect-test
  (player! 1 [10 10] {:wood 4})
  (let [registered (server/process-message 2 {:type :register :player-id 2 :name "Red" :client-kind :prolog})
        a (get-in (build! 1 :fence [11 10]) [:building :building-id])
        b (get-in (build! 1 :fence [10 11]) [:building :building-id])
        first-hit (attack-command 2 [11 10] a 1)
        second-hit (attack-command 2 [10 11] b 2)
        clock (atom 0)]
    (with-redefs [world/now-ns #(deref clock)]
      (dosync (alter world/world update-in [:players 2] assoc :x 11 :y 11))
      (is (= 40 (:hp (server/process-message 2 first-hit))))
      (auth/disconnect! 2)
      (is (= :ok (:status (server/process-message :new
                            (assoc (select-keys registered [:token :server-instance])
                                   :type :login :player-id 2)))))
      (is (= 40 (:hp (server/process-message :new first-hit))))
      (is (= :attack-seq-conflict (:code (server/process-message :new (assoc second-hit :attack-seq 1)))))
      (reset! clock 999999999)
      (is (= :attack-cooldown (:code (server/process-message :new second-hit))))
      (is (= :stale-attack (:code (server/process-message :new first-hit))))
      (reset! clock 1000000000)
      (is (= :attack-cooldown (:code (server/process-message :new second-hit))))
      (let [hit (server/process-message :new (assoc second-hit :attack-seq 3))]
        (is (= 40 (:hp hit)))
        (is (= 40 (get-in hit [:world :objects [11 10] :hp])))
        (is (= 40 (get-in hit [:world :objects [10 11] :hp])))
        (is (not (contains? (:world hit) :building-attacks)))))))

(deftest destroy-house-releases-footprint-and-rejects-old-target-test
  (player! 1 [10 10] {:wood 30 :stone 20})
  (player! 2 [13 11] {})
  (let [anchor [11 10]
        house (:building (build! 1 :house anchor))
        command (attack-command 2 [12 11] (:building-id house) 1)
        clock (atom 0)]
    (with-redefs [world/now-ns #(deref clock)]
      (doseq [n (range 1 20)]
        (is (= (- 200 (* 10 n)) (:hp (server/process-message 2 (assoc command :attack-seq n)))))
        (swap! clock + 1000000000))
      (let [final-hit (assoc command :attack-seq 20)
            result (server/process-message 2 final-hit)]
        (is (= {:hp 0 :destroyed? true} (select-keys result [:hp :destroyed?])))
        (is (= result (server/process-message 2 final-hit)))
        (is (= {:blue 0 :red 0} (get-in result [:world :match :scores])))
        (doseq [cell (buildings/cells anchor house)]
          (is (world/free-cell? (:world result) cell))))
      (is (= :ok (:status (build! 1 :house anchor))))
      (swap! clock + 1000000000)
      (let [result (server/process-message 2 (assoc command :attack-seq 21))]
        (is (= :stale-building (:code result)))
        (is (= 200 (get-in result [:world :objects anchor :hp])))))))

(deftest concurrent-attacks-from-one-player-apply-only-one-hit-test
  (player! 1 [10 10] {:wood 2})
  (player! 2 [12 10] {})
  (let [id (get-in (build! 1 :fence [11 10]) [:building :building-id])
        command (attack-command 2 [11 10] id 1)
        attack #(server/process-message 2 %)]
    (with-redefs [world/now-ns (constantly 0)]
      (let [results (race [#(attack command) #(attack command)])]
        (is (= [40 40] (mapv :hp results)))
        (is (= 40 (get-in (world/get-world) [:objects [11 10] :hp])))))
    (with-redefs [world/now-ns (constantly 1000000000)]
      (let [results (race [#(attack (assoc command :attack-seq 2))
                           #(attack (assoc command :attack-seq 3))])]
        (is (= {:ok 1 :error 1} (frequencies (map :status results))))
        (is (= 30 (get-in (world/get-world) [:objects [11 10] :hp])))))))

(deftest concurrent-final-hits-remove-building-once-test
  (player! 1 [10 10] {:wood 2})
  (player! 2 [12 10] {})
  (player! 3 [14 10] {})
  (player! 4 [11 11] {})
  (let [position [11 10]
        id (get-in (build! 1 :fence position) [:building :building-id])]
    (dosync (alter world/world assoc-in [:objects position :hp] 10))
    (let [results (race (mapv (fn [player-id]
                                #(server/process-message player-id (attack-command player-id position id 1)))
                              [2 4]))]
      (is (= {:ok 1 :error 1} (frequencies (map :status results))))
      (is (= 1 (count (filter :destroyed? results))))
      (is (= [:building-not-found] (keep :code results)))
      (is (= {:blue 0 :red 0} (get-in (world/get-world) [:match :scores])))
      (is (world/free-cell? (world/get-world) position)))))

(deftest repair-uses-server-price-and-caps-healing-test
  (player! 1 [10 10] {:wood 17 :stone 10})
  (player! 2 [14 11] {:wood 1})
  (player! 3 [13 11] {:wood 2})
  (let [anchor [11 10]
        id (get-in (build! 1 :house anchor) [:building :building-id])
        repair #(repair-command 1 [12 11] id %)]
    (dosync (alter world/world assoc-in [:objects anchor :hp] 186)
            (alter world/world update-in [:players 1] assoc :x 13 :y 10))
    (let [result (server/process-message 1 (assoc (repair 1)
                                                 :cost {:wood 0} :hp 999 :restored-hp 999
                                                 :team :red :owner-id 3 :max-hp 999))]
      (is (= {:status :ok :hp 196 :restored-hp 10 :anchor anchor :building-id id}
             (dissoc result :world)))
      (is (= 1 (:wood (items 1))))
      (is (= 2 (:wood (items 3)))))
    (let [result (server/process-message 1 (repair 2))]
      (is (= {:hp 200 :restored-hp 4} (select-keys result [:hp :restored-hp])))
      (is (zero? (:wood (items 1)))))
    (let [state (world/get-world)]
      (is (= :building-at-full-hp (:code (server/process-message 1 (repair 3)))))
      (is (= state (world/get-world)))
      (is (= {:blue 15 :red 0} (get-in state [:match :scores]))))))

(deftest repair-validates-authority-and-target-without-charging-test
  (player! 1 [10 10] {:wood 2})
  (player! 2 [12 10] {:wood 1})
  (let [anchor [11 10]
        id (get-in (build! 1 :fence anchor) [:building :building-id])
        orphan (buildings/initialize-durability {:type :building :kind :fence})
        repair #(repair-command 1 anchor id %)]
    (dosync (alter world/world assoc-in [:objects anchor :hp] 20)
            (alter world/world update :objects assoc [10 9] orphan [10 11] :house))
    (let [before (world/get-world)]
      (is (= :not-authenticated (:code (server/process-message :unknown (repair 1)))))
      (is (= :wrong-player (:code (server/process-message 2 (repair 1)))))
      (is (= :invalid-target-team (:code (server/process-message 2 (assoc (repair 1) :player-id 2 :team :blue)))))
      (is (= :insufficient-items (:code (server/process-message 1 (repair 1)))))
      (is (= :stale-building (:code (server/process-message 1 (assoc (repair 2) :building-id (random-uuid))))))
      (is (= :invalid-target-team (:code (server/process-message 1 (assoc (repair 3) :position [10 9]
                                                                        :building-id (:building-id orphan))))))
      (is (= :building-not-found (:code (server/process-message 1 (assoc (repair 4) :position [10 11])))))
      (is (= before (world/get-world))))
    (world/change-player-items! 1 {:wood 1})
    (is (= :ok (:status (server/process-message 1 {:type :move :player-id 1 :direction :left}))))
    (let [before (world/get-world)]
      (is (= :too-far (:code (server/process-message 1 (repair 5)))))
      (is (= before (world/get-world))))))

(deftest repair-replay-survives-reconnect-test
  (let [registration (server/process-message 1 {:type :register :player-id 1 :name "Blue"})
        anchor [11 10]]
    (dosync (alter world/world update-in [:players 1] assoc :x 10 :y 10))
    (world/change-player-items! 1 {:wood 5})
    (let [id (get-in (build! 1 :fence anchor) [:building :building-id])
          command (repair-command 1 anchor id 1)]
      (dosync (alter world/world assoc-in [:objects anchor :hp] 20))
      (let [initial (server/process-message 1 command)]
        (is (= 30 (:hp initial)))
        (auth/disconnect! 1)
        (is (= :ok (:status (server/process-message :new
                              (assoc (select-keys registration [:token :server-instance])
                                     :type :login :player-id 1)))))
        (is (= initial (server/process-message :new command)))
        (is (= :repair-seq-conflict (:code (server/process-message :new (assoc command :position [9 10])))))
        (is (= 2 (:wood (items 1))))
        (is (= 40 (:hp (server/process-message :new (assoc command :repair-seq 2)))))
        (is (= :stale-repair (:code (server/process-message :new command))))
        (is (= 1 (:wood (items 1))))))))

(deftest concurrent-repairs-cannot-double-charge-or-exceed-max-hp-test
  (player! 1 [10 10] {:wood 4})
  (player! 2 [14 10] {})
  (player! 3 [12 10] {:wood 2})
  (let [anchor [11 10]
        id (get-in (build! 1 :fence anchor) [:building :building-id])
        request #(server/process-message %1 (repair-command %1 anchor id %2))]
    (dosync (alter world/world assoc-in [:objects anchor :hp] 34))
    (is (= [44 44] (mapv :hp (race [#(request 1 1) #(request 1 1)]))))
    (is (= 1 (:wood (items 1))))
    (let [results (race [#(request 1 2) #(request 3 1)])]
      (is (= {:ok 1 :error 1} (frequencies (map :status results))))
      (is (= [:not-building-owner] (keep :code results)))
      (is (= 50 (get-in (world/get-world) [:objects anchor :hp])))
      (is (= 2 (+ (:wood (items 1)) (:wood (items 3))))))))

(deftest repair-races-with-destruction-test
  (player! 1 [10 10] {:wood 3})
  (player! 2 [12 10] {})
  (let [anchor [11 10]
        id (get-in (build! 1 :fence anchor) [:building :building-id])
        repair (repair-command 1 anchor id 1)
        hit (attack-command 2 anchor id 1)]
    (dosync (alter world/world assoc-in [:objects anchor :hp] 10))
    (let [[repaired attacked] (race [#(server/process-message 1 repair) #(server/process-message 2 hit)])
          state (world/get-world)]
      (is (= :ok (:status attacked)))
      (if (:destroyed? attacked)
        (do
          (is (= :building-not-found (:code repaired)))
          (is (nil? (get-in state [:objects anchor])))
          (is (= {:blue 0 :red 0} (get-in state [:match :scores])))
          (is (= 1 (:wood (items 1)))))
        (do
          (is (= 20 (:hp repaired)))
          (is (= 10 (get-in state [:objects anchor :hp])))
          (is (= {:blue 0 :red 0} (get-in state [:match :scores])))
          (is (zero? (:wood (items 1)))))))))

(deftest repair-and-construction-cannot-overspend-wood-test
  (player! 1 [10 10] {:wood 4})
  (let [anchor [11 10]
        id (get-in (build! 1 :fence anchor) [:building :building-id])]
    (dosync (alter world/world assoc-in [:objects anchor :hp] 20))
    (let [results (race [#(server/process-message 1 (repair-command 1 anchor id 1))
                         #(build! 1 :fence [10 11])])
          state (world/get-world)
          repaired? (= :ok (:status (first results)))]
      (is (= {:ok 1 :error 1} (frequencies (map :status results))))
      (is (= [:insufficient-items] (keep :code results)))
      (is (= (if repaired? 1 0) (:wood (items 1))))
      (is (= (if repaired? 30 20) (get-in state [:objects anchor :hp])))
      (is (= 0 (get-in state [:match :scores :blue]))))))

(defn- put-scoring-structures! [teams]
  (dosync
    (alter world/world update :objects into
           (map-indexed (fn [index team]
                          [[(+ 1 (* 3 index)) 20]
                           {:type :building :kind :sawmill :team team}])
                        teams)))
  (world/migrate-teams!))

(deftest concurrent-winning-builds-test
  (player! 1 [10 10] {:wood 20 :stone 10})
  (player! 2 [15 10] {:wood 20 :stone 10})
  (put-scoring-structures! (concat (repeat 4 :blue) (repeat 4 :red)))
  (is (= {:scores {:blue 80 :red 80} :phase :active :winner nil} (:match (world/get-world))))
  (let [results (race [#(build! 1 :sawmill [11 10]) #(build! 2 :sawmill [16 10])])
        winner (if (= :ok (:status (first results))) :blue :red)
        loser ({:blue :red :red :blue} winner)
        state (world/get-world)]
    (is (= {:ok 1 :error 1} (frequencies (map :status results))))
    (is (= :match-finished (:code (first (filter :code results)))))
    (is (= {:scores {winner 100 loser 80} :phase :finished :winner winner} (:match state)))
    (is (= (:match state) (get-in (first (filter :building results)) [:world :match])))
    (is (= 9 (count (:objects state))))
    (is (= [0 20] (sort [(:wood (items 1)) (:wood (items 2))])))))

(deftest match-migration-recalculates-without-duplicating-points-test
  (dosync (alter world/world dissoc :match))
  (dosync (alter world/world assoc-in [:objects [12 11]] :house)
          (alter world/world assoc-in [:objects [14 12]] {:type :building :kind :house :owner-id 99})
          (alter world/world assoc-in [:objects [16 12]] {:type :building :kind :house :team :red}))
  (put-scoring-structures! (concat (repeat 5 :blue) (repeat 5 :red)))
  (let [state (world/get-world)]
    (is (= {:scores {:blue 100 :red 115} :phase :finished :winner :red} (:match state)))
    (world/migrate-teams!)
    (is (= state (world/get-world)))
    ;; Emulate removing one building to verify that the score can be rebuilt.
    (dosync (alter world/world update :objects dissoc [16 12] [1 20] [16 20] [19 20]))
    (world/migrate-teams!)
    (is (= {:scores {:blue 80 :red 60} :phase :finished :winner :red} (:match (world/get-world))))))

(deftest completed-match-blocks-gameplay-test
  (player! 1 [10 10] {:wood 20 :stone 10})
  (let [registration (server/process-message 2 {:type :register :player-id 2 :name "Red" :client-kind :prolog})]
    (dosync (alter world/world update-in [:players 2] assoc :x 15 :y 10)
            (alter world/world assoc-in [:objects [15 9]] :tree))
    (put-scoring-structures! (repeat 4 :blue))
    (let [start (server/process-message 2 {:type :gather-start :player-id 2 :attempt-id "active"})
          operation-id (:operation-id start)]
      (is (= :ok (:status start)))
      (is (= :ok (:status (build! 1 :sawmill [11 10]))))
      (is (empty? @world/gathering))
      (let [state (world/get-world)]
        (let [id (get-in state [:objects [11 10] :building-id])
              attack (server/process-message 2 (attack-command 2 [11 10] id 1))
              repair (server/process-message 1 (repair-command 1 [11 10] id 1))]
          (is (= :match-finished (:code attack)))
          (is (= state (:world attack)))
          (is (= :match-finished (:code repair)))
          (is (= state (:world repair))))
        (doseq [command [{:type :move :direction :down}
                         {:type :craft :tool :axe}
                         {:type :build :building-type :fence :position [10 11]}
                         {:type :build-check :building-type :fence :position [10 11]}
                         {:type :gather-start :attempt-id "new"}]]
          (is (= :match-finished (:code (server/process-message 1 (assoc command :player-id 1))))))
        (doseq [type [:gather-heartbeat :gather-finish :gather-cancel]]
          (is (= :match-finished (:code (server/process-message 2 {:type type :player-id 2
                                                                  :operation-id operation-id})))))
        (is (= :match-finished (:code (server/process-message 3 {:type :register :player-id 3 :name "Late"}))))
        (is (= :match-finished (world/restore-resource [8 8] :tree)))
        (auth/disconnect! 2)
        (is (= :ok (:status (server/process-message :reconnected
                              (merge (select-keys registration [:token :server-instance])
                                     {:type :login :player-id 2})))))
        (is (= {:status :ok :world state} (server/process-message {:type :state})))))))

(deftest simultaneous-payments-cannot-overdraw-test
  (player! 1 [10 10] {:wood 2})
  (is (= {:ok 1 :error 1} (frequencies (map :status (race [#(build! 1 :fence [11 10]) #(build! 1 :fence [10 11])])))))
  (is (zero? (:wood (items 1))))
  (is (= 1 (count (:objects (world/get-world))))))

(defn- open-enclosure! []
  (dosync
    (alter world/world assoc :objects
           (into {} (for [x (range 11 14) y (range 11 14)
                          :when (and (not= [x y] [12 12]) (not= [x y] [12 13]))]
                      [[x y] {:type :building :kind :fence :owner-id 1}])))))

(deftest empty-enclosure-is-allowed-test
  (player! 1 [12 14] {:wood 2}) (open-enclosure!)
  (is (= :ok (:status (build! 1 :fence [12 13])))))

(deftest registration-does-not-spawn-inside-empty-enclosure-test
  (player! 1 [12 14] {:wood 2}) (open-enclosure!)
  (build! 1 :fence [12 13])
  (with-redefs [layout/spawn-cells (fn [] [[12 12] [14 14]])]
    (is (true? (world/add-player 2 "B")))
    (is (= [14 14] ((juxt :x :y) (get-in (world/get-world) [:players 2]))))))

(deftest player-or-resource-cannot-be-enclosed-test
  (player! 1 [12 14] {:wood 10}) (open-enclosure!)
  (player! 2 [12 12] {}) (auth/disconnect! 2)
  (let [before (world/get-world)]
    (is (= :blocks-access (:code (build! 1 :fence [12 13]))))
    (is (= before (world/get-world))))
  (dosync (alter world/world update :players dissoc 2)
          (alter world/world assoc-in [:objects [12 12]] :tree))
  (let [before (world/get-world)]
    (is (= :blocks-access (:code (build! 1 :fence [12 13]))))
    (is (= before (world/get-world)))))

(deftest important-destination-cannot-be-enclosed-test
  (player! 1 [12 14] {:wood 10}) (open-enclosure!)
  (with-redefs [buildings/protected-cells #{[0 0] [12 12]}]
    (is (= :blocks-access (:code (build! 1 :fence [12 13]))))))

(deftest protected-areas-are-not-buildable-test
  (player! 1 [10 10] {:wood 10})
  (with-redefs [buildings/protected-cells #{[0 0] [11 10]}]
    (is (= :protected-area (:code (build! 1 :fence [11 10]))))))

(deftest preview-does-not-authorize-a-later-conflicting-build-test
  (player! 1 [10 10] {:wood 10}) (player! 2 [12 10] {:wood 10})
  (doseq [id [1 2]]
    (is (= :ok (:status (server/process-message id {:type :build-check :player-id id :building-type :fence :position [11 10]})))))
  (build! 1 :fence [11 10])
  (is (= :object-blocked (:code (build! 2 :fence [11 10]))))
  (is (= 10 (:wood (items 2)))))

(deftest cursor-transformation-with-camera-and-letterbox-test
  (let [camera {:x 1200 :y 800 :width 800 :height 600}]
    (is (= [20 15] (building-ui/screen-to-cell camera 800 600 112 192)))
    (is (= [20 15] (building-ui/screen-to-cell camera 1000 800 212 292)))
    (is (nil? (building-ui/screen-to-cell camera 1000 800 20 20)))
    (is (nil? (building-ui/screen-to-cell camera 800 600 800 100))))
  (let [camera {:x 1215.25 :y 831.25 :width 800 :height 600}]
    (is (= [18 12] (building-ui/screen-to-cell camera 800 600 0 0)))
    (is (= [19 13] (building-ui/screen-to-cell camera 800 600 1 1)))
    (is (= [18 12] (building-ui/screen-to-cell camera 1000 800 100 100)))
    (is (= [19 13] (building-ui/screen-to-cell (assoc camera :x 1215.75 :y 831.5)
                                              800 600 0 0)))))

(deftest built-house-renders-once-test
  (let [entry {:type :building :kind :house :owner-id 1}
        camera (rendering/camera {:x 12 :y 12} 800 600)]
    (with-redefs [rendering/decorations {[12 11] :bench}]
      (let [layers (rendering/scene-sprites {[11 10] entry} camera)]
        (is (= 1 (count layers)))
        (is (= :built-house (:kind (first layers))))))))

(defn- edt! [f]
  (let [result (promise)] (SwingUtilities/invokeAndWait #(deliver result (f))) @result))

(deftest building-action-controls-and-snapshots-test
  (edt!
   (bound-fn []
     (let [player-id 10101 player {:x 13 :y 11 :team :blue}
           panel (gui/create-map-panel 13 11 player-id)
           label (JLabel.)
           anchor [11 10]
           id (random-uuid)
           house {:type :building :kind :house :team :red :building-id id :hp 30 :max-hp 200}
           snapshot {:players {player-id player} :objects {anchor house [14 10] :house}}
           displayed (atom snapshot)
           applied! (fn [world]
                      (reset! displayed world)
                      (gui/update-map panel (:players world) (:objects world)))
           pending (atom [])
           camera (-> (rendering/camera player 800 600) (update :x + 40.5) (update :y + 12.25))
           point (fn [[x y]] [(int (- (+ (* x 64) 32) (:x camera)))
                             (int (- (+ (* y 64) 32) (:y camera)))])
           event (fn [cell type button]
                   (let [[x y] (point cell)]
                     (java.awt.event.MouseEvent. panel type 0 0 x y 1 false button)))
           hover! (fn [cell]
                    (doseq [listener (.getMouseMotionListeners panel)]
                      (.mouseMoved listener (event cell java.awt.event.MouseEvent/MOUSE_MOVED 0))))
           click! (fn [cell]
                    (doseq [listener (.getMouseListeners panel)]
                      (.mouseClicked listener (event cell java.awt.event.MouseEvent/MOUSE_CLICKED 1))))
           key! (fn [action] (.actionPerformed (.get (.getActionMap panel) action) nil))
           reply! (fn [response]
                    (let [[_ callback] (peek @pending)] (callback response)))]
       (.setSize panel 800 600)
       (applied! snapshot)
       (.putClientProperty panel :settlement.gui/camera camera)
       (with-redefs [client/async! (fn [_ _ message callback] (swap! pending conj [message callback]) true)]
         (let [controller (building-ui/install! panel {} player-id label applied! (fn [])
                                                {:attack-seq 8 :repair-seq 4})]
           (try
             (doseq [cell [[11 10] [12 10] [11 11] [12 11]]]
               (hover! cell)
               (is (= "Деревянный дом (красные) — 30/200 HP" (.getToolTipText panel))))
             (hover! [14 10])
             (is (nil? (.getToolTipText panel)))
             (key! "attack-mode")
             (is (= :attack (:mode @(:state controller))))
             (key! "attack-mode")
             (is (nil? @(:state controller)))
             ((:select! controller) :fence)
             (is (= :build (:mode @(:state controller))))
             (key! "attack-mode")
             (is (= :attack (:mode @(:state controller))))
             (click! [14 10])
             (is (empty? @pending))
             (is (= "Постройка не найдена." (.getText label)))
             (click! [12 11])
             (click! [12 11])
             (is (= 1 (count @pending)))
             (is (= (attack-command player-id [12 11] id 9) (first (peek @pending))))
             (reply! {:status :error :code :attack-cooldown})
             (is (re-find #"через секунду" (.getText label)))
             (hover! [12 11])
             (click! [12 11])
             (is (= (attack-command player-id [12 11] id 10) (first (peek @pending))))
             (reply! {:status :ok :hp 20 :world (assoc-in snapshot [:objects anchor :hp] 20)})
             (is (= "Деревянный дом (красные) — 20/200 HP" (.getToolTipText panel)))
             (key! "repair-mode")
             (is (= :repair (:mode @(:state controller))))
             (click! [11 11])
             (is (= (repair-command player-id [11 11] id 5) (first (peek @pending))))
             (reply! {:status :error :code :invalid-target-team})
             (is (= "Ремонтировать можно только союзные постройки." (.getText label)))
             (applied! (assoc-in @displayed [:objects anchor :team] :blue))
             (click! [11 11])
             (is (= (repair-command player-id [11 11] id 6) (first (peek @pending))))
             (reply! {:status :ok :restored-hp 10 :world (assoc-in @displayed [:objects anchor :hp] 30)})
             (is (= "Деревянный дом (синие) — 30/200 HP" (.getToolTipText panel)))
             (is (= "Восстановлено 10 HP." (.getText label)))
             ((:select! controller) :house)
             (is (= :build (:mode @(:state controller))))
             (finally ((:close! controller)))))
         (let [reopened (building-ui/install! panel {} player-id label applied! (fn [])
                                             {:attack-seq 9 :repair-seq 5})]
           (try
             (key! "attack-mode")
             (hover! [12 11])
             (click! [12 11])
             (is (= (attack-command player-id [12 11] id 11) (first (peek @pending))))
             (reply! {:status :ok :destroyed? true :world (assoc snapshot :objects {} :match world/initial-match)})
             (is (nil? (.getToolTipText panel)))
             (is (world/free-cell? @displayed anchor))
             (is (= {:blue 0 :red 0} (get-in @displayed [:match :scores])))
             (is (= "Постройка разрушена." (.getText label)))
             (finally ((:close! reopened))))))))))

(deftest asynchronous-preview-and-placement-at-offset-camera-test
  (player! 1 [10 10] {:wood 10})
  (let [queue (transport/create-queue #(server/process-message 1 %))
        panel (gui/create-map-panel 10 10 1) label (JLabel.)
        applied (promise) checked (promise)
        controller (edt! #(do (.setSize panel 800 600)
                             (gui/update-map panel (:players (world/get-world)) (:objects (world/get-world)))
                             (building-ui/install! panel {:queue queue} 1 label
                               (fn [state] (gui/update-map panel (:players state) (:objects state)) (deliver applied state))
                               (fn [] nil))))]
    (try
      (add-watch (:state controller) :test
                 (fn [_ _ _ state] (when (= :ok (get-in state [:result :status])) (deliver checked true))))
      (edt! #(do ((:select! controller) :fence)
                 (let [camera (rendering/camera {:x 10 :y 10} 800 600)]
                   (reset! (:cursor controller) [(- (+ (* 11 64) 32) (:x camera))
                                                 (- (+ (* 10 64) 32) (:y camera))]))
                 ((:check! controller))))
      (is (= true (deref checked 5000 :timeout)))
      (edt! (:place! controller))
      (let [state (deref applied 5000 ::timeout)]
        (is (not= ::timeout state))
        (is (= :fence (get-in state [:objects [11 10] :kind])))
        (is (= 8 (get-in state [:players 1 :inventory :items :wood]))))
      (edt! (:cancel! controller))
      (is (nil? @(:state controller)))
      (finally (remove-watch (:state controller) :test)
               (edt! (:close! controller)) (transport/stop! queue)))))

(deftest cancelled-preview-response-cannot-reactivate-placement-test
  (player! 1 [10 10] {:wood 10})
  (let [entered (promise) release (promise) answered (promise)
        queue (transport/create-queue (fn [message] (deliver entered true) @release
                                        (let [result (server/process-message 1 message)] (deliver answered true) result)))
        panel (gui/create-map-panel 10 10 1)
        controller (edt! #(do (.setSize panel 800 600)
                             (gui/update-map panel (:players (world/get-world)) {})
                             (building-ui/install! panel {:queue queue} 1 (JLabel.) identity (fn [] nil))))]
    (try
      (edt! #(do ((:select! controller) :fence) (reset! (:cursor controller) [464 295]) ((:check! controller))))
      (is (= true (deref entered 5000 :timeout)))
      (edt! (:cancel! controller))
      (deliver release true)
      (is (= true (deref answered 5000 :timeout)))
      (edt! (fn [] nil))
      (is (nil? @(:state controller)))
      (is (empty? (:objects (world/get-world))))
      (finally (deliver release true) (edt! (:close! controller)) (transport/stop! queue)))))

(deftest preview-colors-and-footprint-use-camera-test
  (doseq [[status expected] [[:ok (java.awt.Color. 85 210 110)] [:error (java.awt.Color. 235 80 65)]]]
    (let [image (BufferedImage. 800 600 BufferedImage/TYPE_INT_ARGB)
          graphics (.createGraphics image) camera {:x 640 :y 640 :width 800 :height 600}]
      (try
        (building-ui/draw-preview! graphics camera 0 0 :house [11 11] status)
        (is (= (.getRGB expected) (.getRGB image 64 64)))
        (is (= (.getRGB expected) (.getRGB image 128 128)))
        (finally (.dispose graphics))))))

(deftest buildings-synchronize-between-real-tcp-clients-test
  (with-open [listener (ServerSocket. 0)]
    (let [connections (atom [])
          open! (fn []
                  (let [socket (Socket. "127.0.0.1" (.getLocalPort listener))
                        accepted (.accept listener) handler (future (server/handle-client accepted))
                        connection {:socket socket
                                    :reader (java.io.BufferedReader. (java.io.InputStreamReader. (.getInputStream socket) "UTF-8"))
                                    :writer (java.io.BufferedWriter. (java.io.OutputStreamWriter. (.getOutputStream socket) "UTF-8"))}]
                    (.setSoTimeout socket 5000) (swap! connections conj [connection handler]) connection))]
      (try
        (let [a (open!) b (open!)]
          (doseq [[connection id position] [[a 1 [10 10]] [b 2 [12 10]]]]
            (is (= :ok (:status (client/request connection {:type :register :player-id id :name (str id) :client-kind (if (= id 2) :prolog :human)}))))
            (dosync (alter world/world update-in [:players id] assoc :x (first position) :y (second position)))
            (world/change-player-items! id {:wood 10}))
          (is (= :ok (:status (client/request a {:type :build :player-id 1 :building-type :fence :position [11 10]}))))
          (let [state (:world (client/request b {:type :state}))]
            (is (= state (:world (client/request a {:type :state}))))
            (is (= {:blue 0 :red 0} (get-in state [:match :scores])))
            (is (= [:blue :red] (mapv #(get-in state [:players % :team]) [1 2])))
            (let [building (get-in state [:objects [11 10]])]
              (is (uuid? (:building-id building)))
              (is (= {:type :building :kind :fence :owner-id 1 :team :blue :hp 50 :max-hp 50}
                     (dissoc building :building-id)))))
          (is (= :object-blocked (:code (client/request b {:type :move :player-id 2 :direction :left}))))
          (let [id (get-in (world/get-world) [:objects [11 10] :building-id])
                clock (atom 0)]
            (with-redefs [world/now-ns #(deref clock)]
              (is (= 40 (:hp (client/request b (attack-command 2 [11 10] id 1)))))
              (let [repaired (client/request a (repair-command 1 [11 10] id 1))]
                (is (= {:hp 50 :restored-hp 10} (select-keys repaired [:hp :restored-hp])))
                (is (= 7 (get-in repaired [:world :players 1 :inventory :items :wood])))
                (is (= (:world repaired) (:world (client/request b {:type :state}))))
                (is (= repaired (client/request a (repair-command 1 [11 10] id 1)))))
              (swap! clock + 1000000000)
              (doseq [n (range 2 7)]
                (let [result (client/request b (attack-command 2 [11 10] id n))]
                  (is (= (- 50 (* 10 (dec n))) (:hp result)))
                  (is (= (:world result) (:world (client/request a {:type :state})))))
                (swap! clock + 1000000000)))
            (let [state (:world (client/request b {:type :state}))]
              (is (= state (:world (client/request a {:type :state}))))
              (is (nil? (get-in state [:objects [11 10]])))
              (is (= {:blue 0 :red 0} (get-in state [:match :scores]))))
            (let [request {:type :state :include-building-sequences true :player-id 2}]
              (is (= {:attack-seq 0 :repair-seq 1} (:building-sequences (client/request a request))))
              (is (= {:attack-seq 6 :repair-seq 0} (:building-sequences (client/request b request))))
              (is (nil? (:building-sequences (client/request (open!) request))))
              (is (nil? (:building-sequences (client/request a {:type :state})))))
            (is (= :ok (:status (client/request b {:type :move :player-id 2 :direction :left}))))))
        (finally
          (doseq [[connection handler] @connections]
            (client/close-connection connection) (deref handler 5000 nil) (future-cancel handler)))))))

(deftest neighboring-fences-connect-visually-without-extra-state-test
  (let [fence {:type :building :kind :fence :owner-id 1}
        objects {[10 10] fence [11 10] fence [10 11] fence}]
    (is (= 6 (rendering/fence-connections objects [10 10])))
    (is (= 8 (rendering/fence-connections objects [11 10])))
    (is (= 1 (rendering/fence-connections objects [10 11])))
    (is (every? #(= fence %) (vals objects)))))

(def production-protected-cells buildings/protected-cells)

(deftest new-structures-retain-damage-and-lose-points-only-on-destruction-test
  (doseq [[kind cost hp points size] [[:watchtower {:wood 10 :stone 8} 150 10 [2 1]]
                                      [:sawmill {:wood 20 :stone 10} 240 20 [2 2]]
                                      [:mine {:wood 10 :stone 20} 260 20 [2 1]]]]
    (testing (name kind)
      (with-redefs [world/world (ref {:players {} :objects {[24 24] :sawmill}
                                     :resources {:wood 0 :stone 0} :match world/initial-match})
                    auth/state (ref (auth/new-state))]
        (let [registration (server/process-message 1 {:type :register :client-kind :human
                                                       :player-id 1 :name "Owner"})
              anchor [11 10]
              clock (atom 0)]
          (dosync (alter world/world update-in [:players 1] assoc :x 10 :y 10))
          (world/change-player-items! 1 (update cost :wood inc))
          (player! 2 [13 10] {})
          (let [result (build! 1 kind anchor)
                entry (:building result)
                uuid (:building-id entry)
                placement (rendering/building-placement anchor entry)]
            (is (= :ok (:status result)))
            (is (= [hp hp] ((juxt :hp :max-hp) entry)))
            (is (= kind (:kind placement)))
            (is (= (* (first size) (second size)) (count (buildings/cells anchor entry))))
            (is (every? #(not (world/free-cell? (world/get-world) %)) (buildings/cells anchor entry)))
            (is (= {:wood 1 :stone 0 :axe 0 :pickaxe 0} (items 1)))
            (is (= points (get-in (world/get-world) [:match :scores :blue])))
            (with-redefs [world/now-ns #(deref clock)]
              (is (= (- hp 10) (:hp (server/process-message 2 (attack-command 2 anchor uuid 1)))))
              (dotimes [_ 3]
                (rendering/render-scene {} (:objects (world/get-world))
                                        (rendering/camera {:x 11 :y 10} 320 240)))
              (world/migrate-building-durability!)
              (auth/disconnect! 1)
              (is (= :ok (:status (server/process-message :owner
                                     (assoc (select-keys registration [:token :server-instance])
                                            :type :login :player-id 1)))))
              (let [snapshot (server/process-message :owner {:type :state})]
                (is (= (- hp 10) (get-in snapshot [:world :objects anchor :hp])))
                (is (= points (get-in snapshot [:world :match :scores :blue]))))
              (let [repair (repair-command 1 anchor uuid 1)
                    repaired (server/process-message :owner repair)]
                (is (= hp (:hp repaired)))
                (is (= repaired (server/process-message :owner repair)))
                (is (zero? (:wood (items 1)))))
              (doseq [sequence (range 2 (+ 2 (quot hp 10)))]
                (swap! clock + 1000000000)
                (is (= :ok (:status (server/process-message 2
                                       (attack-command 2 anchor uuid sequence))))))
              (is (nil? (get-in (world/get-world) [:objects anchor])))
              (is (zero? (get-in (world/get-world) [:match :scores :blue])))
              (is (= :invalid-resource (world/restore-resource anchor entry)))
              (is (= :building-not-found (:code (server/process-message :owner
                                                  (repair-command 1 anchor uuid 2)))))
              (is (nil? (get-in (world/get-world) [:objects anchor]))))))))))

(deftest bot-cooldowns-apply-to-raw-tcp-and-survive-login-test
  (let [clock (atom 0)
        clients (atom [])]
    (with-redefs [world/now-ns #(deref clock)
                  config/bot-timing-ms {:move 350 :action 700 :after-build 2000}]
      (with-open [listener (ServerSocket. 0)]
        (letfn [(connect! []
                  (let [socket (doto (Socket. "127.0.0.1" (.getLocalPort listener)) (.setSoTimeout 5000))
                        handler (future (server/handle-client (.accept listener)))
                        connection {:socket socket
                                    :reader (java.io.BufferedReader. (java.io.InputStreamReader.
                                                                      (.getInputStream socket) "UTF-8"))
                                    :writer (java.io.BufferedWriter. (java.io.OutputStreamWriter.
                                                                      (.getOutputStream socket) "UTF-8"))}]
                    (swap! clients conj [connection handler])
                    [connection handler]))
                (advance! [ms] (swap! clock + (* ms 1000000)))]
          (try
            (let [[bot bot-handler] (connect!)
                  [human _] (connect!)
                  registration (client/request bot {:type :register :client-kind :prolog :player-id 2 :name "Bot"})
                  move {:type :move :player-id 2 :direction :right}
                  axe {:type :craft :player-id 2 :tool :axe}
                  pickaxe (assoc axe :tool :pickaxe)
                  build {:type :build :player-id 2 :building-type :watchtower :position [12 11]}]
              (is (= :ok (:status registration)))
              (is (= :ok (:status (client/request human {:type :register :player-id 1 :name "Human"}))))
              (dosync (alter world/world update-in [:players 2] assoc :x 10 :y 10))
              (world/change-player-items! 2 {:wood 15 :stone 13})
              (is (= :ok (:status (client/request bot move))))
              (advance! 349)
              (is (= :bot-cooldown (:code (client/request bot move))))
              (advance! 1)
              (is (= :ok (:status (client/request bot move))))
              (is (= :ok (:status (client/request bot axe))))
              (advance! 699)
              (is (= :bot-cooldown (:code (client/request bot pickaxe))))
              (advance! 1)
              (is (= :ok (:status (client/request bot pickaxe))))
              (is (= :bot-cooldown (:code (client/request bot build))))
              (advance! 700)
              (is (= :ok (:status (client/request bot build))))
              (client/close-connection bot)
              (deref bot-handler 5000 nil)
              (let [[reconnected _] (connect!)
                    login (assoc (select-keys registration [:token :server-instance])
                                 :type :login :player-id 2)]
                (is (= :ok (:status (client/request reconnected login))))
                (advance! 1999)
                (is (= :bot-cooldown (:code (client/request reconnected move))))
                (dotimes [_ 2]
                  (is (= :ok (:status (client/request human (assoc move :player-id 1))))))
                (advance! 1)
                (is (= :ok (:status (client/request reconnected move))))
                (let [anchor (:position build)
                      id (get-in (world/get-world) [:objects anchor :building-id])
                      repair (repair-command 2 anchor id 1)]
                  (dosync (alter world/world assoc-in [:objects anchor :hp] 140))
                  (world/change-player-items! 2 {:wood 1})
                  (let [result (client/request reconnected repair)]
                    (is (= 150 (:hp result)))
                    (is (= result (client/request reconnected repair)))
                    (is (= :bot-cooldown (:code (client/request reconnected (assoc repair :repair-seq 2)))))
                    (is (zero? (:wood (items 2))))))))
            (finally
              (doseq [[connection handler] @clients]
                (client/close-connection connection)
                (deref handler 5000 nil)
                (future-cancel handler)))))))))

(deftest build-on-production-map-preserves-settlement-and-access-test
  (player! 1 [24 29] {:wood 15 :stone 10})
  (dosync (alter world/world assoc :objects layout/initial-objects))
  (with-redefs [buildings/protected-cells production-protected-cells
                layout/spawn-position [23 25]]
    (is (= :ok (:status (build! 1 :house [25 29]))))
    (is (buildings/access-safe? (world/get-world) (:objects (world/get-world))))
    (is (every? #(= :house (get-in (world/get-world) [:objects %])) layout/house-positions))
    (is (= layout/initial-objects (select-keys (:objects (world/get-world)) (keys layout/initial-objects))))))
