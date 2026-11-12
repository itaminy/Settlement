(ns settlement.world-test
  (:require [clojure.test :refer :all]
            [settlement.server :as server]
            [settlement.auth :as auth]
            [settlement.auth-test]
            [settlement.rendering-test]
            [settlement.gui :as gui]
            [settlement.world :as world]))

;; Запуск: clojure -M -m settlement.world-test fast (или slow, all).
(def ^:private initial-world
  {:resources {:wood 0 :stone 0}
   :players {}
   :objects {[3 4] :tree [7 2] :stone [8 6] :house [5 5] :tree}})

(def ^:private real-schedule-restore world/schedule-resource-restore)

(use-fixtures :each
  (fn [test-fn]
    ;; Исходный ref не меняется. Быстрые тесты не оставляют фоновых задач.
    (with-redefs [world/world (ref initial-world)
                  auth/state (ref (auth/new-state))
                  world/schedule-resource-restore (fn [_ _] nil)]
      (test-fn))))

(defn- place-player! [id x y]
  (world/add-player id (str "Player " id))
  (dosync
    (alter world/world update-in [:players id] assoc :x x :y y)))

(defn- neighbors [[x y]]
  (filter (fn [[x y]] (and (<= 0 x 11) (<= 0 y 11)))
          [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]))

(deftest initial-map-layout-test
  (let [objects world/initial-objects
        ;; Проверяем доступ даже без прохода через объекты.
        reachable (loop [visited #{[0 0]} frontier #{[0 0]}]
                    (let [next-cells (set (for [position frontier
                                                neighbor (neighbors position)
                                                :when (and (not (contains? objects neighbor))
                                                           (not (contains? visited neighbor)))]
                                            neighbor))]
                      (if (empty? next-cells) visited
                          (recur (into visited next-cells) next-cells))))]
    (is (= {:tree 18 :stone 12 :house 3} (frequencies (vals objects))))
    (is (= 33 (count objects)))
    (is (not (contains? objects [0 0])))
    (is (= gui/map-size 12))
    (is (every? (fn [[x y]] (and (<= 0 x 11) (<= 0 y 11))) (keys objects)))
    (is (= (- (* 12 12) (count objects)) (count reachable)))
    (doseq [[position object] objects :when (#{:tree :stone} object)]
      (is (some reachable (neighbors position)) (str "Resource accessible at " position)))))

(deftest registration-test
  (world/add-player 1 "Alice")
  (world/add-player 2 "Bob")
  (is (= {1 {:name "Alice" :x 0 :y 0 :collected {:wood 0 :stone 0}}
          2 {:name "Bob" :x 1 :y 0 :collected {:wood 0 :stone 0}}}
         (:players (world/get-world))))
  (is (= (dissoc initial-world :players)
         (dissoc (world/get-world) :players))))

(deftest movement-test
  (place-player! 1 5 4)
  (doseq [[direction position] [[:up [5 3]] [:right [6 3]]
                              [:down [6 4]] [:left [5 4]]]]
    (is (true? (world/move-player 1 direction)))
    (is (= position ((juxt :x :y) (get-in (world/get-world) [:players 1])))))
  (is (= (:objects initial-world) (:objects (world/get-world))))
  (is (= {:wood 0 :stone 0} (:resources (world/get-world)))))

(deftest map-boundaries-test
  (doseq [[x y direction] [[0 0 :up] [0 0 :left]
                           [11 11 :down] [11 11 :right]]]
    (place-player! 1 x y)
    (let [before (world/get-world)]
      (is (nil? (world/move-player 1 direction)))
      (is (= before (world/get-world))))))

(deftest unknown-player-test
  (is (nil? (world/move-player 99 :right)))
  (is (nil? (world/collect-resource 99 :wood 5)))
  (is (= {:status :error :message "Player not found"}
         (world/gather-resource 99)))
  (is (= initial-world (world/get-world))))

(deftest resource-accounting-test
  (world/add-player 1 "Alice")
  (world/add-player 2 "Bob")
  (is (true? (world/collect-resource 1 :wood 5)))
  (is (true? (world/collect-resource 1 :stone 2)))
  (is (true? (world/collect-resource 2 :wood 3)))
  (is (= {:wood 8 :stone 2} (:resources (world/get-world))))
  (is (= {:wood 5 :stone 2} (get-in (world/get-world) [:players 1 :collected])))
  (is (= {:wood 3 :stone 0} (get-in (world/get-world) [:players 2 :collected])))
  (is (= (:objects initial-world) (:objects (world/get-world)))))

(deftest network-collect-rejected-test
  ;; Проверяем отказ и рядом с ресурсом, и при ненулевом запасе.
  (place-player! 1 2 4)
  (world/collect-resource 1 :wood 5)
  (world/collect-resource 1 :stone 3)
  (doseq [resource [:wood :stone]
          amount [1 0 -1 -1000000000 1000000000 999999999999999999999999N]]
    (testing (str resource " amount=" amount)
      (let [before (world/get-world)
            response (server/process-message
                      {:type :collect :player-id 1
                       :resource resource :amount amount})]
        (is (= :error (:status response)))
        (is (= before (world/get-world)))))))

(deftest network-gather-test
  (doseq [[id x y position resource object]
          [[1 2 4 [3 4] :wood :tree] [2 6 2 [7 2] :stone :stone]]]
    (place-player! id x y)
    (dosync (alter auth/state assoc-in [:sessions id] id))
    (let [before (world/get-world)
          scheduled (atom [])]
      (with-redefs [world/schedule-resource-restore
                    (fn [pos obj] (swap! scheduled conj [pos obj]))]
        ;; Количество и тип добычи определяет сервер, даже с лишними полями.
        (is (= {:status :ok :resource resource :amount 1 :position position}
               (server/process-message id
                {:type :gather :player-id id :resource :gold :amount 1000000000})))
        (is (= (update (:resources before) resource inc)
               (:resources (world/get-world))))
        (is (= {:wood (if (= resource :wood) 1 0)
                :stone (if (= resource :stone) 1 0)}
               (get-in (world/get-world) [:players id :collected])))
        (is (= (dissoc (:objects before) position) (:objects (world/get-world))))
        (is (= [[position object]] @scheduled))
        (let [after (world/get-world)]
          (is (= :error (:status (server/process-message id {:type :gather :player-id id}))))
          (is (= after (world/get-world)))
          (is (= 1 (count @scheduled)))))))
  (place-player! 3 0 0)
  (let [before (world/get-world)]
    (is (= :error (:status (server/process-message {:type :gather :player-id 3}))))
    (is (= before (world/get-world)))))

(deftest gather-tree-and-stone-test
  (doseq [[position resource player-position]
          [[[3 4] :wood [2 4]] [[7 2] :stone [6 2]]]]
    (let [[x y] player-position
          scheduled (atom [])]
      (place-player! 1 x y)
      (with-redefs [world/schedule-resource-restore
                    (fn [pos object] (swap! scheduled conj [pos object]))]
        (let [before (world/get-world)
              object (get-in before [:objects position])]
          (is (= {:status :ok :resource resource :amount 1 :position position}
                 (world/gather-resource 1)))
          (is (= (update (:resources before) resource inc)
                 (:resources (world/get-world))))
          (is (= (update (get-in before [:players 1 :collected]) resource inc)
                 (get-in (world/get-world) [:players 1 :collected])))
          (is (= (dissoc (:objects before) position) (:objects (world/get-world))))
          (is (= [[position object]] @scheduled))
          (let [after (world/get-world)]
            (is (= {:status :error :message "No resource nearby"}
                   (world/gather-resource 1)))
            (is (= after (world/get-world)))
            (is (= 1 (count @scheduled)))))))))

(deftest no-adjacent-resource-test
  ;; Диагональ, объект под игроком и дом не подходят для добычи.
  (doseq [[x y] [[0 0] [2 3] [3 4] [8 5]]]
    (place-player! 1 x y)
    (let [before (world/get-world)]
      (is (= {:status :error :message "No resource nearby"}
             (world/gather-resource 1)))
      (is (= before (world/get-world))))))

(deftest restore-resource-test
  (dosync (alter world/world update :objects dissoc [3 4]))
  (world/restore-resource [3 4] :tree)
  (is (= initial-world (world/get-world)))
  (world/restore-resource [3 4] :stone)
  (is (= :tree (get-in (world/get-world) [:objects [3 4]]))))

(defn- concurrently [actions]
  (let [ready (java.util.concurrent.CountDownLatch. (count actions))
        start (promise)
        tasks (mapv #(future (.countDown ready)
                             (when (= :go (deref start 5000 :timeout)) (%))) actions)]
    (try
      (is (.await ready 5 java.util.concurrent.TimeUnit/SECONDS))
      (deliver start :go)
      (mapv #(deref % 5000 ::timeout) tasks)
      (finally
        (deliver start :cancel)
        (doseq [task tasks] (future-cancel task))))))

(deftest object-collisions-and-server-response-test
  (doseq [object [:tree :stone :house]]
    (dosync (alter world/world assoc :objects {[2 2] object}))
    (place-player! 1 1 2)
    (dosync (alter auth/state assoc-in [:sessions 1] :a))
    (let [before (world/get-world)
          response (server/process-message :a {:type :move :player-id 1 :direction :right})]
      (is (= :error (:status response)))
      (is (= :object-blocked (:code response)))
      (is (seq (:message response)))
      (is (= before (world/get-world))))))

(deftest movement-rejection-reasons-test
  (place-player! 1 0 0)
  (let [before (world/get-world)]
    (is (= :out-of-bounds (:code (world/move-player-result 1 :up))))
    (is (= :invalid-direction (:code (world/move-player-result 1 :teleport))))
    (is (= :player-not-found (:code (world/move-player-result 99 :right))))
    (is (= before (world/get-world)))))

(deftest player-collision-and-offline-cell-test
  (place-player! 1 1 1)
  (place-player! 2 2 1)
  (auth/disconnect! :offline)
  (let [before (world/get-world)]
    (is (= :player-blocked (:code (world/move-player-result 1 :right))))
    (is (= before (world/get-world))))
  (is (true? (world/move-player 2 :down)))
  (is (true? (world/move-player 1 :right))))

(deftest concurrent-movement-test
  (place-player! 1 1 1)
  (place-player! 2 3 1)
  (let [results (concurrently [#(world/move-player-result 1 :right)
                               #(world/move-player-result 2 :left)])
        players (:players (world/get-world))]
    (is (= {:ok 1 :error 1} (frequencies (map :status results))))
    (is (= :player-blocked (:code (first (filter #(= :error (:status %)) results)))))
    (is (= 1 (count (filter #(= [2 1] [(:x %) (:y %)]) (vals players)))))
    (is (= 2 (count (set (map (juxt :x :y) (vals players))))))))

(deftest spawn-search-in-row-order-test
  (dosync (alter world/world assoc :objects {[0 0] :tree [1 0] :stone [2 0] :house}))
  (doseq [id (range 1 15)] (is (true? (world/add-player id (str id)))))
  (is (= (vec (take 14 (for [y (range 12) x (range 12)
                             :when (not (#{[0 0] [1 0] [2 0]} [x y]))] [x y])))
         (mapv #(let [p (get-in (world/get-world) [:players %])] [(:x p) (:y p)]) (range 1 15)))))

(deftest gathered-cell-becomes-passable-test
  (place-player! 1 2 4)
  (place-player! 2 0 0)
  (is (nil? (world/move-player 1 :right)))
  (is (= :ok (:status (world/gather-resource 1))))
  (is (true? (world/move-player 1 :right)))
  (is (= [3 4] ((juxt :x :y) (get-in (world/get-world) [:players 1]))))
  (is (= {:wood 1 :stone 0} (get-in (world/get-world) [:players 1 :collected])))
  (is (= {:wood 0 :stone 0} (get-in (world/get-world) [:players 2 :collected]))))

(deftest gathering-from-four-neighbors-only-test
  (doseq [object [:tree :stone]
          position [[1 2] [3 2] [2 1] [2 3]]]
    (dosync (alter world/world assoc :objects {[2 2] object}))
    (apply place-player! 1 position)
    (is (= :ok (:status (world/gather-resource 1)))))
  (doseq [position [[1 1] [3 1] [1 3] [3 3] [0 2] [2 0] [4 2] [2 4]]]
    (dosync (alter world/world assoc :objects {[2 2] :tree}))
    (apply place-player! 1 position)
    (let [before (world/get-world)]
      (is (= :error (:status (world/gather-resource 1))))
      (is (= before (world/get-world))))))

(deftest decorations-and-large-sprites-do-not-block-test
  (dosync (alter world/world assoc :objects {[3 3] :house}))
  (place-player! 1 1 2)
  (is (true? (world/move-player 1 :right)))
  ;; Логический дом в [3 3] не блокирует соседнюю клетку [3 2].
  (is (true? (world/move-player 1 :right)))
  (doseq [position (keys settlement.rendering/decorations)]
    (dosync (alter world/world assoc :objects {}))
    (let [[x y] position]
      (if (pos? x)
        (do (place-player! 1 (dec x) y) (is (true? (world/move-player 1 :right))))
        (do (place-player! 1 1 y) (is (true? (world/move-player 1 :left))))))))

(deftest safe-restore-and-retry-test
  (place-player! 1 2 4)
  (world/gather-resource 1)
  (world/move-player 1 :right)
  (is (= :occupied (world/restore-resource [3 4] :tree)))
  (is (nil? (get-in (world/get-world) [:objects [3 4]])))
  (let [delays (atom [])]
    (with-redefs [world/wait-for-restore
                  (fn [milliseconds]
                    (swap! delays conj milliseconds)
                    (when (= 3 (count @delays)) (world/move-player 1 :right)))]
      (let [task (real-schedule-restore [3 4] :tree)]
        (try
          (is (not= ::timeout (deref task 5000 ::timeout)))
          (is (= [30000 1000 1000] @delays))
          (is (= :tree (get-in (world/get-world) [:objects [3 4]])))
          (is (= :exists (world/restore-resource [3 4] :stone)))
          (is (= :tree (get-in (world/get-world) [:objects [3 4]])))
          (finally (future-cancel task)))))))

(deftest concurrent-restoration-test
  (dosync (alter world/world update :objects dissoc [3 4]))
  (is (= {:restored 1 :exists 1}
         (frequencies (concurrently [#(world/restore-resource [3 4] :tree)
                                     #(world/restore-resource [3 4] :tree)]))))
  (is (= :tree (get-in (world/get-world) [:objects [3 4]]))))

(deftest restoration-racing-with-movement-test
  (dosync (alter world/world update :objects dissoc [3 4]))
  (place-player! 1 2 4)
  (let [[movement restoration] (concurrently [#(world/move-player-result 1 :right)
                                            #(world/restore-resource [3 4] :tree)])
        state (world/get-world)]
    (if (= :ok (:status movement))
      (do (is (= :occupied restoration))
          (is (nil? (get-in state [:objects [3 4]]))))
      (do (is (= :object-blocked (:code movement)))
          (is (= :restored restoration))
          (is (= :tree (get-in state [:objects [3 4]])))))))

(deftest concurrent-gather-test
  (place-player! 1 2 4)
  (place-player! 2 4 4)
  (let [ready (java.util.concurrent.CountDownLatch. 2)
        start (promise)
        scheduled (atom [])]
    (with-redefs [world/schedule-resource-restore
                  (fn [pos object] (swap! scheduled conj [pos object]))]
      (let [tasks (mapv (fn [id]
                          (future
                            (.countDown ready)
                            (when (= :go (deref start 5000 :timeout))
                              (world/gather-resource id))))
                        [1 2])]
        (try
          (is (.await ready 5 java.util.concurrent.TimeUnit/SECONDS))
          (deliver start :go)
          (let [results (mapv #(deref % 5000 :timeout) tasks)
                state (world/get-world)]
            (is (= {:ok 1 :error 1} (frequencies (map :status results))))
            (is (= {:wood 1 :stone 0} (:resources state)))
            (is (= [0 1] (sort (map #(get-in state [:players % :collected :wood]) [1 2]))))
            (is (every? zero? (map #(get-in state [:players % :collected :stone]) [1 2])))
            (is (= (dissoc (:objects initial-world) [3 4]) (:objects state)))
            (is (= [[[3 4] :tree]] @scheduled)))
          (finally
            (deliver start :cancel)
            (doseq [task tasks] (future-cancel task))))))))

(deftest ^:slow scheduled-restore-after-30-seconds-test
  (place-player! 1 2 4)
  (place-player! 2 6 2)
  (let [tasks (atom [])
        started (System/nanoTime)]
    (with-redefs [world/schedule-resource-restore
                  (fn [pos object]
                    (let [task (real-schedule-restore pos object)]
                      (swap! tasks conj task)
                      task))]
      (try
        (is (= :ok (:status (world/gather-resource 1))))
        (is (= :ok (:status (world/gather-resource 2))))
        (is (nil? (get-in (world/get-world) [:objects [3 4]])))
        (is (nil? (get-in (world/get-world) [:objects [7 2]])))
        (is (= :error (:status (world/gather-resource 1))))
        (is (= :error (:status (world/gather-resource 2))))
        (is (= 2 (count @tasks)))
        (is (true? (world/move-player 1 :right)))
        ;; Камень восстановится через 30 секунд; дерево ждёт ухода игрока.
        (is (not= ::timeout (deref (second @tasks) 35000 ::timeout)))
        (is (nil? (get-in (world/get-world) [:objects [3 4]])))
        (is (not (realized? (first @tasks))))
        (is (true? (world/move-player 1 :left)))
        ;; Ждём завершения настоящей задачи, а не подменяем 30-секундный таймер.
        (doseq [task @tasks]
          (is (not= ::timeout (deref task 35000 ::timeout))))
        (is (>= (/ (- (System/nanoTime) started) 1e9) 30.0))
        (is (= :tree (get-in (world/get-world) [:objects [3 4]])))
        (is (= :stone (get-in (world/get-world) [:objects [7 2]])))
        (is (= {:wood 1 :stone 1} (:resources (world/get-world))))
        (is (= {:wood 1 :stone 0} (get-in (world/get-world) [:players 1 :collected])))
        (is (= {:wood 0 :stone 1} (get-in (world/get-world) [:players 2 :collected])))
        (is (= :ok (:status (world/gather-resource 1))))
        (is (= :ok (:status (world/gather-resource 2))))
        (is (= {:wood 2 :stone 2} (:resources (world/get-world))))
        (is (= {:wood 2 :stone 0} (get-in (world/get-world) [:players 1 :collected])))
        (is (= {:wood 0 :stone 2} (get-in (world/get-world) [:players 2 :collected])))
        (finally
          (doseq [task @tasks] (future-cancel task)))))))

(defn -main [& [suite]]
  (let [suite (or suite "fast")]
    (when-not (#{"fast" "slow" "all"} suite)
      (throw (ex-info "Expected fast, slow or all" {:suite suite})))
    (try
      (let [tests (->> ["settlement.world-test" "settlement.auth-test" "settlement.rendering-test"]
                       (mapcat #(vals (ns-interns (symbol %))))
                       (filter #(-> % meta :test))
                       (filter #(case suite
                                  "fast" (not (:slow (meta %)))
                                  "slow" (:slow (meta %))
                                  "all" true))
                       (sort-by #(-> % meta :name)))
            result (binding [*report-counters* (ref *initial-report-counters*)]
                     (test-vars tests)
                     (let [result @*report-counters*]
                       (do-report (assoc result :type :summary))
                       result))]
        (when (pos? (+ (:fail result) (:error result)))
          (System/exit 1)))
      (finally (shutdown-agents)))))
