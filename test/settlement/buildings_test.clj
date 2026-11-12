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
            [settlement.rendering :as rendering]
            [settlement.gui :as gui]
            [settlement.test-support :as support])
  (:import [javax.swing SwingUtilities JLabel]
           [java.net ServerSocket Socket]
           [java.awt.image BufferedImage]
           [java.util.concurrent CountDownLatch TimeUnit]))

(use-fixtures :each
  (fn [f]
    (with-redefs [world/world (ref {:players {} :objects {} :resources {:wood 0 :stone 0}})
                  world/gathering (ref {}) auth/state (ref (auth/new-state))
                  layout/spawn-position [0 0] layout/spawn-cells support/row-major-spawn
                  buildings/protected-cells #{[0 0]}]
      (f))))

(defn- player! [id position stock]
  (server/process-message id {:type :register :player-id id :name (str id)})
  (let [[x y] position] (dosync (alter world/world update-in [:players id] assoc :x x :y y)))
  (world/change-player-items! id stock))
(defn- build! [id kind position]
  (server/process-message id {:type :build :player-id id :building-type kind :position position}))
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
  (is (= :ok (:status (build! 1 :house [11 10]))))
  (is (= {:wood 13 :stone 10 :axe 0 :pickaxe 0} (items 1)))
  (let [objects (:objects (world/get-world))]
    (is (= 2 (count objects)))
    (is (= {:type :building :kind :house :owner-id 1} (objects [11 10])))
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
                                 [:house [49 49] :out-of-bounds] [:fence [-1 10] :out-of-bounds]
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
  (is (= :ok (:status (server/process-message 1 {:type :build :player-id 1 :building-type :fence :position [11 10]
                                                 :cost {:wood 0} :owner-id 99 :size [0 0]}))))
  (is (= 8 (:wood (items 1))))
  (is (= 1 (get-in (world/get-world) [:objects [11 10] :owner-id]))))

(deftest concurrent-building-and-spending-test
  (player! 1 [10 10] {:wood 2}) (player! 2 [12 10] {:wood 2})
  (is (= {:ok 1 :error 1} (frequencies (map :status (race [#(build! 1 :fence [11 10]) #(build! 2 :fence [11 10])])))))
  (is (= 1 (count (:objects (world/get-world)))))
  (is (= [0 2] (sort [(:wood (items 1)) (:wood (items 2))]))))

(deftest overlapping-houses-cannot-both-commit-test
  (player! 1 [10 10] {:wood 15 :stone 10}) (player! 2 [14 10] {:wood 15 :stone 10})
  (is (= {:ok 1 :error 1} (frequencies (map :status (race [#(build! 1 :house [11 10]) #(build! 2 :house [12 10])])))))
  (is (= 4 (count (buildings/occupied-cells (:objects (world/get-world)))))))

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
    (is (nil? (building-ui/screen-to-cell camera 800 600 800 100)))))

(deftest built-house-renders-once-test
  (let [entry {:type :building :kind :house :owner-id 1}
        camera (rendering/camera {:x 12 :y 12} 800 600)]
    (with-redefs [rendering/decorations {[12 11] :bench}]
      (let [layers (rendering/scene-sprites {[11 10] entry} camera)]
        (is (= 1 (count layers)))
        (is (= :built-house (:kind (first layers))))))))

(defn- edt! [f]
  (let [result (promise)] (SwingUtilities/invokeAndWait #(deliver result (f))) @result))

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
            (is (= :ok (:status (client/request connection {:type :register :player-id id :name (str id)}))))
            (dosync (alter world/world update-in [:players id] assoc :x (first position) :y (second position)))
            (world/change-player-items! id {:wood 10}))
          (is (= :ok (:status (client/request a {:type :build :player-id 1 :building-type :fence :position [11 10]}))))
          (let [state (:world (client/request b {:type :state}))]
            (is (= {:type :building :kind :fence :owner-id 1} (get-in state [:objects [11 10]]))))
          (is (= :object-blocked (:code (client/request b {:type :move :player-id 2 :direction :left})))))
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
(deftest build-on-production-map-preserves-settlement-and-access-test
  (player! 1 [24 29] {:wood 15 :stone 10})
  (dosync (alter world/world assoc :objects layout/initial-objects))
  (with-redefs [buildings/protected-cells production-protected-cells
                layout/spawn-position [23 25]]
    (is (= :ok (:status (build! 1 :house [25 29]))))
    (is (buildings/access-safe? (world/get-world) (:objects (world/get-world))))
    (is (every? #(= :house (get-in (world/get-world) [:objects %])) layout/house-positions))
    (is (= layout/initial-objects (select-keys (:objects (world/get-world)) (keys layout/initial-objects))))))
