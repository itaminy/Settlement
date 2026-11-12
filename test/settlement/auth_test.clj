(ns settlement.auth-test
  (:require [clojure.test :refer :all]
            [settlement.test-support :as support]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [settlement.auth :as auth]
            [settlement.world :as world]
            [settlement.config :as config]
            [settlement.server :as server]
            [settlement.client :as client]
            [settlement.gui :as gui]
            [settlement.credentials :as credentials])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.net ServerSocket Socket]))

(use-fixtures :each
  (fn [f]
    (let [directory (.toFile (Files/createTempDirectory "settlement-test-" (make-array FileAttribute 0)))]
      (try
        (binding [credentials/*credentials-file* (str (io/file directory "credentials.edn"))]
          (with-redefs [world/gathering (ref {})
                  world/now-ns #(deref support/time-ns)
                  world/world (ref {:players {} :resources {:wood 0 :stone 0}
                                         :objects {[1 0] :tree [3 0] :stone}})
                        auth/state (ref (auth/new-state))
                        world/schedule-resource-restore (fn [_ _] nil)]
            (f)))
        (finally
          (doseq [file (reverse (file-seq directory))] (io/delete-file file true)))))))

(defn- register [connection id]
  (support/process-message connection {:type :register :player-id id :name (str "Player " id)}))

(defn- login [connection id registration]
  (support/process-message connection (assoc (select-keys registration [:token :server-instance])
                                           :type :login :player-id id)))

(deftest registration-and-secrets-test
  (let [first (register :a 1)
        second (register :b 2)
        before (world/get-world)]
    (is (= :ok (:status first)))
    (is (= 43 (count (:token first))))
    (is (not= (:token first) (:token second)))
    (is (= :error (:status (register :c 1))))
    (is (= :error (:status (register :a 1))))
    (is (= :error (:status (register :a 3))))
    (is (= before (world/get-world)))
    (is (not (.contains (pr-str (support/process-message {:type :state})) (:token first))))
    (is (not (.contains (pr-str @auth/state) (:token first))))
    (is (nil? (world/add-player 1 "Replacement")))
    (is (= before (world/get-world)))))

(deftest command-authorization-test
  (register :a 1)
  (register :b 2)
  (doseq [connection [nil :unregistered :b]
          command [{:type :move :direction :right} {:type :gather}]]
    (let [before (world/get-world)]
      (is (= :error (:status (support/process-message connection (assoc command :player-id 1)))))
      (is (= before (world/get-world)))))
  (is (= :error (:status (support/process-message :a {:type :move :direction :right}))))
  (is (= :ok (:status (support/process-message :a {:type :gather :player-id 1}))))
  (is (= :ok (:status (support/process-message :a {:type :move :player-id 1 :direction :down}))))
  (is (= :ok (:status (support/process-message :b {:type :move :player-id 2 :direction :down})))))

(deftest reconnect-preserves-progress-test
  (let [registration (register :a 1)]
    (support/process-message :a {:type :gather :player-id 1})
    (support/process-message :a {:type :move :player-id 1 :direction :down})
    (let [before (world/get-world)]
      (is (= :already-connected (:code (login :b 1 registration))))
      (auth/disconnect! :a)
      (is (= :error (:status (register :b 1))))
      (is (= :invalid-credentials (:code (login :b 1 (assoc registration :token "wrong")))))
      (is (= :invalid-credentials (:code (login :b 2 registration))))
      (is (= :ok (:status (login :b 1 registration))))
      (auth/disconnect! :a)
      (is (= 1 (auth/owner :b)))
      (is (= before (world/get-world)))
      (is (= {:wood 1 :stone 0} (get-in before [:players 1 :collected])))
      (is (= 1 (get-in before [:players 1 :y]))))))

(deftest concurrent-registration-and-login-test
  (let [start (promise)
        tasks (mapv #(future @start (register % 1)) [:a :b])]
    (deliver start true)
    (let [results (mapv #(deref % 5000 {:status :timeout}) tasks)
          registration (first (filter #(= :ok (:status %)) results))]
      (is (= {:ok 1 :error 1} (frequencies (map :status results))))
      (auth/disconnect! :a)
      (auth/disconnect! :b)
      (let [start (promise)
            tasks (mapv #(future @start (login % 1 registration)) [:c :d])]
        (deliver start true)
        (is (= {:ok 1 :error 1}
               (frequencies (map :status (mapv #(deref % 5000 {:status :timeout}) tasks)))))))))

(defn- fill-map-except! [free-positions]
  (dosync
    (alter world/world assoc :objects
           (into {} (for [y (range config/world-height) x (range config/world-width)
                          :when (not (free-positions [x y]))]
                      [[x y] :house])))))

(deftest full-map-registration-is-atomic-test
  (fill-map-except! #{})
  (let [before (world/get-world)
        auth-before @auth/state
        response (register :a 1)]
    (is (= {:status :error :code :map-full} response))
    (is (= before (world/get-world)))
    (is (= auth-before @auth/state))
    (is (nil? (auth/owner :a)))))

(deftest concurrent-registration-for-last-cell-test
  (fill-map-except! #{[0 0]})
  (let [ready (java.util.concurrent.CountDownLatch. 2)
        start (promise)
        tasks (mapv (fn [id]
                      (future (.countDown ready)
                              (when (= :go (deref start 5000 :timeout)) (register id id)))) [1 2])]
    (try
      (is (.await ready 5 java.util.concurrent.TimeUnit/SECONDS))
      (deliver start :go)
      (let [responses (mapv #(deref % 5000 {:status :timeout}) tasks)
            ids (set (keys (:players (world/get-world))))]
        (is (= {:ok 1 :error 1} (frequencies (map :status responses))))
        (is (= :map-full (:code (first (filter #(= :error (:status %)) responses)))))
        (is (= 1 (count ids)))
        (is (= ids (set (keys (:credentials @auth/state)))))
        (is (= ids (set (keys (:sessions @auth/state)))))
        (is (= [0 0] ((juxt :x :y) (first (vals (:players (world/get-world))))))))
      (finally
        (deliver start :cancel)
        (doseq [task tasks] (future-cancel task))))))

(deftest simultaneous-registration-uses-distinct-cells-test
  (let [start (promise)
        tasks (mapv #(future @start (register % %)) [1 2])]
    (try
      (deliver start true)
      (is (every? #(= :ok (:status (deref % 5000 {}))) tasks))
      (is (= #{[0 0] [2 0]}
             (set (map (juxt :x :y) (vals (:players (world/get-world)))))))
      (finally (doseq [task tasks] (future-cancel task))))))

(deftest registration-failure-rolls-back-player-test
  (let [before (world/get-world) auth-before @auth/state]
    (with-redefs-fn {#'auth/digest (fn [_] (throw (Exception. "Injected failure")))}
      #(is (thrown? Exception (register :a 1))))
    (is (= before (world/get-world)))
    (is (= auth-before @auth/state))))

(deftest disconnected-player-reserves-position-test
  (let [registration (register :a 1)]
    (auth/disconnect! :a)
    (register :b 2)
    (is (= [2 0] ((juxt :x :y) (get-in (world/get-world) [:players 2]))))
    (is (= :object-blocked (:code (support/process-message :b {:type :move :player-id 2 :direction :left}))))
    ;; Клетка [1 0] занята деревом, поэтому сначала освобождаем её добычей.
    (is (= :ok (:status (support/process-message :b {:type :gather :player-id 2}))))
    (is (= :wood (:resource (support/process-message :b {:type :gather :player-id 2}))))
    (is (= :ok (:status (support/process-message :b {:type :move :player-id 2 :direction :left}))))
    (is (= :player-blocked (:code (support/process-message :b {:type :move :player-id 2 :direction :left}))))
    (is (= :ok (:status (login :reconnected 1 registration))))
    (is (= [0 0] ((juxt :x :y) (get-in (world/get-world) [:players 1]))))))

(deftest credentials-storage-test
  (is (nil? (credentials/load-entry "server" 1)))
  (let [tasks (mapv (fn [id] (future (credentials/save-entry! "server" id "old" (str "secret-" id)))) [1 2])]
    (is (every? #(= :ok (:status (deref % 5000 {}))) tasks)))
  (is (= "secret-1" (:token (credentials/load-entry "server" 1))))
  (is (= "secret-2" (:token (credentials/load-entry "server" 2))))
  (credentials/save-entry! "server" 1 "new" "replacement")
  (is (= "old" (:server-instance (credentials/load-entry "server" 2))))
  (is (= "new" (:server-instance (credentials/load-entry "server" 1))))
  (spit credentials/*credentials-file* "{broken")
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Cannot read local credentials"
                        (credentials/load-entry "server" 1)))
  (is (= {:status :error :code :credentials-save-failed}
         (credentials/save-entry! "server" 1 "new" "never-log-this"))))

(deftest client-save-retry-and-reconnect-test
  (let [connection (atom :a)
        save credentials/save-entry!
        attempts (atom 0)
        retries (atom 0)]
    (with-redefs [client/request (fn [_ message] (support/process-message @connection message))
                  credentials/save-entry! (fn [& args]
                                            (if (= 1 (swap! attempts inc))
                                              {:status :error :code :credentials-save-failed}
                                              (apply save args)))]
      (is (= {:status :ok}
             (client/authorize! {} 1 "Alice" (constantly false)
                                #(do (swap! retries inc) true))))
      (is (= 1 @retries))
      (is (= 2 @attempts))
      (support/process-message :a {:type :gather :player-id 1})
      (support/process-message :a {:type :move :player-id 1 :direction :down})
      (auth/disconnect! :a)
      (reset! connection :b)
      (let [before (world/get-world)]
        (is (= :ok (:status (client/authorize! {} 1 "Changed" (constantly false) (constantly false)))))
        (is (= before (world/get-world)))))))

(deftest stale-token-client-test
  (credentials/save-entry! "127.0.0.1:5000" 1 "previous-server" "old-secret")
  (with-redefs [client/request (fn [_ message] (support/process-message :a message))]
    (is (= :cancelled (:code (client/authorize! {} 1 "Alice" (constantly false) (constantly false)))))
    (is (empty? (:players (world/get-world))))
    (is (= :ok (:status (client/authorize! {} 1 "Alice" (constantly true) (constantly false)))))
    (is (not= "old-secret" (:token (credentials/load-entry "127.0.0.1:5000" 1))))))

(deftest stale-token-cannot-replace-existing-player-test
  (let [registration (register :existing 1)
        before (world/get-world)]
    (credentials/save-entry! "127.0.0.1:5000" 1 "previous-server" "old-secret")
    (with-redefs [client/request (fn [_ message] (support/process-message :new message))]
      (is (= :player-exists (:code (client/authorize! {} 1 "Replacement" (constantly true) (constantly false))))))
    (is (= before (world/get-world)))
    (is (= "old-secret" (:token (credentials/load-entry "127.0.0.1:5000" 1))))
    (is (= :stale-server (:code (login :new 1 (assoc registration :server-instance "old")))))))

(deftest save-cancel-does-not-expose-secret-test
  (with-redefs [client/request (fn [_ message] (support/process-message :a message))
                credentials/save-entry! (fn [& _] {:status :error :code :credentials-save-failed})]
    (let [result (atom nil)
          output (with-out-str (reset! result (client/authorize! {} 1 "Alice" (constantly false) (constantly false))))]
      (is (= "" output))
      (is (= {:status :error :code :credentials-save-failed} @result)))))

(deftest gui-key-bindings-test
  (register :a 1)
  (let [panel (gui/create-map-panel 0 0)
        resource-ui (gui/create-resource-panel (gui/player-resources (world/get-world) 1))]
    (with-redefs [client/async! support/immediate-async]
      (support/process-message :a {:type :gather :player-id 1})
      (gui/update-player-panels! resource-ui (world/get-world) 1)
      (gui/bind-key panel {} 1 java.awt.event.KeyEvent/VK_RIGHT :right)
      (is (= {:wood 1 :stone 0} (get-in (world/get-world) [:players 1 :collected])))
      (is (nil? (get-in (world/get-world) [:objects [1 0]])))
      (.actionPerformed (.get (.getActionMap panel) "move-right") nil)
      (is (= 1 (get-in (world/get-world) [:players 1 :x]))))))

(defn- panel-text [ui]
  (str/join " " (map #(.getText %) (.getComponents (:panel ui)))))

(deftest personal-resource-panels-test
  (register :a 1)
  (register :b 2)
  (is (= [2 0] ((juxt :x :y) (get-in (world/get-world) [:players 2]))))
  (is (= :ok (:status (support/process-message :b {:type :gather :player-id 2}))))
  (javax.swing.SwingUtilities/invokeAndWait
   (fn []
     (let [panel-a (gui/create-map-panel 0 0)
           panel-b (gui/create-map-panel 2 0)
           ui-a (gui/create-resource-panel (gui/player-resources (world/get-world) 1))
           ui-b (gui/create-resource-panel (gui/player-resources (world/get-world) 2))]
       (is (= "Моё дерево: 0 | Мой камень: 0" (panel-text ui-a)))
       (is (= "Моё дерево: 0 | Мой камень: 1" (panel-text ui-b)))
       (with-redefs [client/async! support/immediate-async]
         (support/process-message :a {:type :gather :player-id 1})
         (gui/update-player-panels! ui-a (world/get-world) 1)
         (is (= "Моё дерево: 1 | Мой камень: 0" (panel-text ui-a)))
         (is (= "Моё дерево: 0 | Мой камень: 1" (panel-text ui-b)))
         (let [timer-a (gui/start-map-updater panel-a {:writer :a} ui-a 1)
               timer-b (gui/start-map-updater panel-b {:writer :b} ui-b 2)]
           (try
             ;; Вызываем настоящие обработчики опроса без ожидания таймера.
             (doseq [timer [timer-a timer-b]
                     listener (.getActionListeners timer)]
               (.actionPerformed listener nil))
             (is (= "Моё дерево: 1 | Мой камень: 0" (panel-text ui-a)))
             (is (= "Моё дерево: 0 | Мой камень: 1" (panel-text ui-b)))
             (finally (.stop timer-a) (.stop timer-b)))))
       (is (= {:wood 1 :stone 0} (gui/player-resources (world/get-world) 1)))
       (is (= {:wood 0 :stone 1} (gui/player-resources (world/get-world) 2)))
       (is (= {:wood 1 :stone 1} (:resources (world/get-world))))))))

(deftest two-tcp-clients-and-reconnect-test
  (with-open [listener (ServerSocket. 0)]
    (let [connections (atom [])
          open! (fn []
                  (let [socket (Socket. "127.0.0.1" (.getLocalPort listener))
                        accepted (.accept listener)
                        handler (future (server/handle-client accepted))
                        connection {:socket socket
                                    :reader (java.io.BufferedReader. (java.io.InputStreamReader. (.getInputStream socket) "UTF-8"))
                                    :writer (java.io.BufferedWriter. (java.io.OutputStreamWriter. (.getOutputStream socket) "UTF-8"))}]
                    (.setSoTimeout socket 5000)
                    (swap! connections conj [connection handler])
                    connection))]
      (try
        (let [a (open!) b (open!)
              authorize (fn [connection id] (client/authorize! connection id (str "Player " id)
                                                               (constantly false) (constantly false)))]
          (is (= :ok (:status (authorize a 1))))
          (is (= :ok (:status (authorize b 2))))
          (is (= :ok (:status (support/request a {:type :gather :player-id 1}))))
          (support/request a {:type :move :player-id 1 :direction :down})
          (is (= [2 0] ((juxt :x :y) (get-in (world/get-world) [:players 2]))))
          (is (= :ok (:status (support/request b {:type :gather :player-id 2}))))
          (let [before (world/get-world)
                duplicate (open!)]
            (is (= :already-connected (:code (authorize duplicate 1))))
            (is (= :wrong-player (:code (support/request b {:type :move :player-id 1 :direction :right}))))
            (is (= :wrong-player (:code (support/request b {:type :gather :player-id 1}))))
            (client/close-connection a)
            (is (not= ::timeout (deref (second (first @connections)) 5000 ::timeout)))
            (is (= :ok (:status (authorize (open!) 1))))
            (is (= before (world/get-world)))
            (is (= 1 (get-in before [:players 1 :y])))
            (is (= 1 (get-in before [:players 1 :collected :wood])))
            (is (= {:wood 1 :stone 0} (gui/player-resources (world/get-world) 1)))
            (is (= {:wood 0 :stone 1} (gui/player-resources (world/get-world) 2)))
            (is (= "Моё дерево: 1 | Мой камень: 0"
                   (panel-text (gui/create-resource-panel (gui/player-resources (world/get-world) 1)))))
            (is (= :ok (:status (support/request b {:type :move :player-id 2 :direction :down}))))))
        (finally
          (doseq [[connection handler] @connections]
            (client/close-connection connection)
            (deref handler 5000 nil)
            (future-cancel handler)))))))

(deftest tcp-disconnect-test
  (with-open [listener (ServerSocket. 0)
              socket (Socket. "127.0.0.1" (.getLocalPort listener))]
    (let [handler (future (server/handle-client (.accept listener)))
          connection {:socket socket
                      :reader (java.io.BufferedReader. (java.io.InputStreamReader. (.getInputStream socket) "UTF-8"))
                      :writer (java.io.BufferedWriter. (java.io.OutputStreamWriter. (.getOutputStream socket) "UTF-8"))}]
      (.setSoTimeout socket 5000)
      (try
        (let [registration (support/request connection {:type :register :player-id 1 :name "Alice"})]
          (is (= :ok (:status registration)))
          (is (= :ok (:status (support/request connection {:type :gather :player-id 1}))))
          (support/request connection {:type :move :player-id 1 :direction :down})
          (let [before (world/get-world)]
            (.close socket)
            (is (not= ::timeout (deref handler 5000 ::timeout)))
            (is (empty? (:sessions @auth/state)))
            (is (= :ok (:status (login :reconnected 1 registration))))
            (is (= before (world/get-world)))))
        (finally (.close socket) (future-cancel handler))))))
