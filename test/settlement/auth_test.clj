(ns settlement.auth-test
  (:require [clojure.test :refer :all]
            [settlement.layout :as layout]
            [settlement.test-support :as support]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [settlement.auth :as auth]
            [settlement.world :as world]
            [settlement.config :as config]
            [settlement.framing :as framing]
            [settlement.protocol :as protocol]
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
          (with-redefs [layout/spawn-cells support/row-major-spawn
                  world/gathering (ref {})
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
      (is (= :blue (get-in (world/get-world) [:players 1 :team])))
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
      (is (= {:blue 2}
             (frequencies (map :team (vals (:players (world/get-world)))))))
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
    (is (= :blue (get-in (world/get-world) [:players 2 :team])))
    (is (= :ok (:status (server/process-message :c {:type :register :player-id 3
                                                   :name "Player 3" :team :red}))))
    (is (= :blue (get-in (world/get-world) [:players 3 :team])))
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
      (gui/bind-movement! panel {} resource-ui 1)
      (is (= {:wood 1 :stone 0} (get-in (world/get-world) [:players 1 :collected])))
      (is (nil? (get-in (world/get-world) [:objects [1 0]])))
      (let [stroke (javax.swing.KeyStroke/getKeyStroke java.awt.event.KeyEvent/VK_RIGHT 0)
            action (.get (.getInputMap panel javax.swing.JPanel/WHEN_IN_FOCUSED_WINDOW) stroke)]
        (.actionPerformed (.get (.getActionMap panel) action) nil))
      (is (= 1 (get-in (world/get-world) [:players 1 :x]))))))

(defn- hotbar-resources [ui]
  (select-keys @(get-in ui [:hotbar :items]) [:wood :stone]))

(deftest score-panel-follows-server-state-test
  (javax.swing.SwingUtilities/invokeAndWait
   (fn []
     (let [map-panel (gui/create-map-panel 0 0 1)
           ui (gui/create-resource-panel {:wood 0 :stone 0})
           state {:players {1 {:x 0 :y 0 :collected {:wood 7 :stone 4}}}
                  :objects {}
                  :match {:scores {:blue 17 :red 30} :phase :active}}
           response (atom {:status :ok :world state})
           labels (fn [] (mapv #(.getText (ui %)) [:blue-label :red-label :goal-label]))]
       (is (= ["Синие: 0" "Красные: 0" "Цель: 100"] (labels)))
       (with-redefs [client/async! (fn [_ _ request callback]
                                     (is (= {:type :state} request))
                                     (callback @response))]
         (gui/refresh! map-panel {} ui 1)
         (is (= ["Синие: 17" "Красные: 30" "Цель: 100"] (labels)))
         (is (= {:wood 7 :stone 4} (hotbar-resources ui)))
         (swap! response assoc-in [:world :match :scores :red] 45)
         (gui/refresh! map-panel {} ui 1)
         (is (= ["Синие: 17" "Красные: 45" "Цель: 100"] (labels))))))))

(deftest finished-match-overlay-and-restart-test
  (javax.swing.SwingUtilities/invokeAndWait
   (fn []
     (doseq [[winner title message] [[:blue "Победа!" "Победили синие"]
                                    [:red "Поражение!" "Победили красные"]]]
       (let [map-panel (gui/create-map-panel 0 0 1)
             screen (gui/create-match-screen map-panel)
             gameplay (atom [])
             ui (assoc (gui/create-resource-panel {}) :match-view (atom nil)
                       :match-id (atom 0) :match-screen screen :set-gameplay! #(swap! gameplay conj %))
             active {:players {1 {:x 0 :y 0 :team :blue}}
                     :objects {}
                     :match {:scores {:blue 98 :red 98} :phase :active}}
             finished (-> active
                          (assoc :objects {[1 0] :tree})
                          (assoc-in [:match :scores winner] 100)
                          (assoc-in [:match :phase] :finished)
                          (assoc-in [:match :winner] winner))
             response (atom active)
             requests (atom [])
             restarted (assoc active :match-id 1 :match world/initial-match)]
         (with-redefs [client/async! (fn [_ _ request callback]
                                       (swap! requests conj request)
                                       (callback {:status :ok :world (if (= :restart-match (:type request))
                                                                      restarted @response)}))]
           (#'gui/bind-restart! screen {} map-panel ui 1)
           (gui/refresh! map-panel {} ui 1)
           (is (not (.isVisible (:panel screen))))
           (reset! response finished)
           (dotimes [_ 3] (gui/refresh! map-panel {} ui 1))
           (is (.isVisible (:panel screen)))
           (.setSize map-panel 800 480)
           (.doLayout map-panel)
           (is (= [800 480] [(.getWidth (:panel screen)) (.getHeight (:panel screen))]))
           (is (= [true false] @gameplay))
           (is (= 1 (.getComponentCount map-panel)))
           (is (= title (.getText (:title screen))))
           (is (= message (.getText (:winner screen))))
           (is (= (str "Синие: " (get-in finished [:match :scores :blue])
                       "    Красные: " (get-in finished [:match :scores :red]))
                  (.getText (:score screen))))
           (is (= "Новая игра" (.getText (:button screen))))
           (is (= (if (= :blue winner) "Синие: 100" "Красные: 100")
                  (.getText (ui ({:blue :blue-label :red :red-label} winner)))))
           (is (= {:players (:players finished) :objects (:objects finished)}
                  (.getClientProperty map-panel :settlement.gui/state)))
           (.doClick (:button screen) 0)
           (is (= {:type :restart-match :match-id 0} (peek @requests)))
           (is (= [true false true] @gameplay))
           (is (not (.isVisible (:panel screen))))
           (reset! response finished)
           (gui/refresh! map-panel {} ui 1)
           (is (not (.isVisible (:panel screen))))
           (reset! response (assoc finished :match-id 1))
           (gui/refresh! map-panel {} ui 1)
           (is (.isVisible (:panel screen)))))))))

(deftest polling-client-sees-result-after-another-player-restarts-test
  (let [resumed (promise)
        screen-ref (atom nil)]
    (javax.swing.SwingUtilities/invokeAndWait
     (fn []
       (let [map-panel (gui/create-map-panel 0 0 1)
             screen (gui/create-match-screen map-panel)
             gameplay (atom [])
             ui (assoc (gui/create-resource-panel {}) :match-view (atom nil)
                       :match-id (atom 0) :match-screen screen
                       :set-gameplay! (fn [enabled?]
                                        (when (= [true false true] (swap! gameplay conj enabled?))
                                          (deliver resumed true))))
             active {:players {1 {:x 0 :y 0 :team :red}} :objects {} :match world/initial-match}
             finish {:scores {:blue 100 :red 28} :phase :finished :winner :blue}
             successor (assoc active :match-id 1 :previous-match {:id 0 :result finish})]
         (reset! screen-ref screen)
         (with-redefs [client/async! (fn [snapshot _ _ callback] (callback {:status :ok :world snapshot}))]
           (gui/refresh! map-panel active ui 1)
           (dotimes [_ 2] (gui/refresh! map-panel successor ui 1)))
         (is (.isVisible (:panel screen)))
         (is (not (.isEnabled (:button screen))))
         (is (= "Поражение!" (.getText (:title screen))))
         (is (= [true false] @gameplay)))))
    (is (true? (deref resumed 5000 false)))
    (javax.swing.SwingUtilities/invokeAndWait
     #(is (not (.isVisible (:panel @screen-ref)))))))

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
       (is (= {:wood 0 :stone 0} (hotbar-resources ui-a)))
       (is (= {:wood 0 :stone 1} (hotbar-resources ui-b)))
       (with-redefs [client/async! support/immediate-async]
         (support/process-message :a {:type :gather :player-id 1})
         (gui/update-player-panels! ui-a (world/get-world) 1)
         (is (= {:wood 1 :stone 0} (hotbar-resources ui-a)))
         (is (= {:wood 0 :stone 1} (hotbar-resources ui-b)))
         (let [timer-a (gui/start-map-updater panel-a {:writer :a} ui-a 1)
               timer-b (gui/start-map-updater panel-b {:writer :b} ui-b 2)]
           (try
             ;; Вызываем настоящие обработчики опроса без ожидания таймера.
             (doseq [timer [timer-a timer-b]
                     listener (.getActionListeners timer)]
               (.actionPerformed listener nil))
             (is (= {:wood 1 :stone 0} (hotbar-resources ui-a)))
             (is (= {:wood 0 :stone 1} (hotbar-resources ui-b)))
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
            (is (= {:wood 1 :stone 0}
                   (hotbar-resources (gui/create-resource-panel (gui/player-resources (world/get-world) 1)))))
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

(deftest text-and-edn-sessions-share-world-test
  (with-open [listener (ServerSocket. 0)]
    (let [port (.getLocalPort listener)]
      (with-open [text-socket (doto (Socket. "127.0.0.1" port) (.setSoTimeout 5000))
                  edn-socket (doto (Socket. "127.0.0.1" port) (.setSoTimeout 5000))]
        (let [handlers (mapv (fn [_] (let [socket (.accept listener)]
                                      (future (server/handle-client socket)))) (range 2))
              stream (fn [socket]
                       {:reader (io/reader (.getInputStream socket) :encoding "UTF-8")
                        :writer (io/writer (.getOutputStream socket) :encoding "UTF-8")})
              text (stream text-socket)
              edn (stream edn-socket)
              line! (fn [line]
                      (.write (:writer text) (str line "\r\n"))
                      (.flush (:writer text))
                      (.readLine (:reader text)))
              handshake! (comp protocol/read-message line!)]
          (try
            (is (= :not-authenticated (:code (handshake! "текст 1"))))
            (is (= :not-authenticated (:code (client/request text {:type :inventory}))))
            (let [registration (client/request text {:type :register :player-id 1 :name "Анна"})
                  other (client/request edn {:type :register :player-id 2 :name "Борис"})]
            (is (= :ok (:status registration) (:status other)))
            (is (= :unsupported-version (:code (handshake! "текст 2"))))
            (is (= :ok (:status (client/request text {:type :hello}))))
            (is (= {:status :ok :mode :text :version 1} (handshake! "текст 1")))
            (world/change-player-items! 1 {:wood 2})
            (world/change-player-items! 2 {:stone 3})
            (is (= "reply v=1 ok inventory wood=2 stone=0 axe=0 pickaxe=0"
                   (line! "покажи инвентарь")))
            (is (= :wrong-player (:code (client/request edn {:type :inventory :player-id 1}))))
            (is (= 3 (get-in (client/request edn {:type :inventory}) [:inventory :items :stone])))
            (is (= "reply v=1 error code=invalid-argument" (line! "иди вниз :player-id 2")))
            (is (= "reply v=1 error code=unknown-command" (line! "{:type :hello}")))
            (is (= "reply v=1 ok move x=0 y=1" (line! "иди вниз")))
            (is (= {1 [0 1] 2 [2 0]}
                   (into {} (map (fn [[id player]] [id ((juxt :x :y) player)]))
                         (get-in (client/request edn {:type :state}) [:world :players]))))
            (let [snapshot (line! "покажи мир")]
              (is (str/includes? snapshot "players=[player(1,0,1,blue,\"Анна\"),player(2,2,0,blue,\"Борис\")]"))
              (is (str/ends-with? snapshot "attack_seq=0 repair_seq=0"))
              (doseq [secret [(:token registration) (:token other) (:server-instance registration)]]
                (is (not (str/includes? snapshot secret)))))
            (let [attempt "01234567-89ab-cdef-0123-456789abcdef"]
              (is (= "reply v=1 ok move x=0 y=0" (line! "иди вверх")))
              (let [start (line! (str "собери дерево на 1 0 попытка " attempt))
                    [_ operation-id] (re-find #"operation_id=([0-9a-f-]{36})" start)]
                (is (= (str "reply v=1 ok gather-start operation_id=" operation-id
                            " attempt_id=" attempt " duration_ms=3000 elapsed_ms=0") start))
                (is (str/starts-with? (line! (str "заверши добычу " operation-id))
                                      "reply v=1 error code=too-early operation_id="))))
            (let [long-name (apply str (repeat 4500 "я"))]
              (dosync (alter world/world assoc-in [:players 2 :name] long-name))
              (let [snapshot (line! "покажи мир")]
                (is (< framing/max-request-bytes (alength (.getBytes snapshot "UTF-8"))))
                (is (str/includes? snapshot long-name))
                (is (str/ends-with? snapshot "attack_seq=0 repair_seq=0"))))
            (is (= "reply v=1 error code=missing-argument" (line! "построй дом"))))
            (.write (:writer text) (apply str (repeat (inc framing/max-request-bytes) "x")))
            (.flush (:writer text))
            (is (nil? (.readLine (:reader text))))
            (is (not= ::timeout (deref (first handlers) 5000 ::timeout)))
            (is (nil? (get @world/gathering 1)))
            (is (= :ok (:status (client/request edn {:type :hello}))))
            (finally
              (.close text-socket)
              (.close edn-socket)
              (doseq [handler handlers]
                (deref handler 5000 nil)
                (future-cancel handler)))))))))
