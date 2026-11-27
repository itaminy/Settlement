(ns settlement.bandits-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [settlement.bandits :as bandits]
            [settlement.world :as world]
            [settlement.auth :as auth]
            [settlement.config :as config]
            [settlement.client :as client]
            [settlement.gui :as gui]
            [settlement.building-ui :as building-ui]
            [settlement.layout :as layout]
            [settlement.event-log :as events]
            [settlement.server :as server])
  (:import [java.net ServerSocket Socket]
           [java.awt.event MouseEvent]
           [javax.swing JLabel SwingUtilities]))

(def clock (atom 0))
(defn player [x y team] {:x x :y y :team team :name "Player" :inventory {:items {:wood 9}}})
(defn npc [id x y team]
  {:id id :team :bandits :watch-team team :x x :y y :hp 100 :max-hp 100
   :state :patrol :direction :down :route [[x y] [(+ x 2) y]] :waypoint 0 :target nil})
(defn building [kind team hp]
  {:type :building :building-id (random-uuid) :kind kind :team team :hp hp :max-hp 200})

(use-fixtures :each
  (fn [test]
    (reset! clock 0)
    (with-redefs [world/world (ref {:players {} :objects {} :resources {:wood 0 :stone 0}
                                   :match world/initial-match})
                  world/gathering (ref {})
                  world/now-ns #(* 1000000 @clock)
                  auth/state (ref (auth/new-state))
                  events/journal (events/new-log)]
      (test))))

(deftest scheduled-roster-spawns-at-sixty-and-freezes-after-victory-test
  (let [ticks (atom 0)
        tick world/tick-bandits!]
    (with-redefs [world/tick-bandits! (fn [now] (swap! ticks inc) (tick now))]
      (with-open [scheduler (server/start-bandits!)]
        (let [waiting (:bandits @world/world)]
          (is (= 4 (count waiting)))
          (is (= {:blue 2 :red 2} (frequencies (map :watch-team (vals waiting)))))
          (world/tick-bandits! 59999)
          (is (= waiting (:bandits @world/world)))
          (reset! clock 60000)
          (world/tick-bandits! 60000)
          (is (every? bandits/alive? (vals (:bandits @world/world))))
          (is (= 4 (count (set (map bandits/position (vals (:bandits @world/world)))))))
          (dosync (alter world/world assoc :match {:phase :finished :winner :blue}))
          (let [finished @world/world]
            (world/tick-bandits! 65000)
            (is (= finished @world/world)))))
      (let [before @ticks] (Thread/sleep 230) (is (= before @ticks))))))

(deftest patrol-and-shortest-route-use-the-only-bridge-test
  (let [water (into #{} (for [y (range config/world-height) :when (not= y 25)] [50 y]))]
    (is (= {:step [49 25] :destination [51 24]}
           (bandits/first-step [49 24] #{[51 24]} water 8)))
    (is (nil? (bandits/first-step [49 24] #{[50 24]} water 8))))
  (let [id (random-uuid) resident (npc id 12 12 :blue)]
    (dosync (alter world/world assoc :bandits {id resident}))
    (doseq [now [0 450 900]] (world/tick-bandits! now))
    (is (= [13 12] (bandits/position (get-in @world/world [:bandits id]))))
    (is (some #{:bandit-moved} (mapcat :changes (:events @events/journal))))))

(deftest bandits-damage-both-teams-and-destruction-deducts-points-test
  (let [blue (random-uuid) red (random-uuid)]
    (dosync
      (alter world/world assoc :bandits {blue (npc blue 9 10 :blue) red (npc red 19 10 :red)}
             :objects {[10 10] (building :house :blue 10) [20 10] (building :mine :red 20)}
             :match {:phase :active :winner nil :scores {:blue 15 :red 20}}))
    (world/tick-bandits! 0)
    (is (nil? (get-in @world/world [:objects [10 10]])))
    (is (= {:blue 0 :red 20} (get-in @world/world [:match :scores])))
    (is (= 10 (get-in @world/world [:objects [20 10] :hp])))
    (world/tick-bandits! 1999)
    (is (= 10 (get-in @world/world [:objects [20 10] :hp])))
    (world/tick-bandits! 2000)
    (is (empty? (:objects @world/world)))
    (is (= {:blue 0 :red 0} (get-in @world/world [:match :scores])))
    (is (= {:building-attacks 3 :buildings-destroyed 2 :building-attacks-by-team {:blue 1 :red 2}}
           (:bandit-stats @world/world)))))

(deftest battle-replay-reward-collision-and-safe-respawn-test
  (let [id (random-uuid)
        hit #(world/attack-bandit! 1 [11 10] id 2)]
    (dosync
      (alter world/world assoc :players {1 (player 10 10 :blue)}
             :bandits {id (assoc (npc id 11 10 :blue) :hp 20 :route [[11 10] [13 10]])}))
    (is (= :object-blocked (:code (world/move-player-result 1 :right))))
    (is (= 10 (:hp (world/attack-bandit! 1 [11 10] id 1))))
    (world/tick-bandits! 500)
    (is (= 10 (get-in @world/world [:bandits id :hp])))
    (is (= {:player 1} (get-in @world/world [:bandits id :target])))
    (reset! clock 1000)
    (let [start (promise) tasks (repeatedly 2 #(future @start (hit)))]
      (let [tasks (vec tasks)] (deliver start true) (is (every? :destroyed? (map deref tasks)))))
    (is (= 1 (get-in @world/world [:bandit-stats :bandits-killed])))
    (is (= {:wood 14 :stone 3}
           (select-keys (get-in (world/get-world) [:players 1 :inventory :items]) [:wood :stone])))
    (is (world/free-cell? @world/world [11 10]))
    (world/tick-bandits! 60999)
    (is (not (bandits/alive? (get-in @world/world [:bandits id]))))
    (world/tick-bandits! 61000)
    (let [returning (get-in @world/world [:bandits id])]
      (is (= 100 (:hp returning)))
      (is (>= (bandits/distance [10 10] (bandits/position returning)) (:safe-distance config/bandits))))))

(deftest retaliation-downs-player-and-respawns-with-stock-test
  (let [id (random-uuid)]
    (dosync
      (alter world/world assoc :players {1 (assoc (player 10 10 :blue) :hp 10)}
             :bandits {id (assoc (npc id 11 10 :blue) :target {:player 1})}))
    (world/tick-bandits! 0)
    (is (= 0 (get-in @world/world [:players 1 :hp])))
    (is (= :player-downed (:code (world/move-player-result 1 :up))))
    (is (world/free-cell? @world/world [10 10]))
    (world/tick-bandits! 14999)
    (is (= 15000 (get-in @world/world [:players 1 :respawn-at])))
    (world/tick-bandits! 15000)
    (let [player (get-in @world/world [:players 1])]
      (is (= 100 (:hp player)))
      (is (= layout/spawn-position (bandits/position player)))
      (is (= 9 (get-in player [:inventory :items :wood]))))))

(deftest placement-and-bandit-step-cannot-share-a-cell-test
  (let [id (random-uuid) position [11 10]
        resident (assoc (npc id 12 10 :blue) :route [[10 10]])
        start (promise)]
    (dosync (alter world/world assoc :players {1 (player 10 11 :blue)} :bandits {id resident}))
    (let [move (future @start (world/tick-bandits! 0))
          build (future @start (world/build! 1 :fence position))]
      (deliver start true)
      @move
      (let [result @build npc (get-in @world/world [:bandits id])]
        (if (= :ok (:status result))
          (is (not= position (bandits/position npc)))
          (do (is (= :object-blocked (:code result))) (is (= position (bandits/position npc)))))))))

(deftest swing-f-click-targets-the-bandit-seen-on-screen-test
  (SwingUtilities/invokeAndWait
   (bound-fn []
     (let [local 311 id (random-uuid)
           panel (doto (gui/create-map-panel 10 10 local) (.setSize 800 600))
           label (JLabel.)
           sent (atom nil)
           camera {:x 300.7 :y 360.2 :width 800 :height 600}
           npc (npc id 11 10 :blue)]
       (gui/update-map panel {local (player 10 10 :blue)} {} {id npc})
       (.putClientProperty panel :settlement.gui/camera camera)
       (.putClientProperty panel :settlement.gui/visual-bandits {id (assoc npc :x 10.4)})
       (with-redefs [client/async! (fn [_ _ request callback]
                                     (reset! sent request)
                                     (callback {:status :ok :bandit-id id :hp 90})
                                     true)]
         (let [controller (building-ui/install! panel {} local label identity (constantly nil))]
           (try
             (.actionPerformed (.get (.getActionMap panel) "attack-mode") nil)
             (let [event (MouseEvent. panel MouseEvent/MOUSE_CLICKED 0 0 397 308
                                      1 false MouseEvent/BUTTON1)]
               (doseq [listener (.getMouseListeners panel)] (.mouseClicked listener event)))
             (is (= {:type :attack-bandit :player-id local :position [11 10] :bandit-id id :attack-seq 1}
                    @sent))
             (is (str/includes? (.getText label) "90 HP"))
             (finally ((:close! controller))))))))))

(deftest prolog-style-tcp-attack-snapshot-and-push-test
  (let [id (random-uuid)]
    (with-open [listener (ServerSocket. 0)
                socket (doto (Socket. "127.0.0.1" (.getLocalPort listener)) (.setSoTimeout 5000))
                in (io/reader (.getInputStream socket) :encoding "UTF-8")
                out (io/writer (.getOutputStream socket) :encoding "UTF-8")]
      (let [worker (future (server/handle-client (.accept listener)))
            send! (fn [line] (.write out (str line "\n")) (.flush out) (.readLine in))]
        (try
          (is (= :ok (:status (clojure.edn/read-string
                                (send! "{:type :register :player-id 2 :name \"Bot\" :client-kind :prolog}")))))
          (dosync (alter world/world assoc-in [:players 2] (player 10 10 :red))
                  (alter world/world assoc :bandits {id (assoc (npc id 11 10 :red) :hp 10)}))
          (is (= :text (:mode (clojure.edn/read-string (send! "\u0442\u0435\u043a\u0441\u0442 1")))))
          (is (str/includes? (send! "\u043f\u043e\u043a\u0430\u0436\u0438 \u043c\u0438\u0440")
                             (str "bandit(" id ",11,10,10,100,patrol,down)")))
          (is (str/starts-with? (send! "\u043f\u043e\u0434\u043f\u0438\u0448\u0438\u0441\u044c \u043d\u0430 \u0441\u043e\u0431\u044b\u0442\u0438\u044f")
                                "reply v=1 ok subscribe-events"))
          (let [reply (send! (str "\u0430\u0442\u0430\u043a\u0443\u0439 \u0431\u0430\u043d\u0434\u0438\u0442\u0430 \u043d\u0430 11 10 id " id " \u043d\u043e\u043c\u0435\u0440 1"))]
            (is (str/starts-with? reply "reply v=1 ok attack-bandit"))
            (is (str/includes? reply "hp=0 destroyed=true")))
          (is (str/includes? (.readLine in) "bandit-killed"))
          (is (= 14 (get-in @world/world [:players 2 :inventory :items :wood])))
          (finally (.close socket) (deref worker 5000 nil)))))))
