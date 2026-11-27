(require '[clojure.java.io :as io]
         '[settlement.bandits :as bandits]
         '[settlement.buildings :as buildings]
         '[settlement.client :as client]
         '[settlement.effects :as effects]
         '[settlement.gui :as gui]
         '[settlement.layout :as layout]
         '[settlement.rendering :as rendering]
         '[settlement.server :as server]
         '[settlement.world :as world])
(import '[java.awt Color Dimension]
        '[java.awt.image BufferedImage]
        '[java.net InetAddress ServerSocket Socket]
        '[javax.imageio ImageIO]
        '[javax.swing JFrame JLabel JProgressBar SwingUtilities])

(def folder (io/file (or (first *command-line-args*) ".settlement/effect-frames")))
(def blue 8101)
(def red 8102)

(defn edt! [f]
  (let [v (volatile! nil)]
    (SwingUtilities/invokeAndWait #(vreset! v (f)))
    @v))

(defn connect [port id kind]
  (let [socket (doto (Socket. "127.0.0.1" port) (.setSoTimeout 5000))
        conn {:socket socket :reader (io/reader socket :encoding "UTF-8")
              :writer (io/writer socket :encoding "UTF-8")}]
    (assert (= :ok (:status (client/request conn {:type :register :player-id id
                                                 :client-kind kind :name (if (= id blue) "Blue" "Red")}))))
    conn))

(defn display! []
  (let [window (JFrame. "Settlement · Effects")
        map-panel (gui/create-map-panel 24 25 blue)
        label (doto (JLabel.) (.setForeground (Color. 240 229 193))
                (.setPreferredSize (Dimension. 480 26)))
        progress (doto (JProgressBar.) (.setVisible false))
        ui (gui/create-resource-panel {})
        content (:panel (gui/create-game-content map-panel ui progress label))]
    (.setContentPane window content)
    (.setSize window 784 537)
    (.setVisible window true)
    {:window window :panel map-panel :ui ui :content content :label label}))

(defn update! [{:keys [panel ui]} state]
  (edt! #(#'gui/apply-world! panel ui state blue)))

(defn state! [view conn]
  (update! view (:world (client/request conn {:type :state}))))

(defn command! [{:keys [panel] :as view} conn sent]
  (let [reply (client/request conn sent)]
    (when-not (= :ok (:status reply))
      (throw (ex-info "Demo action rejected" {:type (:type sent) :code (:code reply)
                                            :message (:message reply)})))
    (edt! #(do (when (= :gather-finish (:type sent)) (effects/clear-gather! panel))
               (effects/add! panel (effects/confirmed-effects (:player-id sent) (:type sent) reply))))
    (when-let [state (:world reply)] (update! view state))
    reply))

(defn pose [[x y] entry]
  (merge entry {:x x :y y :hp 100 :inventory {:items {:wood 15 :stone 15 :axe 1 :pickaxe 1}}}
         {:respawn-at nil}))

(defn setup! [{:keys [panel label] :as view} conn name position opponent alter-state]
  (dosync
    (alter world/world
           (fn [state]
             (-> state
                 (assoc :objects layout/initial-objects :match world/initial-match
                        :building-attacks {} :building-repairs {}
                        :bandits (bandits/new-roster (world/combat-time)))
                 (update :match-id (fnil inc 0))
                 (update-in [:players blue] #(pose position %))
                 (update-in [:players red] #(pose opponent %))
                 alter-state))))
  (state! view conn)
  (edt! #(do (.setText label name)
             (.putClientProperty panel :settlement.effects/events []))))

(defn closest [x y]
  (first (for [r (range 6), dx (range (- r) (inc r)), dy (range (- r) (inc r))
               :when (= r (+ (abs dx) (abs dy)))
               :let [p [(+ x dx) (+ y dy)]]
               :when (not (layout/initial-objects p))] p)))

(defn capture! [{:keys [content]} name count action]
  (let [dir (io/file folder name)
        [w h] (edt! #(vector (.getWidth content) (.getHeight content)))
        im (BufferedImage. w h BufferedImage/TYPE_INT_RGB)]
    (.mkdirs dir)
    (dotimes [i count]
      (let [start (System/nanoTime)]
        (action i)
        (edt! #(let [g (.createGraphics im)]
                 (try (.printAll content g) (finally (.dispose g)))))
        (ImageIO/write im "png" (io/file dir (format "frame-%04d.png" i)))
        (let [ms (long (/ (- (System/nanoTime) start) 1000000))]
          (when (< ms 40) (Thread/sleep (- 40 ms))))))
    (println name count)))

(defn environment! [view conn]
  (doseq [[name title target] [["water" "Река · течение, водопад и брызги" (closest 51 9)]
                               ["forest" "Лес · кроны, трава и падающие листья" (closest 36 8)]
                               ["camp" "Лагерь бандитов · огонь, фонарь и дым" (closest 18 18)]]]
    (setup! view conn title target [76 25] identity)
    (capture! view name 70 (constantly nil))))

(defn harvest! [view conn]
  (let [origin [24 27] wood [24 26] ore [25 27] op (atom nil)]
    (setup! view conn "Добыча · топор и кирка" origin [76 25]
            #(update % :objects (fn [objects] (-> objects (dissoc origin)
                                                 (assoc wood :tree ore :stone)))))
    (let [start! (fn [p]
                   (reset! op (:operation-id (command! view conn
                                {:type :gather-start :player-id blue :position p
                                 :attempt-id (str (random-uuid))}))))
          update-gather! (fn [kind]
                           (command! view conn {:type kind :player-id blue :operation-id @op}))]
      (capture! view "harvest" 145
        (fn [i]
          (case i
            8 (start! wood)
            22 (update-gather! :gather-heartbeat)
            37 (do (update-gather! :gather-finish) (state! view conn))
            60 (start! ore)
            75 (update-gather! :gather-heartbeat)
            90 (update-gather! :gather-heartbeat)
            102 (do (update-gather! :gather-finish) (state! view conn))
            nil))))))

(defn stage-bandit [state position hp attack-at]
  (let [[x y] position id (first (keys (:bandits state)))]
    (-> state
        (assoc-in [:bandits id] (merge (get-in state [:bandits id])
                                     {:x x :y y :hp hp :state (if attack-at :attack :patrol)
                                      :target (when attack-at {:player blue}) :direction :left
                                      :next-move (or attack-at Long/MAX_VALUE)
                                      :next-attack (or attack-at Long/MAX_VALUE)}))
        (update-in [:bandits id] dissoc :respawn-at))))

(defn combat! [view conn]
  (let [origin [31 25] target [32 25]]
    (setup! view conn "Бой с бандитом · удар, урон и гибель" origin [76 25]
            #(-> % (update :objects dissoc origin target) (stage-bandit target 20 nil)))
    (let [id (first (filter #(get-in @world/world [:bandits % :x]) (keys (:bandits @world/world))))
          hit! (fn [i] (command! view conn {:type :attack-bandit :player-id blue :position target
                                           :bandit-id id :attack-seq i}))]
      (capture! view "combat" 92 #(case % 10 (hit! 1) 45 (hit! 2) nil)))))

(defn structure! [view conn enemy]
  (let [home [24 31] target [25 31] foe [26 31] id (atom nil)
        attack! (fn [seq] (command! view enemy {:type :attack-building :player-id red
                                               :position target :building-id @id :attack-seq seq}))]
    (setup! view conn "Постройка · строительство, трещины, ремонт и обломки" home foe
            #(update % :objects dissoc home target foe))
    (capture! view "building" 199
      (fn [i]
        (case i
          5 (reset! id (:building-id (:building (command! view conn {:type :build :player-id blue
                                                                     :building-type :fence :position target}))))
          25 (attack! 1)
          53 (attack! 2)
          81 (attack! 3)
          97 (command! view conn {:type :repair-building :player-id blue :position target
                                   :building-id @id :repair-seq 1})
          111 (attack! 4)
          139 (attack! 5)
          167 (attack! 6)
          nil)))))

(defn death! [view conn]
  (let [origin [24 26] target [24 25]]
    (setup! view conn "Возрождение · последние секунды ожидания сокращены в записи" origin [76 25]
            #(-> % (update :objects dissoc origin target)
                  (assoc-in [:players blue :hp] 10)
                  (stage-bandit target 100 (+ (world/combat-time) 760))))
    (capture! view "respawn" 102
      (fn [i]
        (when (= i 51)
          ;; Retain real server timers while omitting idle frames in the recording.
          (while (get-in @world/world [:players blue :respawn-at]) (Thread/sleep 100)))
        (when (zero? (mod i 4)) (state! view conn))))))

(with-open [listener (ServerSocket. 0 16 (InetAddress/getLoopbackAddress))]
  (let [acceptor (future (server/serve! listener))
        port (.getLocalPort listener)
        conn (connect port blue :human)
        enemy (connect port red :prolog)
        view (edt! display!)]
    (try
      (environment! view conn)
      (harvest! view conn)
      (combat! view conn)
      (structure! view conn enemy)
      (death! view conn)
      (finally
        (edt! #(.dispose (:window view)))
        (client/close-connection conn)
        (client/close-connection enemy)
        (.close listener)
        (deref acceptor 3000 nil)
        (shutdown-agents)))))
