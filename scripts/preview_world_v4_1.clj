(require '[clojure.java.io :as io]
         '[settlement.buildings :as buildings]
         '[settlement.client :as client]
         '[settlement.gui :as gui]
         '[settlement.landscape :as landscape]
         '[settlement.layout :as layout]
         '[settlement.rendering :as rendering]
         '[settlement.server :as server]
         '[settlement.world :as world])
(import '[java.awt BorderLayout Color Dimension GraphicsEnvironment RenderingHints]
        '[java.awt.image BufferedImage]
        '[java.net InetAddress ServerSocket Socket]
        '[javax.imageio ImageIO]
        '[javax.swing JFrame JLabel JPanel JProgressBar SwingUtilities])

(def output (io/file "docs/previews/world_v4_1"))
(.mkdirs output)

(defn edt! [f]
  (let [result (volatile! nil)]
    (SwingUtilities/invokeAndWait #(vreset! result (f)))
    @result))

(defn capture [^JPanel panel size]
  (let [[w h] size
        image (BufferedImage. w h BufferedImage/TYPE_INT_RGB)
        graphics (.createGraphics image)]
    (try (.printAll panel graphics) (finally (.dispose graphics)))
    image))

(defn write-image [image name]
  (ImageIO/write image "png" (io/file output (str name ".png"))))

(def regions
  [["forest" [36 8]] ["clearing" [23 9]] ["lake" [89 57]]
   ["river-bridge" [48 25]] ["waterfall" [51 9]]
   ["mountains" [16 62]] ["oasis" [88 78]] ["swamp" [66 54]]
   ["bandit-camp" [18 18]] ["settlement" [29 25]]])

(defn choose-cell [[x y]]
  (first (sort-by #(let [[cx cy] %] (+ (abs (- x cx)) (abs (- y cy))))
                  (for [cy (range (max 0 (- y 5)) (min 100 (+ y 6)))
                        cx (range (max 0 (- x 5)) (min 100 (+ x 6)))
                        :when (not (layout/initial-objects [cx cy]))] [cx cy]))))

(defn player [team [x y] name]
  {:x x :y y :team team :name name :hp 100 :max-hp 100})

(defn demo-world []
  (with-open [listener (ServerSocket. 0 16 (InetAddress/getLoopbackAddress))]
    (let [acceptor (future (server/serve! listener))]
      (try
        (with-open [socket (Socket. "127.0.0.1" (.getLocalPort listener))]
          (.setSoTimeout socket 5000)
          (let [connection {:socket socket :reader (io/reader socket :encoding "UTF-8")
                            :writer (io/writer socket :encoding "UTF-8")}]
            (assert (= :ok (:status (client/request connection
                                      {:type :register :player-id 1 :name "Blue" :client-kind :human}))))
            (assert (world/add-player 2 "Prolog" :red))
            ;; Stage the actors at a crossing, then capture the normal TCP state reply.
            (dosync
              (alter world/world update-in [:players 1] merge (player :blue (choose-cell [46 25]) "Blue"))
              (alter world/world update-in [:players 2] merge (player :red (choose-cell [56 25]) "Prolog"))
              (let [id (first (keys (:bandits @world/world)))]
                (alter world/world update-in [:bandits id] merge
                       (player :bandits (choose-cell [44 22]) "Бандит")
                       {:state :patrol :next-move Long/MAX_VALUE})
                (alter world/world update-in [:bandits id] dissoc :respawn-at)))
            (:world (client/request connection {:type :state}))))
        (finally (.close listener) (deref acceptor 3000 nil))))))

(defn overview []
  (let [image (BufferedImage. 1000 1000 BufferedImage/TYPE_INT_RGB)
        g (.createGraphics image)]
    (try
      (.setRenderingHint g RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
      (doseq [y (range 0 100 10) x (range 0 100 10)]
        (let [section (rendering/render-scene {} layout/initial-objects
                                              {:x (* x 64) :y (* y 64) :width 640 :height 640})]
          (.drawImage g section (* x 10) (* y 10) 100 100 nil)))
      (finally (.dispose g)))
    (write-image image "overview")))

(defn previews [full-world]
  (edt!
   (fn []
     (let [frame (JFrame. "Settlement · World V4.1")
           panel (gui/create-map-panel 24 25 1)
           ui (gui/create-resource-panel {})
           label (doto (JLabel. "B — строительство · F — атака · R — ремонт")
                   (.setForeground (Color. 240 230 193)) (.setPreferredSize (Dimension. 600 40)))
           progress (doto (JProgressBar.) (.setVisible false))
           content (:panel (gui/create-game-content panel ui progress label))
           device (.getDefaultScreenDevice (GraphicsEnvironment/getLocalGraphicsEnvironment))]
       (try
         (.setUndecorated frame true)
         (.setContentPane frame panel)
         (.setSize frame 896 640)
         (.setVisible frame true)
         (doseq [[name cell] regions]
           (let [near (choose-cell cell)
                 npc (when (= name "bandit-camp") {3 (player :bandits (choose-cell [20 18]) "Бандит")})
                 objects (if (= name "settlement")
                           (assoc layout/initial-objects [29 25]
                                  (buildings/initialize-durability {:type :building :team :blue :kind :house :owner-id 1}))
                           layout/initial-objects)]
             (gui/update-map panel {1 (player :blue near "Blue")} objects npc)
             (write-image (capture panel [896 640]) name)))
         (.add content panel BorderLayout/CENTER)
         (.setContentPane frame content)
         (.setFullScreenWindow device frame)
         (.validate frame)
         (let [{:keys [players objects bandits]} full-world]
           (gui/update-map panel players objects bandits)
           (gui/update-player-panels! ui full-world 1)
           (write-image (capture content [(.getWidth content) (.getHeight content)]) "fullscreen")
           (println {:swing-fullscreen [(.getWidth content) (.getHeight content)]}))
         (finally
           (.setFullScreenWindow device nil)
           (.dispose frame)))))))

(defn benchmark [[w h]]
  (let [state (volatile! nil)
        target (volatile! nil)
        samples (vec
                 (for [i (range 56)]
                   (let [player {:x (+ 43 (* 0.16 (mod i 20))) :y 25 :team :blue}
                         camera (rendering/camera player w h)
                         start (System/nanoTime)]
                     (vswap! state rendering/prepare-viewport layout/initial-objects camera nil)
                     (vswap! target rendering/render-viewport {1 player} camera {} @state (quot i 8))
                     (/ (- (System/nanoTime) start) 1e6))))
        ordered (vec (sort (drop 8 samples)))]
    {:size [w h] :median-ms (nth ordered 24) :p95-ms (nth ordered 45)
     :max-ms (last ordered)}))

(when-not (= ["--bench-only"] (vec *command-line-args*))
  (previews (demo-world))
  (overview))
(doseq [size [[1280 720] [1920 1080] [2560 1440]]]
  (println (benchmark size)))
(shutdown-agents)
