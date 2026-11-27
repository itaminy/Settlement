(require '[clojure.java.io :as io]
         '[settlement.buildings :as buildings]
         '[settlement.client :as client]
         '[settlement.gui :as gui]
         '[settlement.landscape :as landscape]
         '[settlement.layout :as layout]
         '[settlement.rendering :as rendering]
         '[settlement.server :as server]
         '[settlement.world :as world]
         '[settlement.visual-v5 :as v5]
         '[settlement.visual-v6 :as v6])
(import '[java.awt BorderLayout Color Dimension GraphicsEnvironment RenderingHints]
        '[java.awt.image BufferedImage]
        '[java.net InetAddress ServerSocket Socket]
        '[javax.imageio ImageIO]
        '[javax.swing JFrame JLabel JPanel JProgressBar SwingUtilities])

(def v6-capture? (= ["world-v6"] (vec *command-line-args*)))
(def output (io/file (if v6-capture? "docs/previews/world_v6" "docs/previews/world_v5")))
(.mkdirs output)
(def prefix (cond (and v6-capture? (not v6/enabled?)) "v5-"
                  (not v5/enabled?) "v41-"))


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
  (ImageIO/write image "png" (io/file output (str prefix name ".png"))))

(def regions
  (if v6-capture?
    [["conifer" [12 6]] ["forest" [36 8]] ["clearing" [23 9]]
     ["mountains" [13 63]] ["gorge" [13 69]] ["mountain-pass" [27 64]]
     ["snow" [19 86]] ["dunes" [92 70]] ["canyon" [85 78]]
     ["ruins" [83 92]] ["oasis" [90 79]] ["swamp" [65 56]]
     ["swamp-crossing" [70 59]] ["river" [48 25]] ["lake" [91 55]]
     ["waterfall" [51 9]] ["settlement" [32 26]] ["bandit-camp" [18 18]]
     ["transition" [58 58]]]
    [["forest" [36 8]] ["conifer" [12 6]] ["clearing" [23 9]] ["desert" [89 88]]
   ["canyon" [83 75]] ["oasis" [90 79]] ["swamp" [67 53]]
   ["swamp-crossing" [70 59]] ["mountains" [15 62]] ["mountain-pass" [21 63]]
   ["snow" [20 87]] ["river" [48 25]] ["lake" [91 55]] ["waterfall" [51 9]]
   ["crossroads" [24 25]] ["settlement" [32 26]] ["bandit-camp" [18 18]]]))

(defn behind-ridge? [[x y]]
  (let [px (+ (* x 64) 32)
        py (+ (* y 64) 22)]
    (some (fn [[p [kind]]]
            (when (#{:massif :escarpment :snow-ridge :sand-ridge} (layout/initial-objects p))
              (let [{:keys [image width height foot-y] top :y left :x}
                    (rendering/sprite-placement p kind)]
                (and (<= (+ py 26) foot-y) (<= left px (dec (+ left width)))
                     (<= top py (dec (+ top height)))
                     (not (zero? (unsigned-bit-shift-right (.getRGB ^BufferedImage image (- px left) (- py top)) 24)))))))
          landscape/landmarks)))

(defn choose-cell [[x y]]
  (first (sort-by #(let [[cx cy] %]
                     [(if (behind-ridge? %) 1 0) (+ (abs (- x cx)) (abs (- y cy)))])
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

(defn- overview-section [x y]
  (if v6-capture?
    (edt! (fn []
            ;; Choose a fallback camera center so the viewport aligns exactly with the chunk.
            (let [panel (doto (gui/create-map-panel (+ x 4.5) (+ y (/ 283 64.0)))
                          (.setSize 640 640))]
              (gui/update-map panel {} layout/initial-objects)
              (capture panel [640 640]))))
    (rendering/render-scene {} layout/initial-objects
                            {:x (* x 64) :y (* y 64) :width 640 :height 640})))

(defn overview []
  (let [image (BufferedImage. 1000 1000 BufferedImage/TYPE_INT_RGB)
        g (.createGraphics image)]
    (try
      (.setRenderingHint g RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
      (doseq [y (range 0 100 10) x (range 0 100 10)]
        (let [section (overview-section x y)]
          (.drawImage g section (* x 10) (* y 10) 100 100 nil)))
      (finally (.dispose g)))
    (write-image image "overview")))

(defn previews [full-world]
  (edt!
   (fn []
     (let [frame (JFrame. "Settlement | World")
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
             (let [[w h :as size] (cond
                                     (and v6-capture? (= name "canyon")) [1536 1152]
                                     (and v6-capture? (#{"mountains" "gorge" "snow"} name)) [1152 768]
                                     :else [896 640])]
               (.setSize frame w h)
               (.validate frame)
               (gui/update-map panel {1 (player :blue near "Blue")} objects npc)
               (write-image (capture panel size) name))))
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

(previews (demo-world))
(overview)
(shutdown-agents)
