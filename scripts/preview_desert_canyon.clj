(require '[clojure.java.io :as io]
         '[settlement.client :as client]
         '[settlement.desert-canyon :as canyon]
         '[settlement.gui :as gui]
         '[settlement.layout :as layout]
         '[settlement.server :as server])
(import '[java.awt GraphicsEnvironment]
        '[java.awt.image BufferedImage]
        '[javax.imageio ImageIO]
        '[javax.swing JFrame JLabel JProgressBar SwingUtilities]
        '[java.net InetAddress ServerSocket Socket]
        '[java.util.concurrent TimeUnit])

(def natural? (some #{"--natural"} *command-line-args*))
(def output (io/file (if natural? "docs/previews/world_v6_4_2" "docs/previews/world_v6_4")))
(def frames (io/file (if natural? ".natural-canyon-frames" ".canyon-frames")))
(.mkdirs output)

(defn edt [f]
  (let [result (volatile! nil)]
    (SwingUtilities/invokeAndWait #(vreset! result (f)))
    @result))

(defn game-view [window [x y] player-id]
  (edt (fn []
         (let [map (gui/create-map-panel x y player-id)
               ui (gui/create-resource-panel {})
               content (:panel (gui/create-game-content map ui (doto (JProgressBar.) (.setVisible false)) (JLabel.)))]
           (.setContentPane window content)
           (when natural? (gui/set-map-scenery! map {:terrain-relief {:scene :canyon :view :lit :revision :natural}}))
           {:map map :ui ui :panel content}))))

(defn refresh! [{:keys [map ui]} snapshot]
  (edt #(do (gui/update-map map (:players snapshot) (:objects snapshot) (:bandits snapshot))
            (when (get-in snapshot [:players 1])
              (gui/update-player-panels! ui snapshot 1)))))

(defn snapshot! [window {:keys [panel]} file width height fullscreen?]
  (edt #(let [device (.getDefaultScreenDevice (GraphicsEnvironment/getLocalGraphicsEnvironment))]
          (.setSize window width height)
          (.setVisible window true)
          (try
            (when fullscreen? (.setFullScreenWindow device window))
            (.validate window)
            (let [image (BufferedImage. (.getWidth panel) (.getHeight panel) BufferedImage/TYPE_INT_RGB)
                  g (.createGraphics image)]
              (try (.printAll panel g) (finally (.dispose g)))
              (ImageIO/write image "png" file))
            (finally (when fullscreen? (.setFullScreenWindow device nil)))))))

(defn views! [window before? snapshot]
  (doseq [[name center w h fullscreen?]
          (if before?
            [["canyon-before" [86 81] 1990 2300 false]]
            [["canyon-overview" [86 81] 1990 2300 false]
             ["canyon-entrance" [76 67] 960 640 false]
             ["deep-gorge" [88 68] 960 640 false]
             ["canyon-bridge" [95 81] 960 640 false]
             ["canyon-camp-1" [81 69] 800 560 false]
             ["canyon-camp-2" [94 76] 800 560 false]
             ["canyon-camp-3" [89 94] 800 560 false]
             ["canyon-ruins" [86 85] 960 640 false]
             ["canyon-oasis" [89 79] 960 640 false]
             ["fullscreen" [90 73] 960 640 true]])]
    (let [view (game-view window center nil)]
      (refresh! view snapshot)
      (snapshot! window view (io/file output (str name ".png")) w h fullscreen?))))

(defn request! [connection command]
  (let [reply (client/request connection command)]
    (when-not (= :ok (:status reply))
      (throw (ex-info "Unexpected TCP reply" {:command command :reply reply})))
    reply))

(defn path [objects origin goal]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY origin), previous {origin nil}]
    (let [p (peek queue)]
      (assert p [:unreachable goal])
      (if (= p goal)
        (reverse (take-while some? (iterate previous p)))
        (let [options (remove #(or (previous %) (= origin %) (objects %)) (layout/neighbors p))]
          (recur (into (pop queue) options) (into previous (map #(vector % p) options))))))))

(defn move [a b]
  {:type :move :player-id 1
   :direction ({[1 0] :right [-1 0] :left [0 1] :down [0 -1] :up} (mapv - b a))})

(defn explore! [port window]
  (when window (.mkdirs frames))
  (with-open [socket (doto (Socket. "127.0.0.1" port) (.setSoTimeout 5000))]
    (let [connection {:socket socket :reader (io/reader socket :encoding "UTF-8")
                      :writer (io/writer socket :encoding "UTF-8")}
          _ (request! connection {:type :register :player-id 1 :name "Blue" :client-kind :human})
          state #(:world (request! connection {:type :state}))
          position #((juxt :x :y) (get-in % [:players 1]))
          initial (state)
          objects (:objects initial)
          approach (path objects (position initial) (first canyon/trail))
          bridge-hits (volatile! #{})
          refused (volatile! #{})
          view (when window (game-view window (position initial) 1))]
      (doseq [[a b] (partition 2 1 approach)] (request! connection (move a b)))
      (doseq [[i p] (map-indexed vector canyon/trail)]
        (when (pos? i) (request! connection (move (canyon/trail (dec i)) p)))
        (assert (= p (position (state))) p)
        (when (canyon/bridges p) (vswap! bridge-hits conj p))
        (doseq [[kind blockers] [[:wall canyon/walls] [:pit canyon/pits]]
                :let [target (first (filter blockers (layout/neighbors p)))]
                :when (and target (not (@refused kind)))]
          (assert (= :object-blocked (:code (client/request connection (move p target)))))
          (assert (= p (position (state))))
          (vswap! refused conj kind))
        (when window
          (refresh! view (state))
          (dotimes [phase 3]
            (Thread/sleep 72)
            (snapshot! window view (io/file frames (format "frame-%04d.png" (+ (* i 3) phase))) 960 640 false))))
      (assert (= canyon/bridges @bridge-hits))
      (assert (= #{:wall :pit} @refused))
      (let [exit (position (state))]
        (when (or natural? (nil? window))
          (doseq [[a b] (partition 2 1 (rseq canyon/trail))]
            (request! connection (move a b)))
          (assert (= (first canyon/trail) (position (state)))))
        (println {:approach (dec (count approach)) :canyon-steps (dec (count canyon/trail))
                  :bridge-steps (count @bridge-hits) :refused @refused :exit exit
                  :reverse-verified (or (boolean natural?) (nil? window))})))))

(defn verify-prolog! [port]
  (let [process (ProcessBuilder. ["swipl.exe" "-q" "-s" "scripts/check_canyon_path.pl"])]
    (.put (.environment process) "SETTLEMENT_PORT" (str port))
    (let [child (.start (.inheritIO process))]
      (when-not (and (.waitFor child 30 TimeUnit/SECONDS) (zero? (.exitValue child)))
        (.destroyForcibly child)
        (throw (ex-info "Prolog canyon path check failed" {}))))))

(let [before? (= ["--before"] (vec *command-line-args*))
      walk? (some #{"--walk"} *command-line-args*)
      verify? (some #{"--verify"} *command-line-args*)
      prolog? (= ["--prolog"] (vec *command-line-args*))
      window (edt #(doto (JFrame. "Settlement | Massive Desert Canyon") (.setUndecorated true)))]
  (try
    (if (or walk? prolog? verify?)
      (with-open [listener (ServerSocket. 0 8 (InetAddress/getLoopbackAddress))]
        (let [acceptor (future (server/serve! listener))]
          (try (cond verify? (do (verify-prolog! (.getLocalPort listener))
                                 (explore! (.getLocalPort listener) nil))
                     prolog? (verify-prolog! (.getLocalPort listener))
                     :else (explore! (.getLocalPort listener) window))
               (finally (.close listener) (deref acceptor 3000 nil)))))
      (views! window before? {:players {} :objects layout/initial-objects :bandits {}}))
    (finally (edt #(.dispose window)))))
(shutdown-agents)
