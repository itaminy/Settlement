(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[settlement.client :as client]
         '[settlement.gui :as gui]
         '[settlement.layout :as layout]
         '[settlement.massive-mountain :as mountain]
         '[settlement.mountain-lake :as lake]
         '[settlement.server :as server])
(import '[java.awt GraphicsEnvironment]
        '[java.awt.image BufferedImage]
        '[java.net InetAddress ServerSocket Socket]
        '[java.util.concurrent TimeUnit]
        '[javax.imageio ImageIO]
        '[javax.swing JFrame JLabel JProgressBar SwingUtilities])

(def natural? (some #{"--natural"} *command-line-args*))
(def output (io/file (str "docs/previews/world_v" (if natural? "6_4_2" (or mountain/art-version "6_1")))))
(def frames (io/file (if natural? ".natural-mountain-frames"
                      (case mountain/art-version
                        "6_3_1" ".mountain-v631-frames"
                        "6_3" ".mountain-v63-frames"
                        "6_2" ".mountain-v62-frames"
                        ".mountain-frames"))))
(.mkdirs output)

(defn edt! [f]
  (let [result (volatile! nil)]
    (SwingUtilities/invokeAndWait #(vreset! result (f)))
    @result))

(defn request! [connection command]
  (let [result (client/request connection command)]
    (when-not (= :ok (:status result))
      (throw (ex-info "Mountain TCP request rejected" {:command command :reply result})))
    result))

(defn direction [from to]
  ({[0 -1] :up [0 1] :down [-1 0] :left [1 0] :right} (mapv - to from)))

(defn walk-path [objects origin goal]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY origin), parents {origin nil}]
    (let [p (peek queue)]
      (assert p [:unreachable goal])
      (if (= p goal)
        (reverse (take-while some? (iterate parents p)))
        (let [neighbors (remove #(or (contains? parents %) (objects %)) (layout/neighbors p))]
          (recur (into (pop queue) neighbors) (into parents (map #(vector % p)) neighbors)))))))

(defn copy-frame [panel file]
  (edt! #(let [image (BufferedImage. (.getWidth panel) (.getHeight panel) BufferedImage/TYPE_INT_RGB)
               g (.createGraphics image)]
           (try (.printAll panel g) (finally (.dispose g)))
           (ImageIO/write image "png" file))))

(defn view! [panel ui snapshot]
  (edt! #(do
           (gui/update-map panel (:players snapshot) (:objects snapshot) (:bandits snapshot))
           (gui/update-player-panels! ui snapshot 1))))

(defn fullscreen! [window panel target]
  (let [device (.getDefaultScreenDevice (GraphicsEnvironment/getLocalGraphicsEnvironment))]
    (try
      (edt! #(do (.setFullScreenWindow device window) (.validate window)))
      (copy-frame panel (io/file output target))
      (finally
        (edt! #(do (.setFullScreenWindow device nil) (.setSize window 896 640) (.validate window)))))))

(defn check-prolog! [port]
  (let [[x y] mountain/summit
        goal (str "atom_string('Mountain A star',Name),fetch_snapshot(9100,register(Name),_,Line),"
                  "parse_frame(Line,ok(Frame)),load_snapshot(Frame),"
                  "get_time(T0),path_to(cell(" x "," y "),Route),is_list(Route),"
                  "get_time(T1),Ms is (T1-T0)*1000,length(Route,N),"
                  "format('A* summit: ~d steps in ~2f ms~n',[N,Ms]),halt")
        process (ProcessBuilder. ["swipl.exe" "-q" "-l" "prolog/smoke_test.pl"
                                  "-l" "prolog/settlement_pathfinding.pl" "-g" goal])]
    (.put (.environment process) "SETTLEMENT_PORT" (str port))
    (let [child (.start (.inheritIO process))]
      (when-not (and (.waitFor child 20 TimeUnit/SECONDS) (zero? (.exitValue child)))
        (.destroyForcibly child)
        (throw (ex-info "Prolog could not plan to the summit" {}))))))

(defn capture-route! [port]
  (.mkdirs frames)
  (with-open [socket (doto (Socket. "127.0.0.1" port) (.setSoTimeout 5000))]
    (let [connection {:socket socket :reader (io/reader socket :encoding "UTF-8")
                      :writer (io/writer socket :encoding "UTF-8")}
          _ (request! connection {:type :register :player-id 1 :name "Blue" :client-kind :human})
          snapshot #(:world (request! connection {:type :state}))
          move! (fn [from to]
                  (request! connection {:type :move :player-id 1 :direction (direction from to)}))
          state (snapshot)
          current ((juxt :x :y) (get-in state [:players 1]))
          approach (walk-path (:objects state) current mountain/entrance)
          [panel ui window content]
          (edt! (fn []
                  (let [panel (gui/create-map-panel (first current) (second current) 1)
                        ui (gui/create-resource-panel {})
                        content (:panel (gui/create-game-content panel ui
                                                       (doto (JProgressBar.) (.setVisible false)) (JLabel.)))
                        window (doto (JFrame. "Settlement | Massive Mountain")
                                 (.setUndecorated true) (.setContentPane content)
                                 (.setSize 896 640) (.setVisible true))]
                    (when natural? (gui/set-map-scenery! panel {:terrain-relief {:scene :mountain :view :lit :revision :natural}}))
                    [panel ui window content])))
          route mountain/trail
          polish? (= "6_3_1" mountain/art-version)
          highlands (- (count route) 13)
          summit (dec (count route))
          fullscreen-shots (cond-> {0 "foothills" summit "summit"}
                             polish? (assoc 39 "slope" 47 "gorge" highlands "highlands"))
          clips (merge {0 "foothills" 11 "lower-slope" (if polish? 47 27)
                        (if (#{"6_3" "6_3_1"} mountain/art-version) "gorge" "switchbacks")
                        (- (count route) 13) "highlands" (dec (count route)) "summit"}
                       (into {} (map-indexed (fn [i p] [(.indexOf route p) (str "camp-" (inc i))])
                                             mountain/camps)))
          checked-wall? (volatile! false)]
      (try
        (doseq [[from to] (partition 2 1 approach)] (move! from to))
        (doseq [[i cell] (map-indexed vector route)]
          (when (pos? i) (move! (nth route (dec i)) cell))
          (let [state (snapshot)]
            (assert (= cell ((juxt :x :y) (get-in state [:players 1]))) [:position i])
            (when-not @checked-wall?
              (when-let [blocked (first (filter mountain/blocks (layout/neighbors cell)))]
                (let [reply (client/request connection {:type :move :player-id 1 :direction (direction cell blocked)})]
                  (assert (= :object-blocked (:code reply)) reply)
                  (vreset! checked-wall? true))))
            (view! panel ui state))
          (doseq [phase (range 3)]
            (Thread/sleep 78)
            (copy-frame content (io/file frames (format "frame-%04d.png" (+ (* i 3) phase)))))
          (when-let [name (when-not natural? (clips i))]
            (copy-frame content (io/file output (str name ".png"))))
          (when-let [name (when-not natural? (fullscreen-shots i))]
            (fullscreen! window content (str "fullscreen-" name ".png"))))
        (assert @checked-wall?)
        (when natural?
          (doseq [[from to] (partition 2 1 (rseq route))] (move! from to))
          (assert (= mountain/entrance ((juxt :x :y) (get-in (snapshot) [:players 1])))))
        (println {:approach (dec (count approach)) :ascent (dec (count route))
                  :reverse-verified (boolean natural?)
                  :wall-collision @checked-wall? :summit mountain/summit :swing-frames (* 3 (count route))})
        (finally (edt! #(.dispose window)))))))

(defn verify-lake-route! [port]
  (with-open [socket (doto (Socket. "127.0.0.1" port) (.setSoTimeout 5000))]
    (let [connection {:socket socket :reader (io/reader socket :encoding "UTF-8")
                      :writer (io/writer socket :encoding "UTF-8")}
          _ (request! connection {:type :register :player-id 1 :name "Lake route" :client-kind :human})
          state #(:world (request! connection {:type :state}))
          snapshot (state)
          position (fn [s] ((juxt :x :y) (get-in s [:players 1])))
          start (position snapshot)
          objects (:objects snapshot)
          reachable (layout/reachable-cells objects start)
          move (fn [from to] {:type :move :player-id 1 :direction (direction from to)})
          walk! (fn [path]
                  (doseq [[from to] (partition 2 1 path)]
                    (request! connection (move from to)))
                  (assert (= (last path) (position (state)))))
          refuse! (fn [from to]
                    (assert (= :object-blocked (:code (client/request connection (move from to)))))
                    (assert (= from (position (state)))))
          [bank water] (first (sort (for [p reachable :when (lake/local-cell? p)
                                         n (layout/neighbors p)
                                         :when (and (lake/local-cell? n) (= :water (objects n)))]
                                     [p n])))
          [ledge wall] (first (for [p mountain/trail n (layout/neighbors p)
                                    :when (= :terrain-blocked (objects n))]
                                [p n]))]
      (assert (and bank water ledge wall) "Water outlet and exposed rock wall must be testable")
      (walk! (walk-path objects start bank))
      (refuse! bank water)
      (walk! (walk-path objects bank mountain/entrance))
      (let [ledge-index (.indexOf mountain/trail ledge)]
        (walk! (subvec mountain/trail 0 (inc ledge-index)))
        (refuse! ledge wall)
        (walk! (subvec mountain/trail ledge-index)))
      (println {:water [bank water] :wall [ledge wall] :ascent (dec (count mountain/trail))
                :summit (position (state))}))))

(defn baseline! []
  (let [{:keys [trail camps]} (edn/read-string (slurp (io/resource "massive_mountain.edn")))
        views [["foothills" (first trail)] ["camp-1" (first camps)]
               ["highlands" (nth trail (- (count trail) 13))] ["summit" (peek trail)]]
        window (edt! #(doto (JFrame. "Settlement | World V6") (.setUndecorated true)))]
    (try
      (doseq [[name [x y]] views]
        (let [content
              (edt! (fn []
                      (let [panel (gui/create-map-panel x y 1)
                            ui (gui/create-resource-panel {})
                            content (:panel (gui/create-game-content panel ui (JProgressBar.) (JLabel.)))]
                        (gui/update-map panel {} layout/initial-objects)
                        (.setContentPane window content)
                        (.setSize window 896 640)
                        (.setVisible window true)
                        (.validate window)
                        content)))]
          (copy-frame content (io/file output (str "v6-" name ".png")))))
      (finally (edt! #(.dispose window))))))

(defn prolog-walk! [port]
  (let [[x y] mountain/summit
        process (ProcessBuilder. ["swipl.exe" "-q" "-s" "scripts/check_mountain_walk.pl"
                                  "--" (str x) (str y)])]
    (.put (.environment process) "SETTLEMENT_PORT" (str port))
    (let [child (.start (.inheritIO process))]
      (when-not (and (.waitFor child 110 TimeUnit/SECONDS) (zero? (.exitValue child)))
        (.destroyForcibly child)
        (throw (ex-info "Prolog could not walk to the summit" {}))))))

(if (= ["--baseline"] (vec *command-line-args*))
  (do (assert (not mountain/enabled?) "Select World V6 when capturing the baseline")
      (baseline!))
  (do
    (assert mountain/enabled? "Massive Mountain is required to test the ascent")
    (with-open [listener (ServerSocket. 0 16 (InetAddress/getLoopbackAddress))]
      (let [acceptor (future (server/serve! listener))
            port (.getLocalPort listener)]
        (try
          (case (first *command-line-args*)
            "--prolog-walk" (prolog-walk! port)
            "--verify-lake" (verify-lake-route! port)
            "--verify" (do (check-prolog! port) (verify-lake-route! port))
            (do (check-prolog! port) (capture-route! port)))
          (finally (.close listener) (deref acceptor 3000 nil)))))))
(shutdown-agents)
