(require '[clojure.java.io :as io]
         '[settlement.building-ui :as building-ui]
         '[settlement.client :as client]
         '[settlement.gui :as gui]
         '[settlement.layout :as layout]
         '[settlement.rendering :as rendering]
         '[settlement.server :as server]
         '[settlement.transport :as transport]
         '[settlement.world :as world])
(import '[java.awt Color Dimension]
        '[java.awt.event KeyEvent]
        '[java.awt.image BufferedImage]
        '[java.net InetAddress ServerSocket Socket SocketException]
        '[java.util.concurrent TimeUnit]
        '[javax.imageio ImageIO]
        '[javax.swing JFrame JLabel JPanel JProgressBar KeyStroke SwingUtilities Timer])

(def frame-dir (io/file (first *command-line-args*)))
(.mkdirs frame-dir)
(def human-id 7781)
(def bot-id 7782)

(defn edt! [f]
  (let [result (promise)]
    (SwingUtilities/invokeAndWait #(deliver result (f)))
    @result))

(defn connect [port]
  (let [socket (doto (Socket. "127.0.0.1" port) (.setSoTimeout 5000))
        connection {:socket socket :reader (io/reader (.getInputStream socket) :encoding "UTF-8")
                    :writer (io/writer (.getOutputStream socket) :encoding "UTF-8")}]
    (assoc connection :queue (transport/create-queue #(client/request connection %) #(.close socket)))))

(defn keyboard! [terrain code release?]
  (let [key (.get (.getInputMap terrain JPanel/WHEN_IN_FOCUSED_WINDOW)
                  (KeyStroke/getKeyStroke code 0 release?))]
    (.actionPerformed (.get (.getActionMap terrain) key) nil)))

(defn show! [connection]
  (let [window (JFrame. "Settlement - movement")
        terrain (gui/create-map-panel 43 25 human-id)
        ui (gui/create-resource-panel {})
        progress (doto (JProgressBar.) (.setVisible false))
        label (doto (JLabel. building-ui/controls-text)
                (.setForeground (Color. 240 230 193)) (.setPreferredSize (Dimension. 600 40)))
        {:keys [panel]} (gui/create-game-content terrain ui progress label)
        movement (gui/bind-movement! terrain connection ui human-id)
        moves (Timer. 16 (reify java.awt.event.ActionListener
                            (actionPerformed [_ _] ((:tick! movement)))))
        snapshots (gui/start-map-updater terrain connection ui human-id)]
    (.setContentPane window panel)
    (.setSize window 784 539)
    (.setLocationRelativeTo window nil)
    (.setVisible window true)
    (.validate window)
    (.start moves)
    (gui/refresh! terrain connection ui human-id)
    {:window window :terrain terrain :panel panel :moves moves :snapshots snapshots}))

(defn percentile [samples p] (nth (vec (sort samples)) (dec (int (Math/ceil (* p (count samples)))))))

(defn record! [{:keys [panel terrain]}]
  (let [start (System/nanoTime)
        width (edt! #(.getWidth panel))
        height (edt! #(.getHeight panel))
        measurements
        (loop [index 0 phase :ready samples []]
          (if (= 144 index)
            samples
            (let [remaining (- (+ start (* index 40000000)) (System/nanoTime))]
              (when (pos? remaining) (.sleep TimeUnit/NANOSECONDS remaining))
              (let [image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
                    [next-phase ms players]
                    (edt!
                     (fn []
                       (let [players (:players (.getClientProperty terrain :settlement.gui/state))
                             x (get-in players [human-id :x] 43)
                             phase (cond
                                     (and (= phase :ready) (= index 7))
                                     (do (keyboard! terrain KeyEvent/VK_D false) :east)
                                     (and (= phase :east) (or (>= x 50) (= index 70)))
                                     (do (keyboard! terrain KeyEvent/VK_D true) :pause)
                                     (and (= phase :pause) (> index 73))
                                     (do (keyboard! terrain KeyEvent/VK_A false) :west)
                                     (and (= phase :west) (<= x 44))
                                     (do (keyboard! terrain KeyEvent/VK_A true) :stopped)
                                     :else phase)
                             graphics (.createGraphics image)
                             now (System/nanoTime)]
                         (try (.printAll panel graphics) (finally (.dispose graphics)))
                         [phase (/ (- (System/nanoTime) now) 1e6) players])))]
                (recur (inc index) next-phase (conj samples {:image image :ms ms :players players}))))))]
    (doseq [[i {:keys [image]}] (map-indexed vector measurements)]
      (ImageIO/write image "png" (io/file frame-dir (format "frame-%03d.png" i))))
    (let [human-xs (keep #(get-in % [:players human-id :x]) measurements)
          bot-cells (set (keep #(when-let [player (get-in % [:players bot-id])] [(:x player) (:y player)])
                               measurements))]
      (assert (and (<= (apply min human-xs) 43) (>= (apply max human-xs) 50)) human-xs)
      (assert (> (count bot-cells) 1) bot-cells)
      (println {:frames (count measurements) :size [width height]
                :human-x [(apply min human-xs) (apply max human-xs)] :bot-cells (sort bot-cells)
                :swing-capture-ms-p50 (percentile (map :ms measurements) 0.5)
                :swing-capture-ms-p95 (percentile (map :ms measurements) 0.95)}))))

(defn benchmark! []
  (let [players {human-id {:x 47.0 :y 25 :team :blue :name "Blue"}
                 bot-id {:x 45.0 :y 24 :team :red :name "Prolog"}}]
    (doseq [[width height] [[896 640] [1280 720] [1920 1080]]]
      (let [prepared (atom nil)
            frame (atom nil)
            render (fn [index]
                     (let [players (update-in players [human-id :x] + (* index 0.064))
                           camera (rendering/camera (players human-id) width height)]
                       (swap! prepared rendering/prepare-viewport layout/initial-objects camera {})
                       (swap! frame rendering/render-viewport players camera {} @prepared)))
            _ (dotimes [i 24] (render i))
            samples (doall (for [i (range 100)]
                             (let [now (System/nanoTime)]
                               (render i)
                               (/ (- (System/nanoTime) now) 1e6))))]
        (println {:viewport [width height] :frames 100
                  :render-ms-p50 (percentile samples 0.5)
                  :render-ms-p95 (percentile samples 0.95)
                  :render-ms-p99 (percentile samples 0.99)})))))

(try
  (with-redefs [layout/spawn-cells (fn [] [[43 25]])
                layout/red-spawn-cells (delay [[42 26]])]
    (with-open [listener (ServerSocket. 0 16 (InetAddress/getLoopbackAddress))]
      (let [port (.getLocalPort listener)
            acceptor (future (try (loop [] (let [socket (.accept listener)]
                                             (future (server/handle-client socket))) (recur))
                                  (catch SocketException error
                                    (when-not (.isClosed listener) (throw error)))))
            connection (connect port)
            process (ProcessBuilder. ["swipl.exe" "-q" "-s" "prolog/settlement_bot.pl" "--"
                                      (str bot-id) "200" "20"])]
        (try
          (assert (= :ok (:status (client/request connection {:type :register :client-kind :human
                                                              :player-id human-id :name "Blue"}))))
          (.put (.environment process) "SETTLEMENT_PORT" (str port))
          (.redirectErrorStream process true)
          (.redirectOutput process (io/file frame-dir "bot.log"))
          (let [view (edt! #(show! connection))
                bot (.start process)]
            (try (record! view)
                 (finally
                   (edt! #(do (.stop (:moves view)) (.stop (:snapshots view)) (.dispose (:window view))))
                   (.destroy bot)
                   (when-not (.waitFor bot 3 TimeUnit/SECONDS) (.destroyForcibly bot)))))
          (finally (client/close-connection connection) (.close listener) (deref acceptor 3000 nil))))))
  (benchmark!)
  (finally (shutdown-agents)))
