(ns settlement.effects
  (:require [settlement.bandits :as bandits]
            [settlement.buildings :as buildings]
            [settlement.config :as config]
            [settlement.ui-theme :as theme])
  (:import [java.awt Color Font Graphics2D]))

(def limit 32)
(def ^:private lifetimes
  {:damage 780 :repair 880 :construction 800 :debris 900 :death 870 :respawn 1000
   :harvest 1050 :swing 300 :gather 4400})

(defn active [events now]
  (into [] (filter #(< now (:until %))) events))

(defn enqueue [events now additions]
  (->> additions
       (into (active events now) (map #(assoc % :at now :until (+ now (lifetimes (:kind %))))))
       (take-last limit) vec))

(defn damage-tier [{:keys [hp max-hp]}]
  (when (and (number? hp) (pos? (or max-hp 0)))
    (cond (<= hp 0) :destroyed
          (> (* 100 hp) (* 75 max-hp)) :normal
          (> (* 100 hp) (* 50 max-hp)) :light
          (> (* 100 hp) (* 25 max-hp)) :heavy
          :else :critical)))

(defn- entity-cell [{:keys [x y]}] [x y])

(defn- building-center [[x y] building]
  (let [[w h] (get-in buildings/catalog [(:kind building) :size])]
    [(+ x (/ (dec w) 2.0)) (+ y (/ (dec h) 2.0))]))

(defn- building-effects [before after]
  (concat
    (keep (fn [[cell object]]
            (when (buildings/building? object)
              (let [old (before cell)
                    same? (and (buildings/building? old) (= (:building-id old) (:building-id object)))
                    center (building-center cell object)
                    delta (when (and same? (:hp old) (:hp object)) (- (:hp object) (:hp old)))]
                (cond (not same?) {:kind :construction :cell center}
                      (neg? (or delta 0)) {:kind :damage :cell center :anchor cell :amount (- delta)}
                      (pos? (or delta 0)) {:kind :repair :cell center :amount delta})))) after)
    (keep (fn [[cell old]]
            (when (and (buildings/building? old)
                       (not= (:building-id old) (:building-id (after cell))))
              {:kind :debris :cell (building-center cell old) :amount (:hp old)})) before)))

(defn- entity-effects [before after living? default-hp]
  (mapcat (fn [[id new]]
            (when-let [old (before id)]
              (let [was (living? old) live (living? new)
                    damage (- (:hp old default-hp) (:hp new default-hp))]
                (cond
                  (and was (not live))
                  [{:kind :death :cell (entity-cell old) :amount (max 0 damage)}]
                  (and (not was) live)
                  [{:kind :respawn :cell (entity-cell new)}]
                  (and was live (pos? damage))
                  [{:kind :damage :cell (entity-cell new) :amount damage}])))) after))

(defn snapshot-effects [before after]
  (when (and before (= (:match-id before) (:match-id after)))
    (concat (when (not= (:objects before) (:objects after))
              (building-effects (:objects before) (:objects after)))
            (entity-effects (:players before) (:players after) bandits/standing? (:hp config/player-combat))
            (entity-effects (:bandits before) (:bandits after) bandits/alive? (:hp config/bandits)))))

(defn confirmed-effects [actor command reply]
  (when (= :ok (:status reply))
    (case command
      :gather-start (when (:position reply)
                      [{:kind :gather :cell (:position reply) :actor actor :resource (:resource reply)
                        :operation (:operation-id reply)}])
      :gather-finish (when (:completed reply)
                       [{:kind :harvest :cell (:position reply) :resource (:resource reply)
                         :amount (:amount reply)}])
      (:attack-bandit :attack-building)
      (when-let [[x y :as cell] (:anchor reply)]
        (into [{:kind :swing :cell cell :actor actor}]
              (keep-indexed (fn [i [resource amount]]
                              (when (pos? amount)
                                {:kind :harvest :cell [x (+ y (* i 0.45))] :resource resource :amount amount})))
              (:awards reply)))
      [])))

(defn now-ms [] (quot (System/nanoTime) 1000000))
(defn enabled? [panel] (not (false? (.getClientProperty panel ::enabled))))
(defn events [panel] (.getClientProperty panel ::events))

(defn toggle! [panel]
  (.putClientProperty panel ::enabled (not (enabled? panel)))
  (.putClientProperty panel ::events [])
  (.repaint panel))

(defn add! [panel additions]
  (when (and (enabled? panel) (seq additions))
    (.putClientProperty panel ::events (enqueue (events panel) (now-ms) additions))
    (.repaint panel)))

(defn clear-gather! [panel]
  (.putClientProperty panel ::events (into [] (remove #(= :gather (:kind %))) (events panel)))
  (.repaint panel))

(defn tick! [panel now]
  (when-let [queue (seq (events panel))]
    (.putClientProperty panel ::events (active queue now))
    true))

(def ^:private hues
  {:damage (Color. 255 111 99) :repair (Color. 130 234 169) :construction (Color. 250 207 140)
   :debris (Color. 168 117 76) :death (Color. 211 125 190) :respawn (Color. 165 225 245)
   :harvest (Color. 236 198 120) :gather (Color. 206 160 91) :swing (Color. 248 236 189)})

(defn- point [[x y] camera ox oy]
  [(int (+ ox (* x config/tile-size) 32 (- (:x camera))))
   (int (+ oy (* y config/tile-size) 43 (- (:y camera))))])

(defn- label! [^Graphics2D g text x y color]
  (let [x (- x (quot (.stringWidth (.getFontMetrics g) text) 2))]
    (.setColor g (Color. 31 30 30))
    (.drawString g text (int (inc x)) (int (inc y)))
    (.setColor g color)
    (.drawString g text (int x) (int y))))

(defn- particles! [^Graphics2D g color x y p radius n]
  (.setColor g color)
  (dotimes [i n]
    (let [a (* i 2.39996)
          force (+ 0.45 (* 0.15 (mod i 5)))
          px (int (+ x (* (Math/cos a) radius p force)))
          py (int (+ y (* (Math/sin a) 24 p) (* -45 p (- 1 p))))]
      (.fillRect g px py (if (even? i) 3 2) (if (zero? (mod i 3)) 3 2)))))

(defn- weapon! [^Graphics2D g players actor x y p resource camera ox oy]
  (when-let [entity (players actor)]
    (let [[ax ay] (point (entity-cell entity) camera ox oy)
          flip (if (< x ax) -1 1)
          a (- (* p 2.5) 1.6)
          hx (+ ax (* 14 flip))
          hy (- ay 4)
          tip-x (int (+ hx (* flip 25 (Math/cos a))))
          tip-y (int (+ hy (* 25 (Math/sin a))))]
      (.setColor g (Color. 66 43 33))
      (.drawLine g hx hy tip-x tip-y)
      (.drawLine g hx (inc hy) tip-x (inc tip-y))
      (.setColor g (Color. 227 225 206))
      (case resource
        :stone (.drawLine g (- tip-x 7) (+ tip-y 3) (+ tip-x 8) (- tip-y 4))
        :wood (.fillRect g (- tip-x 2) (- tip-y 5) 6 9)
        (.drawArc g (- tip-x 10) (- tip-y 15) 19 28 -62 105)))))

(defn draw! [^Graphics2D g queue players camera ox oy now]
  (when (seq queue)
    (let [^Graphics2D g (.create g)
          {:keys [particles damage-numbers]} @theme/settings]
      (try
        (.clipRect g ox oy (:width camera) (:height camera))
        (.setFont g (Font. "Dialog" Font/BOLD 14))
        (doseq [{:keys [kind cell at until amount actor resource]} queue
                :let [[x y] (point cell camera ox oy)]
                :when (and (< (- ox 72) x (+ ox (:width camera) 72))
                           (< (- oy 72) y (+ oy (:height camera) 72)) (< now until))
                :let [p (/ (double (- now at)) (- until at))
                      color (if (and (#{:harvest :gather} kind) (= resource :stone))
                              (Color. 195 204 217) (hues kind))]]
          (if (#{:swing :gather} kind)
            (do (weapon! g players actor x y
                         (if (= kind :gather) (/ (mod (- now at) 420) 420.0) p)
                         resource camera ox oy)
                (when (and particles (= kind :gather))
                  (particles! g color x y (/ (mod (- now at) 420) 420.0) 18 4)))
            (do
              (when particles
                (particles! g color x y p 49 (case kind (:debris :death) 17 (:construction :respawn) 14 8)))
              (when (and (< p 0.26) (#{:damage :repair :construction :death :respawn} kind))
                (.setColor g (Color. (.getRed color) (.getGreen color) (.getBlue color) 90))
                (.fillOval g (- x 20) (- y 19) 40 20))
              (when (and amount (or damage-numbers (#{:harvest :repair} kind)))
                (label! g (case kind :harvest (str "+" amount (if (= resource :wood) " дерево" " камень"))
                                :repair (str "+" amount " HP") (str "−" amount))
                        x (- y 28 (int (* p 32))) color)))))
        (finally (.dispose g))))))
