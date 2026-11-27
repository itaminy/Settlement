(ns settlement.config)

(def world-width 100)
(def world-height 100)
(def world-v7? (contains? #{nil "7.0"} (System/getProperty "settlement.world.version")))
(def world-seed
  (if-let [value (System/getenv "SETTLEMENT_WORLD_SEED")]
    (or (parse-long value)
        (throw (ex-info "SETTLEMENT_WORLD_SEED must be an integer" {:value value})))
    592321))
(def tile-size 64)
(def viewport-width 896)
(def viewport-height 768)

(def move-animation-ms
  (if-let [value (System/getenv "SETTLEMENT_MOVE_ANIMATION_MS")]
    (let [milliseconds (parse-long value)]
      (when-not (and milliseconds (<= 1 milliseconds 1000))
        (throw (ex-info "Invalid movement animation duration" {:value value})))
      milliseconds)
    220))

(def snapshot-interval-ms 125)

(def bandits {:count 4 :hp 100 :damage 10 :attack-ms 2000 :move-ms 450
              :detect-radius 12 :spawn-delay-ms 60000 :respawn-ms 60000
              :safe-distance 5 :reward {:wood 5 :stone 3}})

(def player-combat {:hp 100 :respawn-ms 15000})

(def bot-timing-ms
  (into {} (for [[kind variable fallback minimum] [[:move "SETTLEMENT_BOT_MOVE_MS" 350 1]
                                                  [:action "SETTLEMENT_BOT_ACTION_MS" 700 700]
                                                  [:after-build "SETTLEMENT_BOT_BUILD_MS" 2000 1]]]
             [kind (if-let [value (System/getenv variable)]
                     (let [milliseconds (parse-long value)]
                       (when-not (and milliseconds (<= minimum milliseconds 60000))
                         (throw (ex-info "Invalid bot timing" {:variable variable :minimum minimum})))
                       milliseconds)
                     fallback)])))

(defn in-bounds? [[x y]]
  (and (<= 0 x) (< x world-width) (<= 0 y) (< y world-height)))
