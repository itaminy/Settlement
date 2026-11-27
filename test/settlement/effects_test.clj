(ns settlement.effects-test
  (:require [clojure.test :refer [deftest is]]
            [settlement.effects :as effects]))

(def house {:type :building :kind :house :building-id (random-uuid) :hp 200 :max-hp 200})

(deftest server-health-selects-all-damage-thresholds
  (doseq [[hp expected] [[200 :normal] [152 :normal] [150 :light] [102 :light]
                         [100 :heavy] [52 :heavy] [50 :critical] [1 :critical] [0 :destroyed]]]
    (is (= expected (effects/damage-tier (assoc house :hp hp))))))

(deftest confirmed-snapshots-only-emit-transitions-in-the-same-match
  (let [empty-world {:match-id 0 :objects {} :players {1 {:x 2 :y 2 :hp 100}}
                     :bandits {2 {:hp 0 :respawn-at 50}}}
        active (-> empty-world
                   (assoc-in [:objects [4 4]] house)
                   (assoc-in [:bandits 2] {:x 5 :y 5 :hp 100}))
        damaged (-> active (assoc-in [:objects [4 4] :hp] 170)
                    (assoc-in [:players 1] {:x 2 :y 2 :hp 0 :respawn-at 500}))
        repaired (assoc-in damaged [:objects [4 4] :hp] 180)
        destroyed (assoc repaired :objects {})]
    (is (= #{:construction :respawn} (set (map :kind (effects/snapshot-effects empty-world active)))))
    (is (= #{:damage :death} (set (map :kind (effects/snapshot-effects active damaged)))))
    (is (= [10] (mapv :amount (effects/snapshot-effects damaged repaired))))
    (is (= [:debris] (mapv :kind (effects/snapshot-effects repaired destroyed))))
    (is (empty? (effects/snapshot-effects active active)))
    (is (empty? (effects/snapshot-effects active (assoc empty-world :match-id 1))))
    (is (empty? (effects/snapshot-effects nil active)))))

(deftest rejections-never-display-success-and-expired-events-are-bounded
  (doseq [command [:gather-start :gather-finish :attack-bandit :attack-building :build]]
    (is (empty? (effects/confirmed-effects 1 command {:status :error :position [2 3] :completed true}))))
  (is (= :harvest (:kind (first (effects/confirmed-effects 1 :gather-finish
                                   {:status :ok :completed true :position [2 3] :resource :wood :amount 1})))))
  (let [event {:kind :debris :cell [2 3]}
        queue (effects/enqueue [] 100 (repeat (* 3 effects/limit) event))]
    (is (= effects/limit (count queue)))
    (is (empty? (effects/active queue 1100)))))
