(ns settlement.text-protocol-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [settlement.config :as config]
            [settlement.text-protocol :refer [write-reply]]))

(def ^:private id #uuid "01234567-89ab-cdef-0123-456789abcdef")
(def ^:private attempt "11111111-2222-3333-4444-555555555555")
(def ^:private building
  {:type :building :kind :fence :building-id id :team :red :owner-id 2 :hp 40 :max-hp 50})

(deftest action-replies-test
  (let [progress {:operation-id (str id) :attempt-id attempt :duration-ms 3000 :elapsed-ms 250}
        progress-fields (str " operation_id=" id " attempt_id=" attempt " duration_ms=3000 elapsed_ms=250")]
    (doseq [[command result expected]
            [[{:type :move :player-id 1} {:world {:players {1 {:x 12 :y 8}}}} "move x=12 y=8"]
             [{:type :inventory} {:inventory {:items {:wood 3 :stone 2 :axe 1}}}
              "inventory wood=3 stone=2 axe=1 pickaxe=0"]
             [{:type :gather-start} progress (str "gather-start" progress-fields)]
             [{:type :gather-heartbeat} progress (str "gather-heartbeat" progress-fields)]
             [{:type :gather-finish :operation-id (str id)}
              {:position [12 8] :resource :wood :amount 1}
              (str "gather-finish operation_id=" id " resource=wood amount=1 x=12 y=8")]
             [{:type :gather-cancel :attempt-id attempt} {:cancelled true}
              (str "gather-cancel operation_id=none attempt_id=" attempt)]
             [{:type :gather-cancel :operation-id (str id)} {:cancelled true}
              (str "gather-cancel operation_id=" id " attempt_id=none")]
             [{:type :build} {:position [12 8] :building building}
              (str "build building=building(12,8," id ",fence,red,2,40,50)")]
             [{:type :attack-building :attack-seq 17} {:anchor [12 8] :building-id id :hp 40 :destroyed? false}
              (str "attack-building attack_seq=17 x=12 y=8 building_id=" id " hp=40 destroyed=false")]
             [{:type :attack-building :attack-seq 18} {:anchor [12 8] :building-id id :hp 0 :destroyed? true}
              (str "attack-building attack_seq=18 x=12 y=8 building_id=" id " hp=0 destroyed=true")]
             [{:type :repair-building :repair-seq 4} {:anchor [12 8] :building-id id :hp 50 :restored-hp 10}
              (str "repair-building repair_seq=4 x=12 y=8 building_id=" id " hp=50 restored_hp=10")]]]
      (is (= (str "reply v=1 ok " expected) (write-reply command (assoc result :status :ok)))))
    (doseq [[result expected]
            [[{:code :missing-argument :message "internal"} "missing-argument"]
             [{:code :attack-cooldown} "attack-cooldown"]
             [{:code :stale-attack :last-attack-seq 17} "stale-attack last_attack_seq=17"]
             [{:code :repair-seq-conflict :last-repair-seq 4} "repair-seq-conflict last_repair_seq=4"]
             [(assoc progress :code :too-early) (str "too-early" progress-fields)]
             [{:message "internal"} "command-failed"]]]
      (is (= (str "reply v=1 error code=" expected) (write-reply nil (assoc result :status :error)))))))

(deftest world-replies-test
  (let [command {:type :state :player-id 1}
        prefix (str "reply v=1 ok world width=" config/world-width " height=" config/world-height " self=1")]
    (is (= (str prefix " players=[] objects=[] total_wood=0 total_stone=0 blue=0 red=0 phase=active winner=none"
                " inventory_wood=0 inventory_stone=0 inventory_axe=0 inventory_pickaxe=0 attack_seq=0 repair_seq=0")
           (write-reply command {:status :ok :world {} :building-sequences {:attack-seq 0 :repair-seq 0}})))
    (is (= (str prefix " players=[player(1,0,1,blue,\"Анна \\\"Я\\\"\\\\\\u000a\\u000d\\u0009\\u2028\"),"
                "player(2,2,0,red,\"Борис\")]"
                " objects=[obstacle(0,3,house),resource(4,5,tree),resource(6,8,stone),"
                "building(12,8," id ",fence,red,2,40,50)]"
                " total_wood=5 total_stone=7 blue=0 red=100 phase=finished winner=red"
                " inventory_wood=3 inventory_stone=2 inventory_axe=1 inventory_pickaxe=0 attack_seq=17 repair_seq=4")
           (write-reply command
                        {:status :ok :token "secret" :credentials {"private" "hash"}
                         :world {:players {2 {:x 2 :y 0 :team :red :name "Борис" :token "secret"
                                              :inventory {:items {:wood 999}}}
                                           1 {:x 0 :y 1 :team :blue :name "Анна \"Я\"\\\n\r\t\u2028"
                                              :inventory {:items {:wood 3 :stone 2 :axe 1}}}}
                                 :objects {[12 8] building [4 5] :tree [6 8] :stone [0 3] :house}
                                 :resources {:wood 5 :stone 7}
                                 :match {:scores {:blue 0 :red 100} :phase :finished :winner :red}}
                         :building-sequences {:attack-seq 17 :repair-seq 4}})))))

(deftest text-snapshot-only-includes-own-health
  (let [snapshot {:status :ok
                  :world {:players {1 {:x 0 :y 1 :team :blue :name "Blue" :hp 20}
                                    2 {:x 2 :y 0 :team :red :name "Red" :hp 0 :respawn-at 10}}}
                  :building-sequences {:attack-seq 0 :repair-seq 0}}
        blue (write-reply {:type :state :player-id 1} snapshot)
        red (write-reply {:type :state :player-id 2} snapshot)]
    (is (str/includes? blue "objects=[downed(2),health(1,20)]"))
    (is (str/includes? red "objects=[downed(2),health(2,0)]"))))
