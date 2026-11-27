(ns settlement.ui-theme-test
  (:require [clojure.test :refer [deftest is]]
            [settlement.gui :as gui]
            [settlement.ui-theme :as theme]
            [settlement.world :as world])
  (:import [java.io File]))

(deftest display-settings-round-trip-including-disabled-switches
  (let [file (doto (File/createTempFile "settlement-ui-" ".edn") (.deleteOnExit))
        original @theme/settings
        chosen {:fullscreen true :resolution "1920×1080" :scale 115 :fps 120
                :animations false :particles false :damage-numbers false
                :names false :minimap :large}]
    (try
      (binding [theme/*settings-file* (.getPath file)]
        (theme/save! chosen)
        (reset! theme/settings theme/defaults)
        (is (= chosen (theme/load!)))
        (is (= 60 (:fps (theme/normalize {:fps 1}))))
        (is (= :normal (:minimap (theme/normalize {:minimap :unknown}))))
        (is (= "02:05" (gui/format-duration 125900))))
      (finally (reset! theme/settings original)))))

(deftest server-duration-stops-at-victory
  (let [clock (atom 0)
        world-state (ref {:match world/initial-match :players {} :objects {}})]
    (with-redefs [world/world world-state world/combat-time #(deref clock)]
      (world/initialize-bandits!)
      (reset! clock 14000)
      (is (= 14000 (:match-duration-ms (world/get-world))))
      (dosync
        (alter world/world (fn [state]
                             (#'world/recalculate-match
                               (assoc state :objects
                                      (zipmap (range 5) (repeat {:type :building :team :blue :kind :mine})))))))
      (reset! clock 21000)
      (is (= :finished (get-in (world/get-world) [:match :phase])))
      (is (= 14000 (:match-duration-ms (world/get-world)))))))
