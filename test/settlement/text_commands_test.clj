(ns settlement.text-commands-test
  (:require [clojure.test :refer [deftest is testing]]
            [settlement.auth :as auth]
            [settlement.config :as config]
            [settlement.text-commands :refer [parse-command]]
            [settlement.world :as world])
  (:import [java.util UUID]))

(def ^:private id "01234567-89ab-cdef-0123-456789abcdef")
(def ^:private building-id (UUID/fromString id))

(deftest supported-commands-test
  (doseq [[command expected]
          [["иди вверх" {:type :move :direction :up}]
           ["иди вниз" {:type :move :direction :down}]
           ["иди влево" {:type :move :direction :left}]
           ["иди вправо" {:type :move :direction :right}]
           ["покажи мир" {:type :state}]
           ["покажи инвентарь" {:type :inventory}]
           [(str "собери дерево попытка " id)
            {:type :gather-start :resource-type :tree :attempt-id id}]
           [(str "собери камень попытка " id)
            {:type :gather-start :resource-type :stone :attempt-id id}]
           [(str "собери дерево на 0 1 попытка " id)
            {:type :gather-start :resource-type :tree :position [0 1] :attempt-id id}]
           [(str "собери камень на 12 8 попытка " id)
            {:type :gather-start :resource-type :stone :position [12 8] :attempt-id id}]
           [(str "продолжай добычу " id) {:type :gather-heartbeat :operation-id id}]
           [(str "заверши добычу " id) {:type :gather-finish :operation-id id}]
           [(str "отмени добычу " id) {:type :gather-cancel :operation-id id}]
           [(str "отмени попытку добычи " id) {:type :gather-cancel :attempt-id id}]
           ["построй забор на 0 0" {:type :build :building-type :fence :position [0 0]}]
           ["построй дом на 12 8" {:type :build :building-type :house :position [12 8]}]
           ["построй башню на 12 8" {:type :build :building-type :watchtower :position [12 8]}]
           ["построй сторожевую башню на 12 8" {:type :build :building-type :watchtower :position [12 8]}]
           ["построй лесопилку на 12 8" {:type :build :building-type :sawmill :position [12 8]}]
           ["построй шахту на 12 8" {:type :build :building-type :mine :position [12 8]}]
           [(str "атакуй постройку на 12 8 id " id " номер 17")
            {:type :attack-building :position [12 8] :building-id building-id :attack-seq 17}]
           [(str "почини постройку на 12 8 id " id " номер 4")
            {:type :repair-building :position [12 8] :building-id building-id :repair-seq 4}]]]
    (is (= expected (parse-command command)) command)))

(deftest case-and-horizontal-whitespace-test
  (is (= {:type :build :building-type :house :position [12 8]}
         (parse-command " \tПоСтРоЙ\tДОМ  НА\u00a012   8\t ")))
  (is (= {:type :gather-heartbeat :operation-id id}
         (parse-command "ПРОДОЛЖАЙ ДОБЫЧУ 01234567-89AB-CDEF-0123-456789ABCDEF")))
  (is (= building-id
         (:building-id (parse-command
                        "АТАКУЙ ПОСТРОЙКУ НА 1 2 ID 01234567-89AB-CDEF-0123-456789ABCDEF НОМЕР 1")))))

(deftest numeric-arguments-test
  (let [max-x (dec config/world-width)
        max-y (dec config/world-height)]
    (is (= [max-x max-y]
           (:position (parse-command (str "построй дом на " max-x " " max-y))))))
  (let [invalid {:status :error :code :invalid-argument}]
    (doseq [position ["-1 0" "0 -1" "9223372036854775808 0" "0 -9223372036854775809"
                      "1.5 0" "0 1/2" (str config/world-width " 0") (str "0 " config/world-height)]
            command [(str "построй дом на " position)
                     (str "собери дерево на " position " попытка " id)
                     (str "атакуй постройку на " position " id " id " номер 1")
                     (str "почини постройку на " position " id " id " номер 1")]]
      (is (= invalid (parse-command command)) command))
    (doseq [[verb field] [["атакуй" :attack-seq] ["почини" :repair-seq]]]
      (let [prefix (str verb " постройку на 1 2 id " id " номер ")]
        (is (= Long/MAX_VALUE (field (parse-command (str prefix Long/MAX_VALUE)))))
        (doseq [number ["0" "-1" "9223372036854775808" "1.0"]]
          (is (= invalid (parse-command (str prefix number))) (str prefix number)))))))

(deftest invalid-uuid-test
  (doseq [uuid ["1-1-1-1-1" "01234567-89ab-cdef-0123-456789abcdeg" (str id "0")]
          command [(str "собери дерево попытка " uuid)
                   (str "отмени добычу " uuid)
                   (str "почини постройку на 1 2 id " uuid " номер 1")]]
    (is (= {:status :error :code :invalid-argument} (parse-command command)) command)))

(deftest incomplete-commands-test
  (doseq [command ["иди" "покажи" "собери дерево" "собери камень на" "собери камень на 1"
                   "собери дерево на 1 2 попытка" "продолжай добычу" "заверши добычу"
                   "отмени добычу" "отмени попытку добычи" "построй дом" "построй забор на 1"
                   "атакуй постройку на 1 2 id" (str "почини постройку на 1 2 id " id " номер")]]
    (is (= {:status :error :code :missing-argument} (parse-command command)) command)))

(deftest unexpected-input-test
  (testing "Unknown commands"
    (doseq [input ["" " \t " "телепортируйся" "иди наискосок" "покажи погоду" "(+ 1 2)"]]
      (is (= {:status :error :code :unknown-command} (parse-command input)) (pr-str input))))
  (testing "Malformed arguments, trailing input, and impersonation"
    (doseq [input [nil 42 {} "иди вверх 1" "иди вверх player-id 2" "покажи мир :player-id 2"
                   "иди вверх\nиди вниз" "построй дом около 1 2" "построй дом на 1 2 3"
                   (str "собери дерево попытка " id " extra")
                   (str "почини постройку на 1 2 id " id " номер 1 player-id 2")]]
      (is (= {:status :error :code :invalid-argument} (parse-command input)) (pr-str input)))))

(deftest parsing-has-no-side-effects-test
  (let [before [@world/world @world/gathering @auth/state]
        command (str "атакуй постройку на 1 2 id " id " номер 1")
        output (java.io.StringWriter.)]
    (binding [*out* output *err* output]
      (let [parsed (parse-command command)]
        (is (= :attack-building (:type parsed)))
        (is (= parsed (parse-command command)))
        (is (not (contains? parsed :player-id))))
      (is (= {:status :error :code :invalid-argument}
             (parse-command "иди вверх #=(clojure.core/println \"unexpected\")"))))
    (is (= before [@world/world @world/gathering @auth/state]))
    (is (= "" (str output)))))
