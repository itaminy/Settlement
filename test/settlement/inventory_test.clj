(ns settlement.inventory-test
  (:require [clojure.test :refer :all]
            [settlement.layout :as layout]
            [settlement.test-support :as support]
            [settlement.inventory :as inventory]
            [settlement.inventory-ui :as inventory-ui]
            [settlement.hotbar :as hotbar]
            [settlement.world :as world]
            [settlement.server :as server]
            [settlement.auth :as auth]
            [settlement.client :as client]
            [settlement.gui :as gui])
  (:import [javax.swing JLabel JPanel JProgressBar KeyStroke SwingUtilities]
           [java.awt.event KeyEvent MouseAdapter MouseEvent MouseWheelEvent]
           [java.awt.image BufferedImage]))

(use-fixtures :each
  (fn [f]
    (with-redefs [layout/spawn-cells support/row-major-spawn
                  world/gathering (ref {})
                  world/now-ns #(deref support/time-ns)
                  world/world (ref {:players {} :resources {:wood 0 :stone 0}
                                   :objects {[1 0] :tree [3 0] :stone}})
                  auth/state (ref (auth/new-state))
                  world/schedule-resource-restore (fn [_ _] nil)]
      (f))))

(defn- register! [connection id]
  (support/process-message connection {:type :register :player-id id :name (str id)}))

(deftest hotbar-keeps-square-slots-and-input-separate-from-the-map-test
  (SwingUtilities/invokeAndWait
   (bound-fn []
     (let [map-panel (gui/create-map-panel 0 0 7)
           ui (gui/create-resource-panel {})
           {:keys [panel selected]} (:hotbar ui)
           progress (JProgressBar.)
           status (JLabel. "Status")
           {:keys [footer] content :panel} (gui/create-game-content map-panel ui progress status)
           map-clicks (atom 0)]
       (.addMouseListener map-panel (proxy [MouseAdapter] []
                                     (mousePressed [_] (swap! map-clicks inc))
                                     (mouseClicked [_] (swap! map-clicks inc))))
       (doseq [width [400 800 1920]]
         (.setSize content width 600)
         (.doLayout content)
         (.doLayout footer)
         (let [slots (hotbar/slot-bounds (.getWidth panel) (.getHeight panel))
               [left _ size] (first slots) [right _] (last slots)]
           (is (= 9 (count slots)))
           (is (every? (fn [[_ _ w h]] (= size w h)) slots))
           (is (<= (abs (- (+ left right size) width)) 1))
           (is (= (if (= width 400) 39 56) size))
           (is (<= (+ (.getY map-panel) (.getHeight map-panel)) (.getY footer)))
           (is (<= (+ (.getY progress) (.getHeight progress)) (.getY panel)))
           (is (<= (+ (.getY panel) (.getHeight panel)) (.getY status)))))
       (doseq [[key expected] [[KeyEvent/VK_9 8] [KeyEvent/VK_NUMPAD2 1]]]
         (let [action (.get (.getInputMap panel JPanel/WHEN_IN_FOCUSED_WINDOW)
                            (KeyStroke/getKeyStroke key 0))]
           (.actionPerformed (.get (.getActionMap panel) action) nil)
           (is (= expected @selected))))
       (let [[left top] (nth (hotbar/slot-bounds (.getWidth panel) (.getHeight panel)) 3)
             press (MouseEvent. panel MouseEvent/MOUSE_PRESSED 0 0 (+ left 4) (+ top 4)
                                1 false MouseEvent/BUTTON1)]
         (is (= "Кирка" (.getToolTipText panel press)))
         (.dispatchEvent panel press)
         (is (= 3 @selected))
         (is (.isConsumed press)))
       (doseq [[target rotation expected] [[panel -4 8] [map-panel 1 0]]]
         (.dispatchEvent target (MouseWheelEvent. target MouseEvent/MOUSE_WHEEL 0 0 10 10
                                                  0 false MouseWheelEvent/WHEEL_UNIT_SCROLL 1 rotation))
         (is (= expected @selected)))
       (is (zero? @map-clicks))
       (is (= 5 (.getComponentCount (:panel ui))))))))

(deftest hotbar-follows-gather-craft-build-and-repair-test
  (SwingUtilities/invokeAndWait
   (bound-fn []
     (register! :owner 1)
     (let [map-panel (gui/create-map-panel 0 0 1)
           ui (gui/create-resource-panel {})
           contents (get-in ui [:hotbar :items])
           request (fn [command] (support/process-message :owner (assoc command :player-id 1)))
           stock! (fn [expected]
                    (gui/refresh! map-panel {:writer :owner} ui 1)
                    (is (= expected (mapv @contents inventory/item-order))))]
       (with-redefs [client/async! support/immediate-async]
         (is (= :ok (:status (request {:type :gather}))))
         (stock! [1 0 0 0])
         (world/change-player-items! 1 {:wood 5 :stone 2})
         (is (= :ok (:status (request {:type :craft :tool :axe}))))
         (stock! [3 0 1 0])
         (let [position [1 0]
               built (request {:type :build :building-type :fence :position position})
               id (get-in built [:building :building-id])]
           (is (= :ok (:status built)))
           (stock! [1 0 1 0])
           (dosync (alter world/world assoc-in [:objects position :hp] 40))
           (is (= :ok (:status (request {:type :repair-building :position position
                                        :building-id id :repair-seq 1}))))
           (stock! [0 0 1 0])))))))

(deftest hotbar-renders-and-refreshes-only-the-local-snapshot-test
  (SwingUtilities/invokeAndWait
   (bound-fn []
     (let [map-panel (gui/create-map-panel 0 0 1)
           ui (gui/create-resource-panel {})
           {:keys [panel items selected]} (:hotbar ui)
           local-stock {:wood 12 :stone 7 :axe 1 :pickaxe 1}
           snapshot (atom {:players {1 {:x 0 :y 0 :inventory {:items local-stock}}
                                     2 {:x 1 :y 0 :inventory {:items {:wood 999 :stone 999}}}}
                           :objects {} :resources {:wood 9999 :stone 9999}})
           paint (fn []
                   (let [target (BufferedImage. 552 72 BufferedImage/TYPE_INT_ARGB)
                         graphics (.createGraphics target)]
                     (try (.paint panel graphics) (finally (.dispose graphics)))
                     target))
           pixels (fn [^BufferedImage image [x y w h]] (vec (.getRGB image x y w h nil 0 w)))]
       (.setSize panel 552 72)
       (let [empty (paint)]
         (with-redefs [client/async! (fn [_ _ _ callback] (callback {:status :ok :world @snapshot}))]
           (gui/refresh! map-panel {} ui 1)
           (is (= local-stock @items))
           (let [full (paint)]
             (doseq [[index bounds] (map-indexed vector (hotbar/slot-bounds 552 72))]
               (is (= (< index 4) (not= (pixels empty bounds) (pixels full bounds))))))
           (swap! snapshot assoc-in [:players 1 :inventory :items] {:wood 0 :stone 6 :axe 1 :pickaxe 0})
           (gui/refresh! map-panel {} ui 1)
           (is (= {:wood 0 :stone 6 :axe 1 :pickaxe 0} @items))
           (is (= 0 @selected))))))))

(deftest empty-inventory-test
  (register! :a 1)
  (is (= inventory/empty-inventory (get-in @world/world [:players 1 :inventory])))
  (is (= 12 (inventory/free-slots (get-in @world/world [:players 1 :inventory]))))
  (is (not (contains? (get-in @world/world [:players 1]) :collected)))
  (is (= {:wood 0 :stone 0} (get-in (world/get-world) [:players 1 :collected]))))

(deftest migration-preserves-existing-resources-test
  (dosync (alter world/world assoc :players
                 {1 {:name "Existing" :x 7 :y 8 :collected {:wood 23 :stone 9}}}))
  (world/migrate-inventories!)
  (let [player (get-in @world/world [:players 1])]
    (is (= {:items {:wood 23 :stone 9 :axe 0 :pickaxe 0}} (:inventory player)))
    (is (= ["Existing" 7 8] ((juxt :name :x :y) player)))
    (is (not (contains? player :collected)))
    (world/migrate-inventories!)
    (is (= player (get-in @world/world [:players 1]))))
  (is (true? (world/collect-resource 1 :wood 2)))
  (is (= {:wood 25 :stone 9} (get-in (world/get-world) [:players 1 :collected]))))

(deftest inventory-is-authoritative-test
  (register! :a 1)
  (dosync (alter world/world update-in [:players 1]
                 assoc :collected {:wood 999 :stone 999}))
  (is (= {:wood 0 :stone 0} (get-in (world/get-world) [:players 1 :collected])))
  (world/migrate-inventories!)
  (world/change-player-items! 1 {:wood 5 :stone 3})
  (world/change-player-items! 1 {:wood -2 :stone -1})
  (is (= {:wood 3 :stone 2} (get-in (world/get-world) [:players 1 :collected])))
  (is (= {:items {:wood 3 :stone 2 :axe 0 :pickaxe 0}}
         (get-in @world/world [:players 1 :inventory]))))

(deftest independent-gathered-inventories-test
  (register! :a 1)
  (register! :b 2)
  (is (= :ok (:status (support/process-message :a {:type :gather :player-id 1}))))
  (is (= :ok (:status (support/process-message :b {:type :gather :player-id 2}))))
  (let [state (world/get-world)]
    (is (= {:wood 1 :stone 0 :axe 0 :pickaxe 0} (get-in state [:players 1 :inventory :items])))
    (is (= {:wood 0 :stone 1 :axe 0 :pickaxe 0} (get-in state [:players 2 :inventory :items])))
    (is (= {:wood 1 :stone 1} (:resources state)))
    (doseq [id [1 2]]
      (is (= (get-in state [:players id :collected])
             (inventory/resources (get-in state [:players id :inventory])))))))

(deftest reconnect-keeps-inventory-test
  (let [registration (register! :a 1)]
    (support/process-message :a {:type :gather :player-id 1})
    (let [before (world/get-world)]
      (auth/disconnect! :a)
      (is (= :ok (:status (support/process-message :new
                           (merge (select-keys registration [:token :server-instance])
                                  {:type :login :player-id 1})))))
      (is (= before (world/get-world)))
      (is (= 1 (get-in (world/get-world) [:players 1 :inventory :items :wood]))))))

(deftest forged-inventory-commands-rejected-test
  (register! :a 1)
  (register! :b 2)
  (doseq [message [{:type :gather :player-id 1}
                   {:type :collect :player-id 1 :resource :wood :amount 500}
                   {:type :set-inventory :player-id 1 :inventory {:items {:wood 500}}}
                   {:type :change-items :player-id 1 :changes {:axe 1}}]]
    (let [before (world/get-world)]
      (is (= :error (:status (support/process-message :b message))))
      (is (= before (world/get-world)))))
  (support/process-message :b {:type :gather :player-id 2 :inventory {:items {:axe 99}}})
  (is (= 0 (get-in (world/get-world) [:players 2 :inventory :items :axe]))))

(deftest future-cost-and-reward-validation-test
  (register! :a 1)
  (world/change-player-items! 1 {:wood 5 :stone 2})
  (is (= :ok (:status (world/change-player-items! 1 {:wood -3 :stone -1 :axe 1 :pickaxe 1}))))
  (let [before (world/get-world)]
    (is (= :insufficient-items (:code (world/change-player-items! 1 {:wood -99 :axe 1}))))
    (is (= :invalid-items (:code (world/change-player-items! 1 {:gold 10}))))
    (is (= :invalid-items (:code (world/change-player-items! 1 {:wood 1.5}))))
    (is (= :player-not-found (:code (world/change-player-items! 99 {:wood 1}))))
    (is (= before (world/get-world))))
  (is (= {:wood 2 :stone 1 :axe 1 :pickaxe 1}
         (get-in (world/get-world) [:players 1 :inventory :items]))))

(deftest inventory-panel-updates-after-gather-test
  (register! :a 1)
  (javax.swing.SwingUtilities/invokeAndWait
   (fn []
     (let [contents inventory/empty-inventory
           ui (inventory-ui/create-panel contents)
           resource-ui (assoc (gui/create-resource-panel {:wood 0 :stone 0}) :inventory-ui ui)
           panel (gui/create-map-panel 0 0 1)]
       (is (= 12 (count (.getComponents (:slots ui)))))
       (is (= "Свободно слотов: 12 / 12" (.getText (:free-label ui))))
       (with-redefs [client/async! support/immediate-async]
         (support/process-message :a {:type :gather :player-id 1})
         (gui/update-player-panels! resource-ui (world/get-world) 1))
       (is (= "Количество: 1" (.getText (get-in ui [:counts :wood]))))
       (is (= "Количество: 0" (.getText (get-in ui [:counts :axe]))))
       (is (= "Свободно слотов: 11 / 12" (.getText (:free-label ui))))
       (is (= 1 (:wood @(get-in resource-ui [:hotbar :items]))))))))

(deftest inventory-key-toggle-test
  (let [panel (JPanel.) visible (atom false)]
    (gui/bind-inventory-key panel #(swap! visible not))
    (is (= "inventory" (.get (.getInputMap panel JPanel/WHEN_IN_FOCUSED_WINDOW)
                             (KeyStroke/getKeyStroke KeyEvent/VK_I 0))))
    (.actionPerformed (.get (.getActionMap panel) "inventory") nil)
    (is (true? @visible))
    (.actionPerformed (.get (.getActionMap panel) "inventory") nil)
    (is (false? @visible))))
