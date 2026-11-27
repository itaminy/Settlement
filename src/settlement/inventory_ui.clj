(ns settlement.inventory-ui
  (:require [settlement.hotbar :as hotbar]
            [settlement.inventory :as inventory]
            [settlement.rendering :as rendering]
            [settlement.ui-theme :as theme])
  (:import [javax.swing JPanel JLabel ImageIcon]
           [java.awt BorderLayout GridLayout Dimension]))

(defn affordable? [items price]
  (every? (fn [[item amount]] (>= (get items item 0) amount)) price))

(defn update-panel! [ui contents]
  (let [items (:items contents)]
    (doseq [[item label] (:counts ui)]
      (.setText label (str "Количество: " (get items item 0))))
    (doseq [[tool button] (:craft-buttons ui)
            :let [cost (inventory/recipes tool)
                  available? (affordable? items cost)]]
      (.setEnabled button (and available? @(:enabled? ui)))
      (.setText button (format "%s ×1  ·  Дерево %d/%d  ·  Камень %d/%d"
                              (inventory/item-names tool) (get items :wood 0) (:wood cost)
                              (get items :stone 0) (:stone cost))))
    (reset! (:contents ui) contents)
    (.setText (:free-label ui) (str "Свободно слотов: " (inventory/free-slots contents)
                                  " / " inventory/capacity))))

(defn enable! [ui enabled?]
  (reset! (:enabled? ui) enabled?)
  (update-panel! ui @(:contents ui)))

(defn create-panel
  ([contents] (create-panel contents :blue))
  ([contents team]
   (let [panel (doto (JPanel. (BorderLayout. 12 10))
                 (.setBackground theme/wood) (.setBorder (theme/frame-border 15)))
         slots (doto (JPanel. (GridLayout. 3 4 5 5)) (.setOpaque false))
         portrait (doto (JPanel. (BorderLayout. 0 5)) (.setBackground theme/inset)
                    (.setBorder (theme/frame-border 8)) (.setPreferredSize (Dimension. 117 140)))
         center (doto (JPanel. (BorderLayout. 9 0)) (.setOpaque false))
         free-label (theme/label "" 12 theme/muted)
         counts (atom {})
         footer (doto (JPanel. (GridLayout. 0 1 0 4)) (.setOpaque false))
         message (theme/label " " 12)
         buttons (into {} (for [tool [:axe :pickaxe]] [tool (theme/button "")]))]
     (.add panel (theme/label "Инвентарь" 24 theme/gold) BorderLayout/NORTH)
     (.add portrait (theme/label "Персонаж" 13 theme/gold) BorderLayout/NORTH)
     (when-let [image (rendering/character-sprite team :down 0)]
       (.add portrait (doto (theme/label "" 13)
                        (.setIcon (ImageIcon. (rendering/scale-image image 100 100)))) BorderLayout/CENTER))
     (doseq [index (range inventory/capacity)]
       (let [item (get inventory/item-order index)
             cell (doto (JPanel. (BorderLayout.)) (.setBackground theme/inset)
                    (.setBorder (theme/frame-border 2)) (.setPreferredSize (Dimension. 108 62)))
             name-label (theme/label (get inventory/item-names item "—") 11)
             count-label (theme/label "" 10 theme/muted)]
         (.add cell name-label BorderLayout/NORTH)
         (when-let [image (hotbar/item-image item)]
           (.add cell (doto (theme/label "" 11) (.setIcon (ImageIcon. (rendering/scale-image image 31 31))))
                 BorderLayout/CENTER))
         (.add cell count-label BorderLayout/SOUTH)
         (.add slots cell)
         (when item (swap! counts assoc item count-label))))
     (.add center portrait BorderLayout/WEST)
     (.add center slots BorderLayout/CENTER)
     (.add panel center BorderLayout/CENTER)
     (.add footer free-label)
     (.add footer (theme/label "Крафт" 13 theme/gold))
     (doseq [tool [:axe :pickaxe]] (.add footer (buttons tool)))
     (.add footer message)
     (.add panel footer BorderLayout/SOUTH)
     (let [ui {:panel panel :slots slots :counts @counts :free-label free-label :craft-buttons buttons
               :message message :contents (atom contents) :enabled? (atom true)}]
       (update-panel! ui contents)
       ui))))
