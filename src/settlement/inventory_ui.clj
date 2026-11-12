(ns settlement.inventory-ui
  (:require [settlement.inventory :as inventory])
  (:import [javax.swing JPanel JLabel SwingConstants]
           [javax.swing.border LineBorder EmptyBorder]
           [java.awt Color Font BorderLayout GridLayout Dimension]))

(defn update-panel! [ui contents]
  (doseq [[item label] (:counts ui)]
    (.setText label (str "Количество: " (get-in contents [:items item] 0))))
  (.setText (:free-label ui) (str "Свободно слотов: " (inventory/free-slots contents)
                                " / " inventory/capacity)))

(defn create-panel [contents]
  (let [panel (JPanel. (BorderLayout. 0 12))
        slots (JPanel. (GridLayout. 3 4 8 8))
        title (JLabel. "ИНВЕНТАРЬ — I: закрыть" SwingConstants/CENTER)
        free-label (JLabel. "" SwingConstants/CENTER)
        counts (atom {})]
    (.setBackground panel (Color. 45 59 39))
    (.setBorder panel (EmptyBorder. 16 16 16 16))
    (.setOpaque slots false)
    (doseq [index (range inventory/capacity)]
      (let [item (get inventory/item-order index)
            cell (JPanel. (BorderLayout. 4 6))
            name-label (JLabel. (get inventory/item-names item "Свободный слот") SwingConstants/CENTER)
            count-label (JLabel. (if item "" "—") SwingConstants/CENTER)]
        (.setPreferredSize cell (Dimension. 128 88))
        (.setBackground cell (Color. 78 86 52))
        (.setBorder cell (LineBorder. (Color. 151 126 74) 3))
        (doseq [label [name-label count-label]]
          (.setForeground label (Color. 245 236 201))
          (.setFont label (Font. "Monospaced" Font/BOLD 12)))
        (.add cell name-label BorderLayout/CENTER)
        (.add cell count-label BorderLayout/SOUTH)
        (.add slots cell)
        (when item (swap! counts assoc item count-label))))
    (doseq [label [title free-label]]
      (.setForeground label (Color. 245 236 201))
      (.setFont label (Font. "Monospaced" Font/BOLD 14)))
    (.add panel title BorderLayout/NORTH)
    (.add panel slots BorderLayout/CENTER)
    (.add panel free-label BorderLayout/SOUTH)
    (let [ui {:panel panel :slots slots :counts @counts :free-label free-label}]
      (update-panel! ui contents)
      ui)))
