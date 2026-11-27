(ns settlement.visual-v5)

(def enabled? (not= "4.1" (System/getProperty "settlement.world.version")))
(def props
  {:dune-field 6 :dry-channel 5 :scrub-cluster 6 :relict-well 4 :ruin-court 5
   :abandoned-camp 5 :saguaro 6 :rooted-snags 5 :drowned-log 5 :marsh-bank 6
   :forest-floor 6 :talus-fan 6})
(def ground-props #{:dune-field :dry-channel :forest-floor :talus-fan})
(def ridges #{:massif :escarpment :snow-ridge :sand-ridge})
(def terrain [:dune :shale :bog :litter :lagoon :depth :wash])
(defn path [kind variant]
  (str "images/visual_v5/" (name kind) "_" variant ".png"))
