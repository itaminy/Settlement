(ns settlement.visual-v6)

(def enabled? (not (#{"4.1" "5"} (System/getProperty "settlement.world.version"))))

(def ridges
  {[5 61] [:massif 14 2 "west-cordillera"]
   [7 67] [:escarpment 10 2 "gorge-north"]
   [12 74] [:massif 11 2 "gorge-south"]
   [28 65] [:escarpment 8 2 "east-rampart"]
   [8 83] [:snow-ridge 17 2 "glacier-crown"]
   [12 91] [:snow-ridge 12 2 "glacier-front"]
   [80 73] [:sand-ridge 13 2 "canyon-west"]
   [84 85] [:sand-ridge 12 2 "canyon-east"]
   [87 94] [:sand-ridge 9 2 "canyon-south"]})

(defn path [name] (str "images/visual_v6/" name ".png"))
