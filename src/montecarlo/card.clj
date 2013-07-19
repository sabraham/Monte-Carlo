(ns montecarlo.card)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Card
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Card
    [suit rank])

(def COMPLETE-DECK
  (for [suit [:hearts :diamonds :spades :clubs]
        rank (range 2 15)]
    (->Card suit rank)))
