(ns montecarlo.action)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Action
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ActionP
  (call [this] [this bet-amt])
  (fold [this])
  (raise [this r] [this prev-bet-amt r]))

(defn is-fold?
  [action]
  (neg? action))

(defn is-call?
  [action]
  (= action 0))

(defn is-raise?
  [action]
  (pos? action))

(defn action->raise
  [r]
  r)
