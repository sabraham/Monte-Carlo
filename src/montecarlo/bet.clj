(ns montecarlo.bet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Bet
    [bet players original-players n]) ;; player-ids

(defn merge-bets
  [bets]
  (loop [input bets
         output (list)]
    (if-let [bet-i (first input)]
      (let [bet-o (first output)]
        (if (and (= (:players bet-i) (:players bet-o))
                 (= (:original-players bet-i) (:original-players bet-o)))
          (recur (rest input)
                 (conj (rest output)
                       (->Bet (+ (:bet bet-i) (:bet bet-o))
                              (:players bet-i)
                              (:original-players bet-i)
                              (:n bet-i))))
          (recur (rest input)
                 (conj output
                       bet-i))))
      (reverse output))))

(defn split-bet
  [bet x player]
  [(->Bet x
          (conj (:players bet) player)
          (conj (:original-players bet) player)
          (inc (:n bet)))
   (update-in bet [:bet] #(- % x))])

(defn call-bet
  [bets player]
  (let [bet (first bets)]
    (conj (rest bets) (assoc bet
                        :players (conj (:players bet) player)
                        :original-players (conj (:original-players bet) player)
                        :n (inc (:n bet))))))

(defn update-bets
  [bets new-bet]
  (let [player (first (get new-bet :players)) ;; new bet only has one player
        bet (get new-bet :bet)]
    (loop [bets bets
           bet bet
           ret []]
      (if-let [standing-bet (first bets)]
        (cond
         (< bet (:bet standing-bet)) (concat ret (split-bet standing-bet bet player))
         (= bet (:bet standing-bet)) (concat ret (call-bet bets player))
         (> bet (:bet standing-bet)) (recur (rest bets)
                                            (- bet (:bet standing-bet))
                                            (conj ret (first (call-bet bets player)))))
        (concat ret [(->Bet bet #{player} #{player} 1)])))))
