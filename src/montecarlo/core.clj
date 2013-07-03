(ns montecarlo.core
  (require [clojure.core.async :as async
            :refer [<! >! <!! >!! timeout chan alt! alts!! go]]))

(defrecord Card
    [suit rank])

(defrecord Bet
    [bet players])

(defrecord Pot
    [pot players])

(defrecord Player
    [id stack])

(defrecord Board
    [deck
     community-cards
     bets    ;; list of Bet
     pots    ;; list of Pot
     remaining-players ;; players who have not yet played
     players ;; players still in hand
     dealer  ;; Player that is dealer
     blinds])

(def COMPLETE-DECK
  (for [suit [:hearts :diamonds :spades :clubs]
        rank (range 2 15)]
    (Card. suit rank)))

(defn init-board
  [players dealer blinds]
  (let [deck COMPLETE-DECK
        community-cards (list)
        folded (set)
        pots (list)
        bets []
        remaining-players players]
    (Board. deck community-cards bets pots
            remaining-players players dealer blinds)))


(defn is-fold?
  [action]
  (neg? action))

(defn is-call?
  [action]
  (= action 0))

(defn is-raise?
  [action]
  (pos? action))

(defn fold
  [player board]
  (let [remaining-players (remove #{player} (:remaining-players board))]
    (assoc board :remaining-players remaining-players)))

(defn action->raise
  [r]
  r)

(defn split-bet
  [bet x player]
  [(Bet. x (conj (:players bet) player))
   (update-in bet [:bet] #(- % x))])

(defn call-bet
  [bets player]
  (update-in bets [0 :players] #(conj % player)))

(defn update-bets
  [bets new-bet]
  (let [[player] (get new-bet :players)
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
        (concat ret [(Bet. bet #{player})])))))

(defn raise
  [board player r]
  (let [bet (+ r (get-in board [:bet]))]))

(defn call
  [board player]
  (raise board player 0))

(comment
  (defn play-stage
    [board stage-end stage-transition action-ch broadcast-ch]
    (loop [board board
           remaining-cycle (cycle (:remaining-players board))]
      (let [player (first remaining-cycle)]
        (if (stage-end board)
          (stage-transition board)
          (go
           (let [action (<!! action-ch)]
             (cond
              (is-fold? action) (recur (fold board)
                                       (rest remaining-cycle))
              (is-call? action) (recur (call board player)
                                       (rest remaining-cycle))
              (is-raise? action (recur (raise board (action->raise action))
                                       (rest remaining-cycle)))
              :else (throw "Action is not fold nor call nor raise!"))))))))

  (defn preflop-board->flop-board
    "First player and current player reset"
    [board]
    (let [])))