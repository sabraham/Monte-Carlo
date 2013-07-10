(ns montecarlo.core
  (require [clojure.core.async :as async
            :refer [<! >! <!! >!! timeout chan alt! alts!! go close!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Card
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Card
    [suit rank])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Bet
    [bet players]) ;; player-ids

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Pot
    [pot players]) ;; player-ids

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Player
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Player
    [id stack hand
     action-ch ;; Action :: player -> main -_-
     card-ch ;; Card :: board -> player
     board-ch ;; Board :: board -> player
     quit-ch])

(defprotocol PlayerP
  (run [this])
  (public-player [this]))

(extend-type Player
  PlayerP
  (run [this]
    (go (while true
          (alt! (:card-ch this)  ([card]
                                    (do
                                      (swap! (:hand this) conj card)))
                (:board-ch this) ([board] ;(println (:id this) " received update")
                                    )
                (:quit-ch this)  ([s] (println "i don't know to quit you."))))))
  (public-player [this]
    (select-keys this [:id :stack])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Board
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Board
    [community-cards
     bets    ;; list of Bet
     pots    ;; list of Pot
     remaining-players ;; players who have not yet played
     play-order ;; typically (cycle remaining-players)
     players ;; players still in hand
     blinds ;; blinds
     ])

(defprotocol BoardP
  (read-board [this])
  (update-players [this]))

(extend-type Board
  BoardP
  (read-board [this]
    (let [public-board (select-keys this [:community-cards :bets :pots
                                          :remaining-players :play-order
                                          :blinds])]
      (assoc public-board :players (map public-player (:players this)))))
  (update-players [this]
    (let [new-board (read-board this)]
      (doseq [ch (map :board-ch (:players this))]
        (go (>! ch new-board))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def COMPLETE-DECK
  (for [suit [:hearts :diamonds :spades :clubs]
        rank (range 2 15)]
    (->Card suit rank)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Action
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bets & Board
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn split-bet
  [bet x player]
  [(->Bet x (conj (:players bet) player))
   (update-in bet [:bet] #(- % x))])

(defn call-bet
  [bets player]
  (conj (rest bets) (update-in (first bets) [:players] #(conj % player))))

(defn merge-bets
  [bets]
  (loop [input bets
         output (list)]
    (if-let [bet-i (first input)]
      (let [bet-o (first output)]
        (if (= (:players bet-i) (:players bet-o))
          (recur (rest input)
                 (conj (rest output)
                       (->Bet (+ (:bet bet-i) (:bet bet-o)) (:players bet-i))))
          (recur (rest input)
                 (conj output
                       bet-i))))
      (reverse output))))

(defn update-bets
  [bets new-bet]
  (let [player (first (get new-bet :players))
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
        (concat ret [(->Bet bet #{player})])))))

(defn board->total-bet
  [board]
  (reduce + (map :bet (:bets board))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Board
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fold
  [board]
  (let [player (first (:play-order board))
        remaining-players (remove #{player} (:remaining-players board))
        play-order (filter #(not (= player %)) (:play-order board))
        players (remove #{player} (:players board))]
    (assoc board :remaining-players remaining-players :play-order play-order
           :players players)))

(defn raise
  [board r]
  (let [player (first (:play-order board))
        new-bet (->Bet (+ r (board->total-bet board))
                       #{(:id player)})
        bets (update-bets (:bets board) new-bet)]
    (assoc board
      :bets (merge-bets bets)
      :play-order (rest (:play-order board))
      :remaining-players (remove #{player} (:players board)))))

(defn call
  [board]
  (let [player (first (:play-order board))
        bet-amt (board->total-bet board)]
    (if (pos? bet-amt)
      (let [new-bet (->Bet bet-amt
                           #{(:id player)})
            bets (update-bets (:bets board) new-bet)]
        (assoc board
          :bets (merge-bets bets)
          :play-order (rest (:play-order board))
          :remaining-players (remove #{player} (:remaining-players board))))
      (assoc board
        :bets (merge-bets (:bets board))
        :play-order (rest (:play-order board))
        :remaining-players (remove #{player} (:remaining-players board))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn stage-end?
  [board]
  (empty? (:remaining-players board)))

(defn stage-transition
  [board]
  (assoc board
    :remaining-players (:players board)
    :bets (list)
    :pots (concat (:pots board) (:bets board))
    :play-order (cycle (:players board))))

(defn play-stage
  [board action-ch]
  (loop [board board]
     (let [player (first (:play-order board))]
       (if (stage-end? board)
         (stage-transition board)
         (let [action (<!! action-ch)]
           (cond
            (is-fold? action) (let [res (fold board)]
                                (update-players res)
                                (recur res))
            (is-call? action) (let [res (call board)]
                                (update-players res)
                                (recur res))
            (is-raise? action) (let [res (raise board (action->raise action))]
                                (update-players res)
                                (recur res))
            :else (throw "Action is not fold nor call nor raise!")))))))

(defn init-board
  [players blinds]
  (let [community-cards (list)
        pots (list)
        bets (list)
        remaining-players players
        play-order (cycle remaining-players)]
    (->Board community-cards bets pots
             remaining-players play-order players blinds)))

(defn deal
  [deck players cards-per-player]
  (let [n (count players)]
    (loop [cards-per-player cards-per-player
           deck deck]
      (if (zero? cards-per-player)
        deck
        (do
          (doseq [[card player] (map list deck players)]
            (go (>! (:card-ch player) card)))
          (recur (dec cards-per-player)
                 (drop n deck)))))))

(defn burn
  [deck]
  (rest deck))

(defn deal-community
  [board deck n]
  {:board (assoc board :community-cards (take n deck))
   :deck (drop n deck)})

(defn flop
  [board deck]
  (deal-community board deck 3))

(defn turn
  [board deck]
  (deal-community board deck 1))

(defn river
  [board deck]
  (deal-community board deck 1))

(defn play-blinds
  [board]
  (-> board
      (raise (get-in board [:blinds :small]))
      (raise (- (get-in board [:blinds :big])
                (get-in board [:blinds :small])))
      (assoc :remaining-players (:players board))))

(defn preflop-stage
  [board action-ch]
  (-> board
      play-blinds
      (play-stage action-ch)))

(defn game
  [players blinds]
  (future
    (let [deck (shuffle COMPLETE-DECK)
          deck (deal deck players 2)
          action-ch (-> players
                        first
                        :action-ch)
          board (-> (init-board players blinds)
                    (preflop-stage action-ch))
          ;; flop stage
          {:keys [board deck]} (flop board (burn deck))
          board (play-stage board action-ch)
          ;; turn stage
          {:keys [board deck]} (turn board (burn deck))
          board (play-stage board action-ch)
          ;; river stage
          {:keys [board deck]} (river board (burn deck))
          board (play-stage board action-ch)
          ]
      board)))
