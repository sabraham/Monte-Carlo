(ns montecarlo.core
  (require [clojure.core.async :as async
            :refer [<! >! <!! >!! timeout chan alt! alts!! go close!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Card
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Card
    [suit rank])

(def COMPLETE-DECK
  (for [suit [:hearts :diamonds :spades :clubs]
        rank (range 2 15)]
    (->Card suit rank)))

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
  (if-let [total-bet (reduce + (map :bet (:bets board)))]
    total-bet
    0))

(defn board->needed-bet
  [board player-id]
  (let [unmet-bets (filter #(not (contains? (:players %) player-id)) (:bets board))]
    (if-let [bet (reduce + (map :bet unmet-bets))]
      bet
      0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Player
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Player
    [id stack hand
     listen-ch ;; Action :: real player -> logical player
     action-ch ;; Action :: logical player -> board
     card-ch ;; Card :: board -> logical player
     board-ch ;; Board :: board -> logical player
     quit-ch])

(extend-type Player
  ActionP
  (fold [this] (println "player folded"))
  (call [this total-bet] (swap! (:stack this) #(max 0 (- % total-bet)))
    (println "player called"))
  (raise [this r total-bet]
    (println "player to raise")
    (swap! (:stack this) #(max 0 (- % total-bet r)))))

(defn run
  [player]
  (go
   (while true
     (alt!
      (:card-ch player)  ([card]
                            (swap! (:hand player) conj card))
      (:board-ch player) ([board]
                            (when (= (:id player)
                                     (first (:play-order board)))
                              (do
                                (let [action (<!! (:listen-ch player))]
                                  (println (str "player " (:id player) " action: " action))
                                  (cond
                                   (is-fold? action) (fold player)
                                   (is-call? action) (call player
                                                           (board->needed-bet board
                                                                              (:id player)))
                                   (is-raise? action) (raise player
                                                             (action->raise action)
                                                             (board->needed-bet board (:id player)))
                                   :else (throw (Exception. "Action is not fold nor call nor raise!")))
                                  (>!! (:action-ch player) action)
                                  ;; TODO here: send update to real player
                                  ))))
      (:quit-ch player)  ([s] (println "i don't know how to quit you."))))))

(defn public-player [player]
    (select-keys player [:id :stack]))

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

(defn read-board
  [board]
  (let [public-board (select-keys board [:community-cards :bets :pots
                                         :remaining-players :play-order
                                         :blinds])]
    (assoc public-board :players (map public-player (:players board)))))

(defn update-players
  [board]
  (let [new-board (read-board board)]
    (doseq [ch (map :board-ch (:players board))]
      (go (>! ch new-board)))))

(extend-type Board
  ActionP
  (fold [this]
    (let [player (first (:play-order this))
          remaining-players (remove #{player} (:remaining-players this))
          play-order (filter #(not (= player %)) (:play-order this))
          players (remove #(= player (:id %)) (:players this))]
      (assoc this :remaining-players remaining-players :play-order play-order
             :players players)))
  (call [this]
    (let [player (first (:play-order this))
          bet-amt (board->total-bet this)]
      (if (pos? bet-amt)
        (let [new-bet (->Bet bet-amt
                             #{player})
              bets (update-bets (:bets this) new-bet)]
          (assoc this
            :bets (merge-bets bets)
            :play-order (rest (:play-order this))
            :remaining-players (remove #{player} (:remaining-players this))))
        (assoc this
          :bets (merge-bets (:bets this))
          :play-order (rest (:play-order this))
          :remaining-players (remove #{player} (:remaining-players this))))))
  (raise [this r]
    (let [player (first (:play-order this))
          new-bet (->Bet (+ r (board->total-bet this))
                         #{player})
          bets (update-bets (:bets this) new-bet)]
      (assoc this
        :bets (merge-bets bets)
        :play-order (rest (:play-order this))
        :remaining-players (remove #{player} (map :id (:players this)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn game-end?
  [board]
  (empty? (rest (:players board))))

(defn stage-end?
  [board]
  (do
    (println (str "stage-end: "         (or (empty? (:remaining-players board))
                                            (game-end? board))))
    (or (empty? (:remaining-players board))
        (game-end? board))))


(defn stage-transition
  [board]
  (assoc board
    :remaining-players (map :id (:players board))
    :bets (list)
    :pots (concat (:pots board) (:bets board))
    :play-order (cycle (map :id (:players board)))))

(defn play-stage
  [board action-ch]
  (loop [board board]
    (println (str "\n"
                  "players: " (seq (map :id (:players board))) "\n"
                  "remaining-players: " (seq (:remaining-players board)) "\n"
                  "play-order: " (seq (take 4 (:play-order board))) "\n"
                  "bets: " (seq (:bets board))
                  "\n"))
    (let [player (first (:play-order board))]
      (if (stage-end? board)
        (do
          (println "it is the stage-end!")
          (stage-transition board))
        (let [action (<!! action-ch)]
          (println (str "\n"
                        "play-stage:\n"
                        "action: " action
                        "\n"))
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
           :else (throw (Exception. "Action is not fold nor call nor raise!"))))))))

(defn init-board
  [players blinds]
  (let [community-cards (list)
        bets (list)
        pots (list)
        remaining-players (map :id players)
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

;; (comment
;;   (defn play-blinds
;;     [board]
;;     (-> board
;;         (raise (get-in board [:blinds :small]))
;;         (raise (- (get-in board [:blinds :big])
;;                   (get-in board [:blinds :small])))
;;         (assoc :remaining-players (map :id (:players board))))))

(defn play-blinds
  [board]
  (let [[l1 l2] (map :listen-ch  (take 2 (:players board)))
        small-blind (get-in board [:blinds :small])
        big-blind (get-in board [:blinds :big])
        a (-> board :players first :action-ch)]
    (println "begin play-blinds")
    (>!! l1 small-blind)
    (update-players board)
    (<!! a) ;; discard item in action-ch
    (println "updated board")
    ;; players get board with no play, but p1 will get
    ;; stack deducted
    (let [board (raise board small-blind)]
      ;; board now has that p1 put in small blind and play has moved to p2
      (println "begin l2")
      (>!! l2 (- big-blind small-blind))
      (update-players board)
      (<!! a) ;; discard item in action-ch
      (println "updated board - 2")
      ;; players get the board with small-blind
      (let [board (raise board (- big-blind small-blind))]
        ;; board now has that p2 has put in big blind
        (println (str "blinds: player to play is " (-> board :play-order first)))
        (assoc board :remaining-players (map :id (:players board)))))))

(defn preflop-stage
  [board action-ch]
  (let [blinds-board (play-blinds board)]
    (println (str "preflop-stage: player to play is " (-> blinds-board :play-order first)))
    (update-players blinds-board) ;;
    (play-stage blinds-board action-ch)))

(defn game
  [players blinds]
  (future
    (let [deck (shuffle COMPLETE-DECK)
          deck (deal deck players 2)
          action-ch (-> players
                        first
                        :action-ch)
          board (-> (init-board players blinds)
                    (preflop-stage action-ch))]
      (if (game-end? board)
        board
        ;; flop stage
        (let [{:keys [board deck]} (flop board (burn deck))
              _ (update-players board)
              board (play-stage board action-ch)]
          (if (game-end? board)
            board
            ;; turn stage
            (let [{:keys [board deck]} (turn board (burn deck))
                  board (play-stage board action-ch)]
              (if (game-end? board)
                board
                ;; river stage
                (let [{:keys [board deck]} (river board (burn deck))
                      board (play-stage board action-ch)]
                  board)))))))))
