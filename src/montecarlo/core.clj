(ns montecarlo.core
  (require [clojure.core.async :as async
            :refer [<! >! <!! >!! timeout chan alt! alts!! go close!]]
           [montecarlo.card :as card]
           [montecarlo.hand-evaluator :as evaluator]
           [clojure.math.combinatorics :as combo]))

(defn board->player-ids
  [board]
  (map :id (deref (:players board))))

(defn debug-board
  [board header]
  (println (str "\n"
                header "\n"
                "players: " (seq (board->player-ids board)) "\n"
                "remaining-players: " (seq (deref (:remaining-players board))) "\n"
                "play-order: " (seq (take 4 (deref (:play-order board)))) "\n"
                "bets: " (seq (deref (:bets board))) "\n"
                "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Bet
    [bet players original-players]) ;; player-ids

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
  [(->Bet x (conj (:players bet) player) (conj (:original-players bet) player))
   (update-in bet [:bet] #(- % x))])

(defn call-bet
  [bets player]
  (let [bet (first bets)]
    (conj (rest bets) (assoc bet
                        :players (conj (:players bet) player)
                        :original-players (conj (:original-players bet) player)))))

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
                              (:original-players bet-i))))
          (recur (rest input)
                 (conj output
                       bet-i))))
      (reverse output))))

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
        (concat ret [(->Bet bet #{player} #{player})])))))

(defn board->total-bet
  [board]
  (if-let [total-bet (reduce + (map :bet (deref (:bets board))))]
    total-bet
    0))

(defn board->needed-bet
  [board player-id]
  (let [unmet-bets (filter #(not (contains? (:players %) player-id))
                           (deref (:bets board)))]
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
  (call [this total-bet]
    (dosync
     (alter (:stack this) #(max 0 (- % total-bet))))
    (println "player called"))
  (raise [this r total-bet]
    (println "player to raise")
    (dosync
     (alter (:stack this) #(max 0 (- % total-bet r))))))

(defn stage-end?
  [board]
  (empty? (deref (:remaining-players board))))

(defn game-end?
  [board]
  (debug-board board "game-end")
  (println (str "stage is: " (deref (:stage board))))
  (or (empty? (rest (deref (:players board))))
      (and (stage-end? board)
           (= 3
              (deref (:stage board))))))

(defn player-action
  [player board]
  (println (type board))
  (when (and (= (:id player)
                (first (deref (:play-order board))))
             (not (game-end? board)))
    (do
      (let [action (<!! (:listen-ch player))]
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

(defn run
  [player]
  (go
   (while true
     (alt!
      (:card-ch player)  ([card]
                            (swap! (:hand player) conj card))
      (:board-ch player) ([board]
                            (player-action player board))
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
     stage  ;; stage 0 - preflop, 1 - flop, 2 - turn, 3 - river
     deck ;;
     action-ch ;; action-ch
     quit-ch ;;
     ])

(defn read-board
  [board]
  (comment
    (let [public-board (select-keys board [:community-cards :bets :pots
                                           :remaining-players :play-order
                                           :blinds])]
      (assoc public-board :players (map public-player (:players board)))))
  board)

(defn update-players
  [board]
  (let [new-board (read-board board)]
    (doseq [ch (map :board-ch (deref (:players board)))]
      (go (>! ch new-board)))))

(defn do-action
  [action board]
  (cond
   (is-fold? action) (fold board)
   (is-call? action) (call board)
   (is-raise? action) (println "is raise!!" (raise board (action->raise action)))
   :else (throw (Exception. "Action is not fold nor call nor raise!"))))


(defn burn
  [deck]
  (dosync
   (alter deck #(drop 1 %))))

(defn deal-community
  [board n]
  (dosync
   (alter (:community-cards board) concat (take n (deref (:deck board))))
   (alter (:deck board) #(drop n %))))

(defn flop
  [board]
  (burn (:deck board))
  (deal-community board 3))

(defn turn
  [board]
  (burn (:deck board))
  (deal-community board 1))

(defn river
  [board]
  (burn (:deck board))
  (deal-community board 1))

(defn deal-stage
  [board]
  (condp = (deref (:stage board))
    0 (flop board)
    1 (turn board)
    2 (river board)))

(defn stage-transition
  [board]
  (dosync
   (deal-stage board)
   (alter (:remaining-players board) (fn [_] (board->player-ids board)))
   (alter (:pots board) concat (deref (:bets board)))
   (alter (:bets board) (fn [_] (list)))
   (alter (:play-order board) (fn [_] (cycle (board->player-ids board))))
   (alter (:stage board) inc)))

(defn comparable-hand-values
  [val-a val-b]
  (loop [l val-a
         r val-b
         l-stack (list)
         r-stack (list)]
    (if (> (count l) (count r))
      1
      (if (< (count l) (count r))
        -1
        (if (empty? l)
          (if (empty? l-stack)
            0
            (recur (first l-stack) (first r-stack) (rest l-stack) (rest r-stack)))
          (let [[x & xs] l
                [y & ys] r]
            (if (coll? x)
              (recur x y (conj l-stack x) (conj r-stack y))
              (let [c (compare x y)]
                (if (= c 0)
                  (recur xs ys l-stack r-stack)
                  c)))))))))

(defn compare-hand-values
  [val-a val-b]
  (if (neg? (comparable-hand-values val-a val-b))
    val-b
    val-a))

(defn player->hand-value
  [board player]
  (let [pocket (:hand player)
        available-cards (concat (deref (:community-cards board)) @pocket)
        hands (combo/combinations available-cards 5)]
    (reduce compare-hand-values (map evaluator/evaluator hands))))

(defn update-stacks
  [pots winners]
  (doseq [pot (deref pots)]
    (dosync
     (alter (:stack (first (filter #((:players pot) (:id %)) winners)))
            #(+ (* (:bet pot) (count (:original-players pot))) %)))))

(defn end-game
  [board]
  (dosync
   (alter (:pots board) concat (deref (:bets board)))
   (alter (:bets board) (fn [_] (list))))
  (let [hand-values (map #(player->hand-value board %) (deref (:players board)))
        winners (reverse (map :player (sort-by :hand-value comparable-hand-values
                                               (map #(hash-map :player %1 :hand-value %2)
                                                    (deref (:players board))
                                                    hand-values))))]
    (update-stacks (:pots board) winners)))

(defn board-action
  [board action]
  (do-action action board)
  (if (game-end? board)
    (end-game board)
    (do
      (when (stage-end? board)
        (stage-transition board))
      (update-players board))))

(defn run-board
  [board]
  (go
   (while true
     (alt!
      (:action-ch board)  ([action]
                             (debug-board board "top")
                             (board-action board action))
      (:quit-ch board)  ([s] (println "i don't know how to quit you."))))))

(extend-type Board
  ActionP
  (fold [this]
    (let [player (first (deref (:play-order this)))]
      (dosync
       (alter (:bets this) (fn [x] (merge-bets (map #(->Bet (:bet %)
                                                            (disj (:players %) player)
                                                            (:original-players %))
                                                    x))))
       (alter (:remaining-players this) #(remove #{player} %))
       (alter (:play-order this) (fn [x] (filter #(not (= player %)) x)))
       (alter (:players this) (fn [x] (remove #(= player (:id %)) x)))
       )))
  (call [this]
    (let [player (first (deref (:play-order this)))
          bet-amt (board->total-bet this)]
      (if (pos? bet-amt)
        (let [new-bet (->Bet bet-amt
                             #{player} #{player})]
          (dosync
           (alter (:bets this) update-bets new-bet)
           (alter (:play-order this) rest)
           (alter (:remaining-players this) #(remove #{player} %))))
        (dosync
         (alter (:bets this) merge-bets)
         (alter (:play-order this) rest)
         (alter (:remaining-players this) #(remove #{player} %))))))
  (raise [this r]
    (let [player (first (deref (:play-order this)))
          new-bet (->Bet (+ r (board->total-bet this))
                         #{player}
                         #{player})]
      (dosync
       (alter (:bets this) update-bets new-bet)
       (alter (:play-order this) rest)
       (alter (:remaining-players this)
              (fn [x]
                (remove #{player} (board->player-ids this))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn init-board
  [players blinds action-ch]
  (let [community-cards (ref (list))
        bets (ref (list))
        pots (ref (list))
        remaining-players (ref (map :id players))
        play-order (ref (cycle (map :id players)))
        stage (ref 0)
        deck (ref (shuffle card/COMPLETE-DECK))
        players (ref players)
        quit-ch (chan)]
    (->Board community-cards bets pots
             remaining-players play-order players blinds stage deck
             action-ch quit-ch)))

(defn deal-hand
  [board]
  (let [n (count (deref (:players board)))]
    (doseq [[p card]
            (map list
                 (cycle (deref (:players board)))
                 (take (* 2 n) (deref (:deck board))))]
      (go (>! (:card-ch p) card)))
    (dosync
     (alter (:deck board) #(drop (* 2 n) %)))))

(defn play-blinds
  [board]
  (let [a (:action-ch board)
        {:keys [small big]} (:blinds board)
        [p1 p2] (take 2 (deref (:players board)))]
    (dosync
     (alter (:stack p1) #(- % small))
     (alter (:bets board) update-bets (->Bet small #{(:id p1)} #{(:id p1)})))
    (dosync
     (alter (:play-order board) #(drop 2 %))
     (alter (:stack p2) #(- % big))
     (alter (:bets board) update-bets (->Bet big #{(:id p2)} #{(:id p2)})))))

(defn game
  [players blinds a]
  (let [board (init-board players blinds a)]
    (play-blinds board)
    (doseq [p players] (run p))
    (run-board board)
    (deal-hand board)
    (update-players board)))