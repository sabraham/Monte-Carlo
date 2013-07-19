(ns montecarlo.core
  (require [clojure.core.async :as async
            :refer [<! >! <!! >!! timeout chan alt! alts!! go close!]]))

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
        (concat ret [(->Bet bet #{player})])))))

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

(defn game-end?
  [board]
  (debug-board board "game-end")
  (println (str "stage is: " (deref (:stage board))))
  (or (empty? (rest (deref (:players board))))
      (= 4
         (deref (:stage board)))))

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

(defn stage-end?
  [board]
  (or (empty? (deref (:remaining-players board)))
      (game-end? board)))


(defn stage-transition
  [board]
  (dosync
   (alter (:remaining-players board) (fn [_] (board->player-ids board)))
   (alter (:pots board) concat (deref (:bets board)))
   (alter (:bets board) (fn [_] (list)))
   (alter (:play-order board) (fn [_] (cycle (board->player-ids board))))
   (alter (:stage board) inc)
   ))

(defn end-game
  [board]
  (let [winner 1]
    (println "end game!")))

(defn board-action
  [board action]
  (do-action action board)
  (if (stage-end? board)
    (do
      (stage-transition board)
      (if (game-end? board)
        (end-game board)
        (update-players board)))
    (update-players board)))

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
       (alter (:remaining-players this) #(remove #{player} %))
       (alter (:play-order this) (fn [x] (filter #(not (= player %)) x)))
       (alter (:players this) (fn [x] (remove #(= player (:id %)) x)))
       )))
  (call [this]
    (let [player (first (deref (:play-order this)))
          bet-amt (board->total-bet this)]
      (if (pos? bet-amt)
        (let [new-bet (->Bet bet-amt
                             #{player})]
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
  (let [community-cards (list)
        bets (ref (list))
        pots (ref (list))
        remaining-players (ref (map :id players))
        play-order (ref (cycle (map :id players)))
        stage (ref 0)
        deck (ref COMPLETE-DECK)
        players (ref players)
        quit-ch (chan)]
    (->Board community-cards bets pots
             remaining-players play-order players blinds stage deck
             action-ch quit-ch)))

(defn burn
  [deck]
  (rest deck))

(defn deal-community
  [board deck n]
  (dosync
   (alter (:community-cards board) conj (take n @deck))
   (alter deck #(drop n %))))

(defn flop
  [board deck]
  (deal-community board deck 3))

(defn turn
  [board deck]
  (deal-community board deck 1))

(defn river
  [board deck]
  (deal-community board deck 1))

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
     (alter (:bets board) update-bets (->Bet small #{(:id p1)})))
    (dosync
     (alter (:play-order board) #(drop 2 %))
     (alter (:stack p2) #(- % big))
     (alter (:bets board) update-bets (->Bet big #{(:id p2)})))))

(defn game
  [players blinds a]
  (let [board (init-board players blinds a)]
    (play-blinds board)
    (doseq [p players] (run p))
    (run-board board)
    (deal-hand board)
    (update-players board)))