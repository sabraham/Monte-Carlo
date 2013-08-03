(ns montecarlo.core
  (require [clojure.core.async :as async
            :refer [<! >! <!! >!! timeout chan alt! alts!! go close!
                    sliding-buffer thread alt!! buffer]]
           [montecarlo.card :as card]
           [montecarlo.hand-evaluator :as evaluator]
           [clojure.math.combinatorics :as combo]
           [montecarlo.bet :refer :all]
           [montecarlo.action :refer :all]
           [cheshire.core :refer :all]
           [gloss.core :as gloss])
  (use [lamina.core]
       [aleph.tcp]))

(defn board->player-ids
  [board]
  (map :id (deref (:players board))))

(defn id->player
  [board id]
  (->> board :players deref
       (filter #(= id (:id %)))
       first))

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
;; Bets & Board
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
     ;;     action-ch ;; Action :: logical player -> board
     card-ch ;; Card :: board -> logical player
     board-ch ;; Board :: board -> logical player
     out-ch ;; logical player ->  real player
     quit-ch])

;; Validation layer
(extend-type Player
  ActionP
  (fold [this board] (>!! (:action-ch board) FOLD))
  (call [this board] (>!! (:action-ch board) CALL))
  (raise [this board r] (>!! (:action-ch board)
                             (max 0
                                  (min r
                                       (- (deref (:stack this))
                                          (board->needed-bet board (:id this))))))))

(defn stage-end?
  [board]
  (empty? (deref (:remaining-players board))))

(defn game-end?
  [board]
  (or (empty? (rest (deref (:players board))))
      (and (stage-end? board)
           (= 3
              (deref (:stage board))))))

(defn player-action
  [player board]
  (when (and (= (:id player)
                (first (deref (:play-order board))))
             (not (game-end? board)))
    (do
      (let [action (<!! (:listen-ch player))]
        (cond
         (is-fold? action) (fold player board)
         (is-call? action) (call player board)
         (is-raise? action) (raise player board action)
         :else (throw (Exception. "Action is not fold nor call nor raise!")))))))

(defn public-player [player]
  {:id (:id player)
   :stack (deref (:stack player))})

(defn read-board
  [board]
  (let [ks [:community-cards :bets :pots :remaining-players :play-order :time]
        public-board (zipmap ks (map #(deref (get board %)) ks))
        public-board (assoc public-board
                       :play-order (take (count (deref (:players board)))
                                         (:play-order public-board)))
        public-board (assoc public-board
                       :players (map public-player (deref (:players board))))
        ]
    (println public-board)
    public-board))

(defn run
  [player]
  (go
   (while true
     (alt!
      (:card-ch player)  ([card]
                            (swap! (:hand player) conj card)
                            (>! (:out-ch player) card))
      (:board-ch player) ([board]
                            (>! (:out-ch player) (read-board board))
                            (player-action player board))
      (:quit-ch player)  ([s] (println "i don't know how to quit you."))))))

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
     time ;; time frame
     action-ch ;; action-ch
     quit-ch ;;
     ])

(defn update-players
  [board]
  (doseq [ch (map :board-ch (deref (:players board)))]
    (go (>! ch board))))

(defn do-action
  [action board]
  (cond
   (is-fold? action) (fold board)
   (is-call? action) (call board)
   (is-raise? action) (raise board (action->raise action))
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
            #(+ (* (:bet pot) (:n pot)) %)))))

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
    (update-stacks (:pots board) winners)
    (>!! (:quit-ch board) :quit)))

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
                             ;;                             (debug-board board "top")
                             (board-action board action))
      (:quit-ch board)  ([s] (println "i don't know how to quit you."))))))

(extend-type Board
  ActionP
  (fold [this]
    (let [player (first (deref (:play-order this)))]
      (dosync
       (alter (:time this) inc)
       (alter (:bets this) (fn [x] (merge-bets (map #(->Bet (:bet %)
                                                            (disj (:players %) player)
                                                            (:original-players %)
                                                            (:n %))
                                                    x))))
       (alter (:remaining-players this) #(remove #{player} %))
       (alter (:play-order this) (fn [x] (filter #(not (= player %)) x)))
       (alter (:players this) (fn [x] (remove #(= player (:id %)) x))))))
  (call [this]
    (let [player-id (first (deref (:play-order this)))
          bet-amt (board->total-bet this)
          delta (board->needed-bet this player-id)
          player (id->player this player-id)]
      (if (pos? bet-amt)
        (let [new-bet (->Bet bet-amt
                             #{player-id} #{player-id} 1)]
          (if (= delta (deref (:stack player)))
            (dosync
             (alter (:time this) inc)
             (alter (:players this) (fn [x] (remove #(= player (:id %)) x)))
             (alter (:stack player) #(- % delta))
             (alter (:bets this) update-bets new-bet)
             (alter (:play-order this) rest)
             (alter (:remaining-players this) #(remove #{player-id} %)))
            (dosync
             (alter (:time this) inc)
             (alter (:stack player) #(- % delta))
             (alter (:bets this) update-bets new-bet)
             (alter (:play-order this) rest)
             (alter (:remaining-players this) #(remove #{player-id} %)))))
        (dosync
         (alter (:time this) inc)
         (alter (:bets this) merge-bets)
         (alter (:play-order this) rest)
         (alter (:remaining-players this) #(remove #{player-id} %))))))
  (raise [this r]
    (let [player-id (first (deref (:play-order this)))
          new-bet (->Bet (+ r (board->total-bet this))
                         #{player-id}
                         #{player-id}
                         1)
          delta (board->needed-bet this player-id)
          player (id->player this player-id)]
      (if (= (+ delta r) (deref (:stack player)))
        (dosync
         (alter (:time this) inc)
         (alter (:players this) (fn [x] (remove #(= player (:id %)) x)))
         (alter (:stack player) (fn [x] 0))
         (alter (:bets this) update-bets new-bet)
         (alter (:play-order this) rest)
         (alter (:remaining-players this)
                (fn [x]
                  (remove #{player-id} (board->player-ids this)))))
        (dosync
         (alter (:time this) inc)
         (alter (:stack player) #(- % (+ delta r)))
         (alter (:bets this) update-bets new-bet)
         (alter (:play-order this) rest)
         (alter (:remaining-players this)
                (fn [x]
                  (remove #{player-id} (board->player-ids this)))))))))

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
        time (ref 0)
        players (ref players)
        quit-ch (chan)]
    (->Board community-cards bets pots
             remaining-players play-order players blinds stage deck time
             action-ch quit-ch)))

(defn deal-hand
  [board]
  (let [n (count (deref (:players board)))
        next-player-id (first (deref (:play-order board)))]
    (doseq [[p card]
            (map list
                 (cycle (deref (:players board)))
                 (take (* 2 n) (deref (:deck board))))]
      (if (= (:id p) next-player-id)
        (>!! (:card-ch p) card)
        (go (>! (:card-ch p) card))))
    (dosync
     (alter (:deck board) #(drop (* 2 n) %)))))

(defn play-blinds
  [board]
  (let [a (:action-ch board)
        {:keys [small big]} (:blinds board)
        [p1 p2] (take 2 (deref (:players board)))]
    (dosync
     (alter (:stack p1) #(- % small))
     (alter (:bets board) update-bets (->Bet small #{(:id p1)} #{(:id p1)} 1)))
    (dosync
     (alter (:play-order board) #(drop 2 %))
     (alter (:stack p2) #(- % big))
     (alter (:bets board) update-bets (->Bet big #{(:id p2)} #{(:id p2)} 1)))))

(defn hand
  [board players]
  (play-blinds board)
  (doseq [p players] (run p))
  (run-board board)
  (deal-hand board)
  (update-players board))

(defn game
  [players blinds]
  (let [action-ch (chan (sliding-buffer (count players)))]
    (loop [players players]
      (let [board (init-board players blinds action-ch)]
        (hand board players)
        (<!! (:quit-ch board))
        (recur (concat (rest players) (list (first players))))))))

(def DATABASE (atom {}))

(defn error-msg
  [p status-code msg]
  (go (>! (:out-ch p) {:status status-code :msg msg})))

(defn ok-msg
  [p]
  (go (>! (:out-ch p) {:status 0 :msg "OK"})))

(defn pos-int?
  [x]
  (and (integer? x) (pos? x)))

(defn valid-blinds?
  [p blinds]
  (let [blind-vals (vals blinds)]
    (if (every? pos-int? blind-vals)
      true
      (do
        (error-msg p -12 "Blinds must be positive integers")
        false))))

(defn valid-num-players?
  [p n]
  (println (str "in this " n))
  (if (pos-int? n)
    true
    (do
      (error-msg p -5 "n (number of players) must be a postive integer.")
      false)))

(defn valid-room?
  [p name]
  (if (get @DATABASE name)
    (do
      (error-msg p -5 "room already exists.")
      false)
    (if (nil? name)
      (do
        (error-msg p -5 "empty room name.")
        false)
      true)))

;; TODO race condition
(defn create-new-room
  [p {:keys [name n blinds] :or {blinds {:small 5 :big 10}}}]
  (println (str name " " n " " blinds))
  (when (and (valid-room? p name)
             (valid-num-players? p n)
             (valid-blinds? p blinds))
    (let [q (chan (buffer n))]
      (swap! DATABASE assoc name {:players (list)
                                  :n n
                                  :q q})
      (go (while true
            (alt! q ([p] (do
                           (swap! DATABASE #(update-in % [name :players] conj p))
                           (println (str "count: " (count (get-in @DATABASE [name :players])) " " n))
                           (when (= (count (get-in @DATABASE [name :players])) n)
                             (game (get-in @DATABASE [name :players])
                                   blinds)))))))
      (ok-msg p))))

(defn join-room
  [p {:keys [name]}]
  (if-let [q (get-in @DATABASE [name :q])]
    (do
      (go (>! q p))
      (ok-msg p))
    (error-msg -2 (str "Room \"" name "\" does not exist yet."))))

(defn handler
  [ch client-info]
  (let [p (->Player (gensym) (ref 100) (atom [])
                    (chan (sliding-buffer 1))
                    (chan) (chan) (chan) (chan))]
    (println "new connection")
    (receive-all ch #(try
                       (let [req (clojure.walk/keywordize-keys (parse-string %))]
                         (case (:type req)
                           "new_room" (create-new-room p req)
                           "join_room" (join-room p req)
                           "play" (go (>! (:listen-ch p) (:amt req)))
                           (error-msg p -1 "bad \"type\" argument")))
                       (catch com.fasterxml.jackson.core.JsonParseException e
                         (error-msg p -17 "You sent me bad json!"))))
    (go
     (while true
       (alt!
        (:out-ch p)  ([update]
                        (enqueue ch (generate-string update))))))))

(start-tcp-server handler
                  {:port 10000, :frame (gloss/string :utf-8 :delimiters ["\r\n"])})
