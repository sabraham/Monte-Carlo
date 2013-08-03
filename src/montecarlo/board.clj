(ns montecarlo.board
  (require [montecarlo.action :refer :all]
           [montecarlo.bet :as mc.bet]
           [montecarlo.card :as mc.card]
           [montecarlo.helpers :as mc.helpers]
           [montecarlo.gameplay :as mc.gameplay]
           [clojure.core.async :as async
            :refer [go alt! chan]]))

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
     quit-ch])

(extend-type Board
  ActionP
  (fold [this]
    (let [player (first (deref (:play-order this)))]
      (dosync
       (alter (:time this) inc)
       (alter (:bets this) (fn [x] (mc.bet/merge-bets (map #(mc.bet/->Bet (:bet %)
                                                            (disj (:players %) player)
                                                            (:original-players %)
                                                            (:n %))
                                                    x))))
       (alter (:remaining-players this) #(remove #{player} %))
       (alter (:play-order this) (fn [x] (filter #(not (= player %)) x)))
       (alter (:players this) (fn [x] (remove #(= player (:id %)) x))))))
  (call [this]
    (let [player-id (first (deref (:play-order this)))
          bet-amt (mc.helpers/board->total-bet this)
          delta (mc.helpers/board->needed-bet this player-id)
          player (mc.helpers/id->player this player-id)]
      (if (pos? bet-amt)
        (let [new-bet (mc.bet/->Bet bet-amt
                             #{player-id} #{player-id} 1)]
          (if (= delta (deref (:stack player)))
            (dosync
             (alter (:time this) inc)
             (alter (:players this) (fn [x] (remove #(= player (:id %)) x)))
             (alter (:stack player) #(- % delta))
             (alter (:bets this) mc.bet/update-bets new-bet)
             (alter (:play-order this) rest)
             (alter (:remaining-players this) #(remove #{player-id} %)))
            (dosync
             (alter (:time this) inc)
             (alter (:stack player) #(- % delta))
             (alter (:bets this) mc.bet/update-bets new-bet)
             (alter (:play-order this) rest)
             (alter (:remaining-players this) #(remove #{player-id} %)))))
        (dosync
         (alter (:time this) inc)
         (alter (:bets this) mc.bet/merge-bets)
         (alter (:play-order this) rest)
         (alter (:remaining-players this) #(remove #{player-id} %))))))
  (raise [this r]
    (let [player-id (first (deref (:play-order this)))
          new-bet (mc.bet/->Bet (+ r (mc.helpers/board->total-bet this))
                         #{player-id}
                         #{player-id}
                         1)
          delta (mc.helpers/board->needed-bet this player-id)
          player (mc.helpers/id->player this player-id)]
      (if (= (+ delta r) (deref (:stack player)))
        (dosync
         (alter (:time this) inc)
         (alter (:players this) (fn [x] (remove #(= player (:id %)) x)))
         (alter (:stack player) (fn [x] 0))
         (alter (:bets this) mc.bet/update-bets new-bet)
         (alter (:play-order this) rest)
         (alter (:remaining-players this)
                (fn [x]
                  (remove #{player-id} (mc.helpers/board->player-ids this)))))
        (dosync
         (alter (:time this) inc)
         (alter (:stack player) #(- % (+ delta r)))
         (alter (:bets this) mc.bet/update-bets new-bet)
         (alter (:play-order this) rest)
         (alter (:remaining-players this)
                (fn [x]
                  (remove #{player-id} (mc.helpers/board->player-ids this)))))))))

(defn debug-board
  [board header]
  (println (str "\n"
                header "\n"
                "players: " (seq (mc.helpers/board->player-ids board)) "\n"
                "remaining-players: " (seq (deref (:remaining-players board))) "\n"
                "play-order: " (seq (take 4 (deref (:play-order board)))) "\n"
                "bets: " (seq (deref (:bets board))) "\n"
                "\n")))

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

(defn board-action
  [board action]
  (do-action action board)
  (if (mc.gameplay/game-end? board)
    (mc.gameplay/end-game board)
    (do
      (when (mc.gameplay/stage-end? board)
        (mc.gameplay/stage-transition board))
      (update-players board))))

(defn run-board
  [board]
  (go
   (while true
     (alt!
      (:action-ch board)  ([action]
                             (board-action board action))
      (:quit-ch board)  ([s] (println "i don't know how to quit you."))))))

(defn init-board
  [players blinds action-ch]
  (let [community-cards (ref (list))
        bets (ref (list))
        pots (ref (list))
        remaining-players (ref (map :id players))
        play-order (ref (cycle (map :id players)))
        stage (ref 0)
        deck (ref (shuffle mc.card/COMPLETE-DECK))
        time (ref 0)
        players (ref players)
        quit-ch (chan)]
    (->Board community-cards bets pots
             remaining-players play-order players blinds stage deck time
             action-ch quit-ch)))