(ns montecarlo.gameplay
  (require
   [clojure.core.async :as async
    :refer [>!! <!! go chan]]
   [montecarlo.helpers :as mc.helpers]
   [montecarlo.bet :as mc.bet]
   [montecarlo.hand-evaluator :as mc.evaluator]
   [montecarlo.card :as mc.card]
   [montecarlo.database :as mc.database]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gameplay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn stage-end?
  [board]
  (empty? (deref (:remaining-players board))))

(defn game-end?
  [board]
  (or (empty? (rest (deref (:players board))))
      (and (stage-end? board)
           (= 3
              (deref (:stage board))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dealing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn deal-hand
  [board]
  (let [n (count (deref (:players board)))
        next-player-id (first (deref (:play-order board)))]
    (doseq [[p card]
            (map list
                 (cycle (deref (:players board)))
                 (take (* 2 n) (deref (:deck board))))]
      (if (= (:id p) next-player-id)
        (>!! (:card-ch p) {:card card :room (:room board)})
        (go (>! (:card-ch p) {:card card :room (:room board)}))))
    (dosync
     (alter (:deck board) #(drop (* 2 n) %)))))

(defn play-blinds
  [board]
  (let [a (:action-ch board)
        {:keys [small big]} (:blinds board)
        [p1 p2] (take 2 (deref (:players board)))]
    (dosync
     (alter (mc.database/player-stack (:id p1)) #(- % small))
     (alter (:bets board) mc.bet/update-bets (mc.bet/->Bet small #{(:id p1)} #{(:id p1)} 1)))
    (dosync
     (alter (:play-order board) #(drop 2 %))
     (alter (mc.database/player-stack (:id p2)) #(- % big))
     (alter (:bets board) mc.bet/update-bets (mc.bet/->Bet big #{(:id p2)} #{(:id p2)} 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sync
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn stage-transition
  [board]
  (dosync
   (deal-stage board)
   (alter (:remaining-players board) (fn [_] (mc.helpers/board->player-ids board)))
   (alter (:pots board) concat (deref (:bets board)))
   (alter (:bets board) (fn [_] (list)))
   (alter (:play-order board) (fn [_] (cycle (mc.helpers/board->player-ids board))))
   (alter (:stage board) inc)))

(defn update-stacks
  [pots winners]
  (doseq [pot (deref pots)]
    (let [pot-intersections (map clojure.set/intersection
                                 (repeat (:players pot))
                                 winners)
          pot-winners (first (filter #(not (empty? %)) pot-intersections))
          total-pot (* (:bet pot) (:n pot))
          pot-share (int (/ total-pot (count pot-winners)))]
      (dosync
       (doseq [p pot-winners]
         (alter (mc.database/player-stack p)
                #(+ pot-share %)))))))

(defn group-winners
  [input]
  (map #(apply hash-set %) (map #(map :player %) (partition-by :hand-value input))))

(defn end-game
  [board]
  (dosync
   (alter (:pots board) concat (deref (:bets board)))
   (alter (:bets board) (fn [_] (list))))
  (let [player-ids (map :id (deref (:players board)))
        hand-values (map #(mc.evaluator/player->hand-value board %) player-ids)
        winners (group-winners (reverse (sort-by :hand-value
                                                 (map #(hash-map :player %1 :hand-value %2)
                                                      player-ids
                                                      hand-values))))]
    (update-stacks (:pots board) winners)
    (doseq [p-id (map :id (deref (:original-players board)))]
      (mc.database/reset-hand p-id (:room board)))
    (let [next-players (take (count (deref (:original-players board)))
                             (rest (cycle (deref (:original-players board)))))]
      (dosync
       (alter (:community-cards board) (fn [_] (list)))
       (alter (:bets board) (fn [_] (list)))
       (alter (:pots board) (fn [_] (list)))
       (alter (:remaining-players board) (fn [_] (map :id next-players)))
       (alter (:play-order board) (fn [_] (cycle (map :id next-players))))
       (alter (:stage board) (fn [_] 0))
       (alter (:deck board) (fn [_] (shuffle mc.card/COMPLETE-DECK)))
       (alter (:time board) (fn [_] 0))
       (alter (:original-players board) (fn [_] next-players))
       (alter (:players board) (fn [_] next-players))))
    (play-blinds board)
    (deal-hand board)))
