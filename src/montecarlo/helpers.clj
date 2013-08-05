(ns montecarlo.helpers
  (require
   [montecarlo.database :as mc.database]))

(defn id->player
  [board id]
  (->> board :players deref
       (filter #(= id (:id %)))
       first))

(defn board->player-ids
  [board]
  (map :id (deref (:players board))))

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

(defn public-player [player]
  {:id (:id player)
   :stack (deref (mc.database/player-stack (:id player)))})

(defn read-board
  [board]
  (let [ks [:community-cards :bets :pots :remaining-players :play-order :time]
        public-board (zipmap ks (map #(deref (get board %)) ks))
        public-board (assoc public-board
                       :play-order (take (count (deref (:players board)))
                                         (:play-order public-board)))
        public-board (assoc public-board
                       :players (map public-player (deref (:players board))))]
    (println public-board)
    public-board))