(ns montecarlo.hand-evaluator
  (require [montecarlo.card :as card]
           [clojure.math.combinatorics :as combo]
           [montecarlo.database :as mc.database]))

(defn constant-coll?
  [coll]
  (when (every? true? (map = coll (rest coll)))
    coll))

(defn constant-cards?
  [cards att]
  (when (constant-coll? (map att cards))
    cards))

;; NAIVE

(defn high-card
  [card-a card-b]
  (if (pos? (compare (:rank card-a) (:rank card-b)))
    card-a
    card-b))

(defn high-hand
  [hand-a hand-b]
  (let [high-a (reduce high-card hand-a)
        high-b (reduce high-card hand-b)]
    (if (= (high-card high-a high-b) high-a)
      hand-a
      hand-b)))

(defn straight?
  [cards]
  (loop [ranks (sort (map :rank cards))]
    (let [[x & xs] ranks]
      (if (empty? xs)
        cards
        (when (= (inc x)
                 (first xs))
          (recur (rest ranks)))))))

(defn flush?
  [cards]
  (constant-cards? cards :suit))

(defn n-of-a-kinds
  [cards n]
  (filter #(constant-cards? % :rank)
          (combo/combinations cards n)))

(defn n-of-a-kind?
  [cards n]
  (let [kinds (n-of-a-kinds cards n)]
    (when (not (empty? kinds))
      (reduce high-hand kinds))))

(reduce high-hand (n-of-a-kinds (filter #(= 2 (:rank %)) card/COMPLETE-DECK) 3))

(defn four-of-a-kind?
  [cards]
  (n-of-a-kind? cards 4))

(defn three-of-a-kind?
  [cards]
  (n-of-a-kind? cards 3))

(defn pair?
  [cards]
  (n-of-a-kind? cards 2))

(defn n-pairs?
  [cards k1 k2]
  (let [perms (combo/permutations cards)
        sorter (fn [x] (sort-by :rank x))
        poss  (distinct (map #(map sorter [(take k1 %) (take k2 (drop k1 %))])
                             perms))
        hits (filter (fn [poss] (every? #(constant-cards? % :rank) poss))
                     poss)]
    (first hits)))

(n-pairs? [(card/->Card :h 9)
           (card/->Card :d 8)
           (card/->Card :s 9)
           (card/->Card :c 8)
           (card/->Card :h 8)]
          3 2)

(defn full-house?
  [cards]
  (n-pairs? cards 3 2))

(defn two-pair?
  [cards]
  (n-pairs? cards 2 2))

(defn cards->ranks
  [cards]
  (vec (sort > (map :rank cards))))

(defn ret
  [value cards hit]
  [value (cards->ranks hit) (cards->ranks (remove (set hit) cards))])

(defn ret-full-house
  [cards hit]
  [6 (map :rank (flatten hit)) []])

(defn ret-two-pair
  [cards hit]
  [2 (cards->ranks (flatten hit)) (cards->ranks (remove (set (flatten hit)) cards))])

(defn evaluator
  [cards]
  (let [is-straight (straight? cards)
        is-flush (flush? cards)
        ranks (cards->ranks cards)]
    (if (and is-straight is-flush)
      [8 ranks []]
      (if-let [hit (four-of-a-kind? cards)]
        (ret 7 cards hit)
        (if-let [hit (full-house? cards)]
          (ret-full-house cards hit)
          (if is-flush
            [5 ranks []]
            (if is-straight
              [4 ranks []]
              (if-let [hit (three-of-a-kind? cards)]
                (ret 3 cards hit)
                (if-let [hit (two-pair? cards)]
                  (ret-two-pair cards hit)
                  (if-let [hit (pair? cards)]
                    (ret 1 cards hit)
                    (ret 0 [] cards)))))))))))


(comment
  (defn comparable-hand-values
    [val-a val-b]
    (loop [l val-a
           r val-b
           l-stack []
           r-stack []]
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
                c))))))))

(defn max-hand-value
  [val-a val-b]
  (if (pos? (compare val-a val-b))
    val-a
    val-b))

(defn player->hand-value
  [board player-id]
  (println (str "\n"
                "player ID: " player-id "\n"
                "board name: " (:room board) "\n"
                "player-hand: " (mc.database/player-hand player-id (:room board))
                "\n"))
  (let [pocket @(mc.database/player-hand player-id (:room board))
        available-cards (concat (deref (:community-cards board)) pocket)
        hands (combo/combinations available-cards 5)]
    (reduce max-hand-value (map evaluator hands))))