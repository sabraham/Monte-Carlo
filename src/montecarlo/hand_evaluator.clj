(ns montecarlo.hand-evaluator
  ;;  (import pokerai.game.eval.spears2p2.StateTableEvaluator)
  (require [montecarlo.card :as card]
           [clojure.math.combinatorics :as combo]))

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

(defn straight? ;; maybe faster to sum ranks and check = what straight should equal
  [cards]
  (loop [[x & xs] (sort (map :rank cards))]
    (if (empty? xs)
      cards
      (when (= (inc x)
               (first xs))
        (recur (rest xs))))))

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
                             perms))]
    (some (fn [poss] (every? #(constant-cards? % :rank) poss))
          poss)))

(defn full-house?
  [cards]
  (n-pairs? cards 3 2))

(defn two-pair?
  [cards]
  (n-pairs? cards 2 2))

(defn high-card?
  [cards]
  (reduce high-card cards))
