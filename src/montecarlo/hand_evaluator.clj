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

(constant-cards?
 (take 4 card/COMPLETE-DECK) :suit)

(defn n-of-a-kind?
  [cards n]
  (some #(constant-cards? % :rank) (combo/combinations cards n)))

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
