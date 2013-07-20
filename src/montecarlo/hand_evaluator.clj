(ns montecarlo.hand-evaluator
  ;;  (import pokerai.game.eval.spears2p2.StateTableEvaluator)
  (require [montecarlo.card :as card]
           [clojure.math.combinatorics :as combo]))

(defn constant-coll?
  [coll]
  (every? true? (map = coll (rest coll))))

(defn remove-first [x coll]
  (let [[pre post] (split-with #(not= x %) coll)]
    (concat pre (rest post))))

;; NAIVE

(defn straight? ;; maybe faster to sum ranks and check = what straight should equal
  [cards]
  (loop [[x & xs] (sort (map :rank cards))]
    (if (empty? xs)
      true
      (when (= (inc x)
               (first xs))
        (recur (rest xs))))))

(defn flush?
  [cards]
  (constant-coll? (map :suit cards)))

(defn n-of-a-kind?
  [cards n]
  (let [ranks (map :rank cards)]
    (some constant-coll? (combo/combinations ranks n))))

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
  (let [perms (combo/permutations (map :rank cards))
        poss  (distinct (map #(map sort [(take k1 %) (take k2 (drop k1 %))])
                             perms))]
    (some #(every? true? (map constant-coll? %))
          poss)))

(defn full-house?
  [cards]
  (n-pairs? cards 3 2))

(defn two-pair?
  [cards]
  (n-pairs? cards 2 2))