(ns montecarlo.hand-evaluator-test
  (use [montecarlo.hand-evaluator]
       [clojure.test])
  (require [montecarlo.card :as card]))

(deftest constant-coll?-test
  (testing "constant coll?"
    (is (constant-coll? [1 1 1]))
    (is (not (constant-coll? [1 1 2])))
    (is (not (constant-coll? [1 2 1])))
    (is (not (constant-coll? [2 1 1])))))

(deftest straight?-test
  (testing "straight?"
    (let [h1 (shuffle (take 5 card/COMPLETE-DECK))
          h2 (take 5 (take-nth 2 card/COMPLETE-DECK))]
      (is (straight? h1))
      (is (not (straight? h2))))))

(deftest n-of-a-kind?-test
  (testing "n-of-a-kind"
    (let [twos (filter #(= 2 (:rank %)) card/COMPLETE-DECK)
          threes (filter #(= 3 (:rank %)) card/COMPLETE-DECK)]
      (is (n-of-a-kind? twos 4))
      (is (n-of-a-kind? twos 3))
      (is (n-of-a-kind? twos 2))
      (is (not (n-of-a-kind? (concat (take 2 twos) (take 2 threes)) 4)))
      (is (not (n-of-a-kind? (concat (take 2 twos) (take 2 threes)) 3)))
      (is (n-of-a-kind? (shuffle (concat (take 2 twos) (take 2 threes))) 2)))))

(deftest full-house?-test
  (testing "full-house"
    (let [twos (filter #(= 2 (:rank %)) card/COMPLETE-DECK)
          threes (filter #(= 3 (:rank %)) card/COMPLETE-DECK)
          fours  (filter #(= 4 (:rank %)) card/COMPLETE-DECK)]
      (is (full-house? twos))
      (is (not (full-house? (concat twos (take 1 threes)))))
      (is (full-house? (concat (take 3 twos) (take 2 threes))))
      (is (not (full-house? (concat (take 2 twos)
                                    (take 1 threes) (take 1 fours)))))
      (is (not (full-house? (concat (take 2 twos)
                                    (take 2 threes) (take 1 fours))))))))

(deftest two-pair?-test
  (testing "full-house"
    (let [twos (filter #(= 2 (:rank %)) card/COMPLETE-DECK)
          threes (filter #(= 3 (:rank %)) card/COMPLETE-DECK)
          fours  (filter #(= 4 (:rank %)) card/COMPLETE-DECK)]
      (is (two-pair? twos))
      (is (two-pair? (concat twos (take 1 threes))))
      (is (two-pair? (concat (take 3 twos) (take 2 threes))))
      (is (not (two-pair? (concat (take 2 twos)
                                    (take 1 threes) (take 1 fours)))))
      (is (two-pair? (concat (take 2 twos)
                                    (take 2 threes) (take 1 fours)))))))
