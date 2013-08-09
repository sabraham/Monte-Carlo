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

(deftest evaluator-test
  (testing "straight flush"
    (is (= [8 [9 8 7 6 5] []]
           (evaluator (shuffle (map #(card/->Card :h %) [5 6 7 8 9]))))))
  (testing "four of a kind"
    (is (= [7 [1 1 1 1] [3]]
           (evaluator [(card/->Card :h 1)
                       (card/->Card :d 1)
                       (card/->Card :s 1)
                       (card/->Card :c 1)
                       (card/->Card :h 3)]))))
  (testing "full-house"
    (is (= [6 [8 8 8 9 9] []]
           (evaluator [(card/->Card :h 9)
                       (card/->Card :d 8)
                       (card/->Card :s 9)
                       (card/->Card :c 8)
                       (card/->Card :h 8)]))))
  (testing "a flush, but not a straight"
    (is (= [5 [9 7 6 5 4] []]
           (evaluator (shuffle (map #(card/->Card :h %) [4 5 6 7 9]))))))
  (testing "straight"
    (is (= [4 [5 4 3 2 1] []]
           (evaluator [(card/->Card :h 1)
                       (card/->Card :d 2)
                       (card/->Card :s 3)
                       (card/->Card :c 5)
                       (card/->Card :h 4)]))))
  (testing "three of a kind"
    (is (= [3 [5 5 5] [8 2]]
           (evaluator [(card/->Card :h 5)
                       (card/->Card :d 2)
                       (card/->Card :s 8)
                       (card/->Card :c 5)
                       (card/->Card :s 5)])))
    (is (= [3 [5 5 5] [3 2]]
           (evaluator [(card/->Card :h 5)
                       (card/->Card :d 2)
                       (card/->Card :s 3)
                       (card/->Card :c 5)
                       (card/->Card :h 5)]))))
  (testing "two pair"
    (is (= [2 [5 5 2 2] [8]]
           (evaluator [(card/->Card :h 5)
                       (card/->Card :d 2)
                       (card/->Card :s 8)
                       (card/->Card :c 2)
                       (card/->Card :h 5)])))
    (is (= [2 [7 7 5 5] [2]]
           (evaluator [(card/->Card :h 5)
                       (card/->Card :d 2)
                       (card/->Card :s 7)
                       (card/->Card :c 7)
                       (card/->Card :h 5)]))))
  (testing "two pair"
    (is (= [2 [5 5 2 2] [8]]
           (evaluator [(card/->Card :h 5)
                       (card/->Card :d 2)
                       (card/->Card :s 8)
                       (card/->Card :c 2)
                       (card/->Card :h 5)])))
    (is (= [2 [7 7 5 5] [2]]
           (evaluator [(card/->Card :h 5)
                       (card/->Card :d 2)
                       (card/->Card :s 7)
                       (card/->Card :c 7)
                       (card/->Card :h 5)]))))
  (testing "one pair"
    (is (= [1 [5 5] [8 7 6]]
           (evaluator [(card/->Card :h 5)
                       (card/->Card :d 7)
                       (card/->Card :s 8)
                       (card/->Card :c 6)
                       (card/->Card :h 5)]))))
  (testing "high card"
    (is (= [0 [8 7 6 4 2] []]
           (evaluator [(card/->Card :h 2)
                       (card/->Card :d 7)
                       (card/->Card :s 8)
                       (card/->Card :c 6)
                       (card/->Card :h 4)])))))
