(ns montecarlo.core-test
  (:use clojure.test
        montecarlo.core))

(deftest test-update-bets
  (let [p1 (->Player 1 100)
        p2 (->Player 2 200)
        p3 (->Player 3 150)
        b1 (->Bet 10 #{p1})
        b1a (->Bet 10 #{p1 p2})
        b2a (->Bet 10 #{p2})
        b1b (->Bet 10 #{p1 p2 p3})
        b2b (->Bet 5 #{p2 p3})
        b3b (->Bet 5 #{p2})]
    (testing "initial bet"
      (is (= [b1]
             (update-bets [] (->Bet 10 [p1])))))
    (testing "additional bet"
      (is (= [b1a b2a]
             (update-bets [b1] (->Bet 20 [p2])))))
    (testing "split earlier bet"
      (is (= [b1b b2b b3b]
             (update-bets [b1a b2a] (->Bet 15 [p3])))))))

