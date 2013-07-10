(ns montecarlo.core-test
  (require [clojure.core.async :as async
            :refer [<! >! <!! >!! timeout chan alt! alts!! go close!]])
  (:use clojure.test
        montecarlo.core))

(defn player-gen
  []
  (let [action-chan (chan)]
    (map #(->Player % (* % 100) (atom []) action-chan (chan) (chan) (chan))
         (rest (range)))))

(deftest test-merge-bets
  (let [ps (player-gen)
        p1 (first ps)
        p2 (second ps)
        p3 (nth ps 2)
        b1 (->Bet 5 #{p1 p2 p3})
        b2 (->Bet 5 #{p1 p2 p3})
        b12 (->Bet 10 #{p1 p2 p3})
        b3 (->Bet 10 #{p1 p2})
        b4 (->Bet 10 #{p1 p2})
        b34 (->Bet 20 #{p1 p2})]
    (testing "merge first two"
      (is (= [b12 b3]
             (merge-bets (list b1 b2 b3)))))
    (testing "merge first two"
      (is (= [b12 b34]
             (merge-bets (list b1 b2 b3 b4)))))))

(deftest test-update-bets
  (let [ps (player-gen)
        p1 (first ps)
        p2 (second ps)
        p3 (nth ps 2)
        b1 (->Bet 10 #{p1})
        b1a (->Bet 10 #{p1 p2})
        b2a (->Bet 10 #{p2})
        b1b (->Bet 10 #{p1 p2 p3})
        b2b (->Bet 5 #{p2 p3})
        b3b (->Bet 5 #{p2})]
    (testing "initial bet"
      (is (= [b1]
             (update-bets [] (->Bet 10 #{p1})))))
    (testing "additional bet"
      (is (= [b1a b2a]
             (update-bets [b1] (->Bet 20 #{p2})))))
    (testing "split earlier bet"
      (is (= [b1b b2b b3b]
             (update-bets [b1a b2a] (->Bet 15 #{p3})))))))

(deftest test-board->total-bet
  (testing "sum bets"
    (let [ps (player-gen)
          p1 (first ps)
          p2 (second ps)
          b1 (->Bet 5 #{p1 p2})
          b2 (->Bet 20.2 #{p2})]
      (is (= 25.2
             (board->total-bet (-> (init-board [p1 p2]
                                               {:small 5 :big 10})
                                   (assoc :bets [b1 b2]))))))))

(deftest test-play-blinds
  (let [ps (player-gen)
        p1 (first ps)
        p2 (second ps)
        p3 (nth ps 2)
        b1 (->Bet 5 #{1 2})
        b2 (->Bet 5 #{2})]
    (testing "two players"
      (let [board (-> (init-board [p1 p2] {:small 5 :big 10})
                      play-blinds)]
        (is (= [p1 p2]
               (get board :players)))
        (is (= [b1 b2]
               (get board :bets)))
        (is (= 1
               (first (get board :play-order))))
        (is (= [1 2]
               (get board :remaining-players)))))
    (testing "three players"
      (let [board (-> (init-board [p1 p2 p3] {:small 5 :big 10})
                      play-blinds)]
        (is (= [p1 p2 p3]
               (get board :players)))
        (is (= [b1 b2]
               (get board :bets)))
        (is (= 3
               (first (get board :play-order))))
        (is (= [1 2 3]
               (get board :remaining-players)))))))

(deftest test-fold
  (testing "player 3 folds after p1 and p2 put in blinds"
    (let [ps (player-gen)
          p1 (first ps)
          p2 (second ps)
          p3 (nth ps 2)
          b1 (->Bet 5 #{1 2})
          b2 (->Bet 5 #{2})
          board (-> (init-board [p1 p2 p3] {:small 5 :big 10})
                    play-blinds
                    fold)]
      (is (= [p1 p2]
             (get board :players)))
      (is (= [b1 b2]
             (get board :bets)))
      (is (= 1
             (first (get board :play-order))))
      (is (= [1 2]
             (get board :remaining-players))))))

(deftest test-raise
  (testing "player 3 raises after p1 and p2 put in blinds"
    (let [ps (player-gen)
          p1 (first ps)
          p2 (second ps)
          p3 (nth ps 2)
          b1 (->Bet 5 #{1 2 3})
          b2 (->Bet 5 #{2 3})
          b3 (->Bet 7 #{3})
          board (-> (init-board [p1 p2 p3] {:small 5 :big 10})
                    play-blinds
                    (raise 7))]
      (is (= [p1 p2 p3]
             (get board :players)))
      (is (= [b1 b2 b3]
             (get board :bets)))
      (is (= 1
             (first (get board :play-order))))
      (is (= [1 2]
             (get board :remaining-players))))))

(deftest test-call
  (testing "player 3 calls after p1 and p2 put in blinds"
    (let [ps (player-gen)
          p1 (first ps)
          p2 (second ps)
          p3 (nth ps 2)
          b1 (->Bet 5 #{1 2 3})
          b2 (->Bet 5 #{2 3})
          board (-> (init-board [p1 p2 p3] {:small 5 :big 10})
                    play-blinds
                    call)]
      (is (= [p1 p2 p3]
             (get board :players)))
      (is (= [b1 b2]
             (get board :bets)))
      (is (= 1
             (first (get board :play-order))))
      (is (= [1 2]
             (get board :remaining-players))))))

(deftest test-preflop-stage
  (testing "first stage of play"
    (let [ps (player-gen)
          p1 (first ps)
          p2 (second ps)
          p3 (nth ps 2)

          _ (run p1)
          _ (run p2)
          _ (run p3)

          b1 (->Bet 5 #{1 2 3})
          b2 (->Bet 5 #{1 2 3})
          b3 (->Bet 10 #{1 3})
          b4 (->Bet 10 #{1 3})
          b12 (->Bet 10 #{1 2 3})
          b34 (->Bet 20 #{1 3})

          big-blind 10
          board0 (init-board [p1 p2 p3] {:small 5 :big big-blind})
          a (-> board0
                :players
                first
                :action-ch)
          end-stage-board (future (preflop-stage board0 a))]
      (is (not (nil? a)))
      (is (= [p1 p2 p3] (:players board0)))
      (is (= [1 2 3] (:remaining-players board0)))
      (is (= (:bets board0)))
      (is (= 1 (first (:play-order board0))))
      (>!! a 0) ;; player 3 calls
      (>!! a big-blind) ;; player 1 raises by big blind
      (>!! a -1) ;; player 2 folds
      (>!! a big-blind) ;; player 3 raises
      (>!! a 0) ;; player 1 calls
      (let [realized @end-stage-board]
        (is (= [p1 p3] (:players realized)))
        (is (= [1 3] (:remaining-players realized)))
        (is (= [] (:bets realized)))
        (is (= [b12 b34] (:pots realized)))
        (is (= 1 (first (:play-order realized)))))
      (close! a))))

(deftest test-game
  (testing "all the way to showdown"
    (let [ps (player-gen)
          [p1 p2 p3 :as players] (take 3 ps)
          _ (doseq [p players] (run p))
          blinds {:small 5 :big 10}
          a (-> players
                first
                :action-ch)
          b1 (->Bet 5 #{1 2 3})
          b2 (->Bet 5 #{1 2 3})
          b12 (->Bet 10 #{1 2 3})
          b3 (->Bet 10 #{1 3})
          b4 (->Bet 17 #{1 3})
          b5 (->Bet 3 #{1 3})

          end-board (game players blinds)]
      (>!! a 0) ;; player 3 calls
      (>!! a 0)  ;; player 1 calls
      (>!! a -1) ;; player 2 folds
      ;; flop
      (>!! a 10) ;; player 1 raises
      (>!! a 0) ;; player 3 calls
      ;; turn
      (>!! a 0) ;; player 1 calls
      (>!! a 17);; player 3 raises
      (>!! a 0) ;; player 1 calls
      ;; river
      (>!! a 3) ;; player 1 raises
      (>!! a 0) ;; player 3 calls
      (let [realized @end-board]
        (is (= [1 3] (:remaining-players realized)))
        (is (= [p1 p3] (:players realized)))
        (is (= [] (:bets realized)))
        (is (= [b12 b3 b4 b5] (:pots realized))))))
  (testing "no one left for turn"
    (let [ps (player-gen)
          [p1 p2 p3 :as players] (take 3 ps)
          _ (doseq [p players] (run p))
          blinds {:small 5 :big 10}
          a (-> players
                first
                :action-ch)
          b1 (->Bet 5 #{1 2 3})
          b2 (->Bet 5 #{1 2 3})
          b12 (->Bet 10 #{1 2 3})
          b3 (->Bet 10 #{1})

          end-board (game players blinds)]
      (>!! a 0) ;; player 3 calls
      (>!! a 10)  ;; player 1 raises
      (>!! a -1) ;; player 2 folds
      (>!! a -1) ;; player 3 folds
      (let [realized @end-board]
        (is (= [1] (:remaining-players realized)))
        (is (= [p1] (:players realized)))
        (is (= [] (:bets realized)))
        (is (= [b12 b3] (:pots realized))))))
  (testing "no one left after flop"
    (let [ps (player-gen)
          [p1 p2 p3 :as players] (take 3 ps)
          _ (doseq [p players] (run p))
          blinds {:small 5 :big 10}
          a (-> players
                first
                :action-ch)
          b1 (->Bet 5 #{1 2 3})
          b2 (->Bet 5 #{1 2 3})
          b12 (->Bet 10 #{1 2 3})
          b3 (->Bet 10 #{1})

          end-board (game players blinds)]
      (>!! a 0) ;; player 3 calls
      (>!! a 0)  ;; player 1 calls
      (>!! a -1) ;; player 2 folds
      ;; flop
      (>!! a 10) ;; player 1 raises
      (>!! a -1) ;; player 3 folds
      (let [realized @end-board]
        (is (= [1] (:remaining-players realized)))
        (is (= [p1] (:players realized)))
        (is (= [] (:bets realized)))
        (is (= [b12 b3] (:pots realized))))))
  (testing "all the way to showdown"
    (let [ps (player-gen)
          [p1 p2 p3 :as players] (take 3 ps)
          _ (doseq [p players] (run p))
          blinds {:small 5 :big 10}
          a (-> players
                first
                :action-ch)
          b1 (->Bet 5 #{1 2 3})
          b2 (->Bet 5 #{1 2 3})
          b12 (->Bet 10 #{1 2 3})
          b3 (->Bet 10 #{1 3})
          b4 (->Bet 17 #{3})

          end-board (game players blinds)]
      (>!! a 0) ;; player 3 calls
      (>!! a 0)  ;; player 1 calls
      (>!! a -1) ;; player 2 folds
      ;; flop
      (>!! a 10) ;; player 1 raises
      (>!! a 0) ;; player 3 calls
      ;; turn
      (>!! a 0) ;; player 1 calls
      (>!! a 17);; player 3 raises
      (>!! a -1) ;; player 1 folds
      (let [realized @end-board]
        (is (= [3] (:remaining-players realized)))
        (is (= [p3] (:players realized)))
        (is (= [] (:bets realized)))
        (is (= [b12 b3 b4] (:pots realized)))))))
