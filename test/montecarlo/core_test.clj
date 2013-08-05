(ns montecarlo.core-test
  (require [clojure.core.async :as async
            :refer [<! >! <!! >!! timeout chan alt! alts!! go close! sliding-buffer]])
  (:use clojure.test
        montecarlo.core
        montecarlo.bet
        montecarlo.player
        montecarlo.board
        montecarlo.gameplay))

(defn player-gen
  []
  (let [action-chan (chan (sliding-buffer 3))]
    (map #(->Player % (ref (* % 100)) (ref [])
                    (chan (sliding-buffer 1))
                    (chan)
                    (chan) (chan) (chan))
         (rest (range)))))

  (deftest test-play-blinds
    (let [ps (player-gen)
          [p1 p2 p3 :as players] (take 3 ps)

          _ (doseq [p players] (run p))

          b1 (->Bet 5 #{1 2})
          b2 (->Bet 5 #{2})
          a  (:action-ch p1)]
      (testing "two players"
        (let [board (init-board [p1 p2] {:small 5 :big 10} a)]
          (run-board board)
         (play-blinds board)
          (is (= [p1 p2]
                 (get board :players)))
          (is (= [b1 b2]
                 (deref (get board :bets))))
          (is (= 1
                 (first (deref (get board :play-order)))))
          (is (= [1 2]
                 (deref (get board :remaining-players))))))
        (testing "three players"
          (let [board (init-board [p1 p2 p3] {:small 5 :big 10} a)]
            (play-blinds board)
            (is (= [p1 p2 p3]
                   (get board :players)))
            (is (= [b1 b2]
                   (deref (get board :bets))))
            (is (= 3
                   (first (deref (get board :play-order)))))
            (is (= [1 2 3]
                   (deref (get board :remaining-players))))))))

(comment
  (deftest test-merge-bets
    (let [ps (player-gen)
          p1 (first ps)
          p2 (second ps)
          p3 (nth ps 2)
          b1 (->Bet 5 #{1 2 3})
          b2 (->Bet 5 #{1 2 3})
          b12 (->Bet 10 #{1 2 3})
          b3 (->Bet 10 #{1 2})
          b4 (->Bet 10 #{1 2})
          b34 (->Bet 20 #{1 2})]
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
          b1 (->Bet 10 #{1})
          b1a (->Bet 10 #{1 2})
          b2a (->Bet 10 #{2})
          b1b (->Bet 10 #{1 2 3})
          b2b (->Bet 5 #{2 3})
          b3b (->Bet 5 #{2})]
      (testing "initial bet"
        (is (= [b1]
               (update-bets [] (->Bet 10 #{1})))))
      (testing "additional bet"
        (is (= [b1a b2a]
               (update-bets [b1] (->Bet 20 #{2})))))
      (testing "split earlier bet"
        (is (= [b1b b2b b3b]
               (update-bets [b1a b2a] (->Bet 15 #{3})))))))

  (deftest test-board->total-bet
    (testing "sum bets"
      (let [ps (player-gen)
            p1 (first ps)
            p2 (second ps)
            b1 (->Bet 5 #{1 2})
            b2 (->Bet 20.2 #{2})
            a  (:action-ch p1)]
        (is (= 25.2
               (board->total-bet (-> (init-board [p1 p2]
                                                 {:small 5 :big 10}
                                                 a)
                                     (assoc :bets [b1 b2]))))))))

  (deftest test-board->needed-bet
    (testing "sum bets"
      (let [ps (player-gen)
            p1 (first ps)
            p2 (second ps)
            b1 (->Bet 5 #{1 2})
            b2 (->Bet 20.2 #{2})
            a  (:action-ch p1)
            board (init-board [p1 p2] {:small 5 :big 10} a)]
        (dosync
         (alter (:bets board) conj b1 b2))
        (is (= 20.2
               (board->needed-bet board
                                  1))))))


  (comment
    (deftest test-raise
      (testing "player 3 raises after p1 and p2 put in blinds"
        (let [ps (player-gen)
              [p1 p2 p3 :as players] (take 3 ps)
              _ (doseq [p players] (run p))
              board (-> (init-board [p1 p2 p3] {:small 5 :big 10})
                        play-blinds
                        (raise 7))
              b1 (->Bet 5 #{1 2 3})
              b2 (->Bet 5 #{2 3})
              b3 (->Bet 7 #{3})]
          (is (= [p1 p2 p3]
                 (get board :players)))
          (is (= [b1 b2 b3]
                 (get board :bets)))
          (is (= 1
                 (first (get board :play-order))))
          (is (= [1 2]
                 (get board :remaining-players)))))))

  (comment
    (deftest test-call
      (testing "player 3 calls after p1 and p2 put in blinds"
        (let [ps (player-gen)
              [p1 p2 p3 :as players] (take 3 ps)
              _ (doseq [p players] (run p))
              board (-> (init-board [p1 p2 p3] {:small 5 :big 10})
                        play-blinds
                        call)
              b1 (->Bet 5 #{1 2 3})
              b2 (->Bet 5 #{2 3})]
          (is (= [p1 p2 p3]
                 (get board :players)))
          (is (= [b1 b2]
                 (get board :bets)))
          (is (= 1
                 (first (get board :play-order))))
          (is (= [1 2]
                 (get board :remaining-players)))))))

  (comment
    (deftest test-preflop-stage
      (testing "first stage of play"
        (let [ps (player-gen)
              [p1 p2 p3 :as players] (take 3 ps)
              _ (doseq [p players] (run p))
              [l1 l2 l3] (map :listen-ch players)

              big-blind 10
              board (init-board [p1 p2 p3] {:small 5 :big big-blind})
              b1 (->Bet 5 #{1 2 3})
              b2 (->Bet 5 #{1 2 3})
              b3 (->Bet 10 #{1 3})
              b4 (->Bet 10 #{1 3})
              b12 (->Bet 10 #{1 2 3})
              b34 (->Bet 20 #{1 3})

              a (-> board :players first :action-ch)
              end-stage-board (future (preflop-stage board a))
              ]
          (is (= [p1 p2 p3] (get board :players)))
          (is (= [b1 b2]    (get board :bets)))
          (is (= 1          (first (get board :play-order))))
          (is (= [1 2]      (get board :remaining-players)))))
      (comment
        (testing "first stage of play"
          (let [ps (player-gen)
                p1 (first ps)
                p2 (second ps)
                p3 (nth ps 2)

                _ (run p1)
                _ (run p2)
                _ (run p3)

                [l1 l2 l3] (map :listen-ch [p1 p2 p3])

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
            (>!! l3 0) ;; player 3 calls
            (Thread/sleep 1000)
            (>!! l1 big-blind) ;; player 1 raises by big blind
            (>!! l2 -1) ;; player 2 folds
            (>!! l3 big-blind) ;; player 3 raises
            (Thread/sleep 1000)
            (>!! l1 0) ;; player 1 calls
            (let [realized @end-stage-board]
              (is (= [p1 p3] (:players realized)))
              (is (= [1 3] (:remaining-players realized)))
              (is (= [] (:bets realized)))
              (is (= [b12 b34] (:pots realized)))
              (is (= 1 (first (:play-order realized)))))
            (close! a))))
      (comment
        (testing "first stage of play and listening channel"
          (let [ps (player-gen)
                p1 (first ps)
                p2 (second ps)
                p3 (nth ps 2)

                _ (run p1)
                _ (run p2)
                _ (run p3)

                b1 (->Bet 5 #{1 2})
                b2 (->Bet 5 #{2})

                big-blind 10
                board0 (init-board [p1 p2 p3] {:small 5 :big big-blind})

                [l1 l2 l3] (map :listen-ch [p1 p2 p3])

                a (-> board0 :players first :action-ch)

                end-stage-board (future (preflop-stage board0 a))]
            (is (not (nil? a)))
            (is (= [p1 p2 p3] (:players board0)))
            (is (= [1 2 3] (:remaining-players board0)))
            (is (= (:bets board0)))
            (is (= 1 (first (:play-order board0))))

            (>!! l3 -1) ;; player 3 folds
            (>!! l1 -1) ;; player 1 folds

            (let [realized @end-stage-board]
              (is (= [2] (map :id (:players realized))))
              (is (= [2] (:remaining-players realized)))
              (is (= [] (:bets realized)))
              (is (= [b1 b2] (:pots realized)))
              (is (= 2 (first (:play-order realized)))))
            (close! a)
            (close! l1)
            (close! l2)
            (close! l3))))

      (comment
        (testing "first stage of play and listening channel"
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

                [l1 l2 l3] (map :listen-ch [p1 p2 p3])

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
            (>!! l3 0) ;; player 3 calls
            (Thread/sleep 1000) ;; HACK -- ensures entries in test are not
            ;; overwritten -- currently by the time l1 is taken from, we have
            ;; overwritten its value with the next time we put into l1
            (>!! l1 big-blind) ;; player 1 raises by big blind
            (>!! l2 -1) ;; player 2 folds
            (>!! l3 big-blind) ;; player 3 raises
            (Thread/sleep 1000)
            (>!! l1 0) ;; player 1 calls
            (let [realized @end-stage-board]
              (is (= [p1 p3] (:players realized)))
              (is (= [1 3] (:remaining-players realized)))
              (is (= [] (:bets realized)))
              (is (= [b12 b34] (:pots realized)))
              (is (= 1 (first (:play-order realized)))))
            (close! a))))))

  (comment
    (deftest test-game
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

              [l1 l2 l3] (map :listen-ch [p1 p2 p3])

              end-board (game players blinds)]
          (>!! l3 0) ;; player 3 calls
          (Thread/sleep 1000)
          (>!! l1 10)  ;; player 1 raises
          (>!! l2 -1) ;; player 2 folds
          (>!! l3 -1) ;; player 3 folds
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

              [l1 l2 l3] (map :listen-ch [p1 p2 p3])

              end-board (game players blinds)]
          (>!! l3 0) ;; player 3 calls
          (Thread/sleep 1000)
          (>!! l1 0)  ;; player 1 calls
          (>!! l2 -1) ;; player 2 folds
          ;; flop
          (Thread/sleep 2000)
          (>!! l1 10) ;; player 1 raises
          (>!! l3 -1) ;; player 3 folds
          (let [realized @end-board]
            (is (= [1] (:remaining-players realized)))
            (is (= [1] (map :id (:players realized))))
            (is (= [] (:bets realized)))
            (is (= [b12 b3] (:pots realized))))))

      (testing "to river"
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

              [l1 l2 l3] (map :listen-ch players)

              end-board (game players blinds)]
          (>!! l3 0) ;; player 3 calls
          (Thread/sleep 1000)
          (>!! l1 0)  ;; player 1 calls
          (>!! l2 -1) ;; player 2 folds
          ;; flop
          (Thread/sleep 1000)
          (>!! l1 10) ;; player 1 raises
          (>!! l3 0) ;; player 3 calls
          (Thread/sleep 1000)
          ;; turn
          (>!! l1 0) ;; player 1 calls
          (>!! l3 17);; player 3 raises
          (Thread/sleep 1000)
          (>!! l1 -1) ;; player 1 folds
          (let [realized @end-board]
            (is (= [3] (:remaining-players realized)))
            (is (= [p3] (:players realized)))
            (is (= [] (:bets realized)))
            (is (= [b12 b3 b4] (:pots realized))))))

      (testing "all the way to showdown"
        (let [ps (player-gen)
              [p1 p2 p3 :as players] (take 3 ps)
              _ (doseq [p players] (run p))
              blinds {:small 5 :big 10}
              a (-> players
                    first
                    :action-ch)
              [l1 l2 l3] (map :listen-ch players)
              b1 (->Bet 5 #{1 2 3})
              b2 (->Bet 5 #{1 2 3})
              b12 (->Bet 10 #{1 2 3})
              b3 (->Bet 10 #{1 3})
              b4 (->Bet 17 #{1 3})
              b5 (->Bet 3 #{1 3})

              end-board (game players blinds)]
          (>!! l3 0) ;; player 3 calls
          (Thread/sleep 1000)
          (>!! l1 0)  ;; player 1 calls
          (>!! l2 -1) ;; player 2 folds
          ;; flop
          (Thread/sleep 1000)
          (>!! l1 10) ;; player 1 raises
          (>!! l3 0) ;; player 3 calls
          ;; turn
          (Thread/sleep 1000)
          (>!! l1 0) ;; player 1 calls
          (>!! l3 17);; player 3 raises
          (Thread/sleep 1000)
          (>!! l1 0) ;; player 1 calls
          ;; river
          (Thread/sleep 1000)
          (>!! l1 3) ;; player 1 raises
          (>!! l3 0) ;; player 3 calls
          (let [realized @end-board]
            (is (= [1 3] (:remaining-players realized)))
            (is (= [p1 p3] (:players realized)))
            (is (= [] (:bets realized)))
            (is (= [b12 b3 b4 b5] (:pots realized))))))))

  (comment
    (def ps (player-gen))
    (def p1 (first ps))
    (def p2 (second ps))
    (def p3 (nth ps 2))
    (def l1 (:listen-ch p1))
    (def l2 (:listen-ch p2))
    (def l3 (:listen-ch p3))
    (future (montecarlo.game/game [p1 p2 p3] {:small 5 :big 17}))
   ;;  (def board (init-board [p1 p2 p3] {:small 5 :big 10} (:action-ch p1)))
   ;; (play-blinds board)
   ;; (run-board board)
   ;; (doseq [p [p1 p2 p3]] (run p))
   ;; (deal-hand board)
   ;;  (update-players board)
    (>!! l3 0)
    (>!! l1 10)
    (>!! l2 -1)
    (>!! l3 0)
    ;;flop
    (>!! l1 10)
    (>!! l3 0)
    ;;turn
    (>!! l1 10)
    (>!! l3 -1)
    ;;river
    (>!! l1 10)
    (>!! l3 0)

    (def board (init-board [p1 p2] {:small 5 :big 10} (chan (sliding-buffer 1))))
    (run-board board)
    (>!! (:action-ch board) 1)

    (def g (game [p1 p2 p3] {:small 5 :big 10}))


    )
)