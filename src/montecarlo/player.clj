(ns montecarlo.player
  (require
   [clojure.core.async :as async
    :refer [>!! >! <!! go alt!]]
   [montecarlo.action :refer :all]
   [montecarlo.board :as mc.board]
   [montecarlo.helpers :as mc.helpers]
   [montecarlo.gameplay :as mc.gameplay]
   [montecarlo.database :as mc.database]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Player
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Player
    [id
;;     listen-ch ;; Action :: real player -> logical player
     card-ch ;; Card :: board -> logical player
     board-ch ;; Board :: board -> logical player
;;     out-ch ;; logical player ->  real player
     quit-ch])

;; Validation layer
(extend-type Player
  ActionP
  (fold [this board] (>!! (:action-ch board) FOLD))
  (call [this board] (>!! (:action-ch board) CALL))
  (raise [this board r] (>!! (:action-ch board)
                             (max 0
                                  (min r
                                       (- (deref (mc.database/player-stack (:id this)))
                                          (mc.helpers/board->needed-bet board (:id this))))))))

(defn player-action
  [player board]
  (when (and (= (:id player)
                (first (deref (:play-order board))))
             (not (mc.gameplay/game-end? board)))
    (do
      (let [action (<!! (mc.database/listen-ch (:id player) (:room board)))]
        (cond
         (is-fold? action) (fold player board)
         (is-call? action) (call player board)
         (is-raise? action) (raise player board action)
         :else (throw (Exception. "Action is not fold nor call nor raise!")))))))

(defn sync-card
  [p wrapped-card]
  (comment  (println (str "wrapped card: " wrapped-card "\n"
                          "unwrapped " (:card wrapped-card) " " (:room wrapped-card) "\n"
                          "player entry: " (get @mc.database/PLAYER-DATABASE (:id p))
                          "player hand: " (mc.database/player-hand (:id p) (:room wrapped-card)))))
  (dosync
   (alter (mc.database/player-hand (:id p) (:room wrapped-card))
          conj (:card wrapped-card)))
  )

(defn run
  [player]
  (go
   (while true
     (alt!
      (:card-ch player)  ([wrapped-card]
                            (sync-card player wrapped-card)
                            (>! (mc.database/out-ch (:id player)) wrapped-card))
      (:board-ch player) ([board]
                            (>! (mc.database/out-ch (:id player)) (mc.helpers/read-board board))
                            (player-action player board))
      (:quit-ch player)  ([s] (println "i don't know how to quit you."))))))
