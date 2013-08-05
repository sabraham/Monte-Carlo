(ns montecarlo.game
  (require [montecarlo.gameplay :as mc.gameplay]
           [montecarlo.player :as mc.player]
           [montecarlo.board :as mc.board]
           [montecarlo.database :as mc.database]
           [clojure.core.async :as async
            :refer [<!! chan sliding-buffer]]))
(defn hand
  [board players]
  (mc.gameplay/play-blinds board)
  (doseq [p players] (mc.player/run p))
  (mc.board/run-board board)
  (mc.gameplay/deal-hand board)
  (mc.board/update-players board))

(defn game
  [name players blinds]
  (let [action-ch (chan (sliding-buffer (count players)))]
    (loop [players players]
      (let [board (mc.board/init-board name players blinds action-ch)]
        (hand board players)
        (let [signal (<!! (:quit-ch board))]
          (doseq [p-id (map :id (deref (:original-players board)))]
            (mc.database/reset-hand p-id (:room board))))
        (recur players)))))