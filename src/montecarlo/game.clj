(ns montecarlo.game
  (require [montecarlo.gameplay :as mc.gameplay]
           [montecarlo.player :as mc.player]
           [montecarlo.board :as mc.board]
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
  [players blinds]
  (let [action-ch (chan (sliding-buffer (count players)))]
    (loop [players players]
      (let [board (mc.board/init-board players blinds action-ch)]
        (hand board players)
        (<!! (:quit-ch board))
        (recur (concat (rest players) (list (first players))))))))