(ns montecarlo.database
  (require [clojure.core.async :as async
            :refer [chan sliding-buffer]]))

(def ROOM-DATABASE (ref {}))
(def PLAYER-DATABASE (ref {}))  ;; {player-id {:stack blah, :hand}}

(defn player-stack
  [p]
  (-> @PLAYER-DATABASE
      (get p)
      :stack))

(defn player-hand
  [p room]
  (-> @PLAYER-DATABASE
      (get p)
      (:rooms)
      deref
      (get room)
      :hand))

(defn reset-hand
  [p room]
  (dosync
   (alter (player-hand p room) (fn [x] (list)))))

(defn add-player
  [p]
  (dosync
   (alter PLAYER-DATABASE assoc (:id p) {:stack (ref 100)
                                         :out-ch (chan)
                                         :rooms (ref {})})))

(defn add-room
  [p-id room]
  (dosync
   (alter (:rooms (get @PLAYER-DATABASE p-id))
          assoc
          room
          {:hand (ref (list))
           :listen-ch (chan (sliding-buffer 1))})))

(defn in-room?
  [p-id room]
  (let [player-data (get @PLAYER-DATABASE p-id)
        rooms @(:rooms player-data)]
    (get rooms room)))

(defn out-ch
  [p-id]
  (-> @PLAYER-DATABASE
      (get p-id)
      :out-ch))

(defn listen-ch
  [p-id room]
  (-> @PLAYER-DATABASE
      (get p-id)
      :rooms
      deref
      (get room)
      :listen-ch))