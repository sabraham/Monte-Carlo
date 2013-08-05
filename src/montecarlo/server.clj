(ns montecarlo.server
  (require [clojure.core.async :as async
            :refer [go chan buffer alt! sliding-buffer]]
           [gloss.core :as gloss]
           [cheshire.core :as json]
           [montecarlo.game :as mc.game]
           [montecarlo.player :as mc.player]
           [montecarlo.database :as mc.database])
  (use [lamina.core]
       [aleph.tcp]))

(defn error-msg
  [p status-code msg]
  (go (>! (mc.database/out-ch p) {:status status-code :msg msg})))

(defn ok-msg
  [p]
  (go (>! (mc.database/out-ch p) {:status 0 :msg "OK"})))

(defn pos-int?
  [x]
  (and (integer? x) (pos? x)))

(defn valid-blinds?
  [p blinds]
  (let [blind-vals (vals blinds)]
    (if (every? pos-int? blind-vals)
      true
      (do
        (error-msg p -12 "Blinds must be positive integers")
        false))))

(defn valid-num-players?
  [p n]
  (println (str "in this " n))
  (if (pos-int? n)
    true
    (do
      (error-msg p -5 "n (number of players) must be a postive integer.")
      false)))

(defn valid-room?
  [p name]
  (if (get @mc.database/ROOM-DATABASE name)
    (do
      (error-msg p -5 "room already exists.")
      false)
    (if (nil? name)
      (do
        (error-msg p -5 "empty room name.")
        false)
      true)))

(defn update-room
  [name p]
  (dosync
   (alter mc.database/ROOM-DATABASE #(update-in % [name :players] conj p))))

;; TODO race condition
(defn create-new-room
  [p {:keys [name n blinds] :or {blinds {:small 5 :big 10}}}]
  (println (str name " " n " " blinds))
  (when (and (valid-room? p name)
             (valid-num-players? p n)
             (valid-blinds? p blinds))
    (let [q (chan (buffer n))]
      (dosync
       (alter mc.database/ROOM-DATABASE assoc name {:players (list)
                                                    :n n
                                                    :q q}))
      (go (while true
            (alt! q ([p] (do
                           (update-room name p)
                           (mc.database/add-room (:id p) name)
                           (println (str "count: " (count (get-in @mc.database/ROOM-DATABASE [name :players])) " " n))
                           (when (= (count (get-in @mc.database/ROOM-DATABASE [name :players])) n)
                             (mc.game/game name
                                           (get-in @mc.database/ROOM-DATABASE [name :players])
                                           blinds)))))))
      (ok-msg p))))

(defn join-room
  [p {:keys [name]}]
  (if-let [q (get-in @mc.database/ROOM-DATABASE [name :q])]
    (do
      (go (>! q p))
      (ok-msg (:id p)))
    (error-msg (:id p) -2 (str "Room \"" name "\" does not exist yet."))))

(defn hand-query
  [p-id room]
  (if (-> @mc.database/PLAYER-DATABASE
          (get p-id)
          :rooms deref
          (get room))
    (go (>! (mc.database/out-ch p-id)
            {:hand @(mc.database/player-hand p-id
                                             room)}))
    (error-msg p-id -1 (str "Player is not in room " room))))

(defn whoami-query
  [p-id]
  (go (>! (mc.database/out-ch p-id) p-id)))

(defn handler
  [ch client-info]
  (let [p (mc.player/->Player (gensym)
                              (chan) (chan) (chan))
        p-id (:id p)]
    (println "new connection")
    (mc.database/add-player p)
    (receive-all ch #(try
                       (let [req (clojure.walk/keywordize-keys (json/parse-string %))]
                         (case (:type req)
                           "new_room" (create-new-room p-id req)
                           "join_room" (join-room p req)
                           "play" (go (>! (mc.database/listen-ch p-id (:name req)) (:amt req)))
                           "hand?" (hand-query p-id (:name req))
                           "whoami?" (whoami-query p-id)
                           (error-msg p -1 "bad \"type\" argument")))
                       (catch com.fasterxml.jackson.core.JsonParseException e
                         (error-msg p -17 "You sent me bad json!"))))
    (go
     (while true
       (alt!
        (mc.database/out-ch (:id p))
        ([update]
           (enqueue ch (json/generate-string update))))))))

(defn start-server
  []
  (start-tcp-server handler
                    {:port 10000, :frame (gloss/string :utf-8 :delimiters ["\r\n"])}))
