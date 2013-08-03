(ns montecarlo.server
  (require [clojure.core.async :as async
            :refer [go chan buffer alt! sliding-buffer]]
           [gloss.core :as gloss]
           [cheshire.core :as json]
           [montecarlo.game :as mc.game]
           [montecarlo.player :as mc.player])
  (use [lamina.core]
       [aleph.tcp]))

(def DATABASE (atom {}))

(defn error-msg
  [p status-code msg]
  (go (>! (:out-ch p) {:status status-code :msg msg})))

(defn ok-msg
  [p]
  (go (>! (:out-ch p) {:status 0 :msg "OK"})))

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
  (if (get @DATABASE name)
    (do
      (error-msg p -5 "room already exists.")
      false)
    (if (nil? name)
      (do
        (error-msg p -5 "empty room name.")
        false)
      true)))

;; TODO race condition
(defn create-new-room
  [p {:keys [name n blinds] :or {blinds {:small 5 :big 10}}}]
  (println (str name " " n " " blinds))
  (when (and (valid-room? p name)
             (valid-num-players? p n)
             (valid-blinds? p blinds))
    (let [q (chan (buffer n))]
      (swap! DATABASE assoc name {:players (list)
                                  :n n
                                  :q q})
      (go (while true
            (alt! q ([p] (do
                           (swap! DATABASE #(update-in % [name :players] conj p))
                           (println (str "count: " (count (get-in @DATABASE [name :players])) " " n))
                           (when (= (count (get-in @DATABASE [name :players])) n)
                             (mc.game/game (get-in @DATABASE [name :players])
                                   blinds)))))))
      (ok-msg p))))

(defn join-room
  [p {:keys [name]}]
  (if-let [q (get-in @DATABASE [name :q])]
    (do
      (go (>! q p))
      (ok-msg p))
    (error-msg -2 (str "Room \"" name "\" does not exist yet."))))

(defn handler
  [ch client-info]
  (let [p (mc.player/->Player (gensym) (ref 100) (atom [])
                    (chan (sliding-buffer 1))
                    (chan) (chan) (chan) (chan))]
    (println "new connection")
    (receive-all ch #(try
                       (let [req (clojure.walk/keywordize-keys (json/parse-string %))]
                         (case (:type req)
                           "new_room" (create-new-room p req)
                           "join_room" (join-room p req)
                           "play" (go (>! (:listen-ch p) (:amt req)))
                           (error-msg p -1 "bad \"type\" argument")))
                       (catch com.fasterxml.jackson.core.JsonParseException e
                         (error-msg p -17 "You sent me bad json!"))))
    (go
     (while true
       (alt!
        (:out-ch p)  ([update]
                        (enqueue ch (json/generate-string update))))))))

(defn start-server
  []
  (start-tcp-server handler
                    {:port 10000, :frame (gloss/string :utf-8 :delimiters ["\r\n"])}))
