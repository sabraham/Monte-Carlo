(ns montecarlo.core
  (require [montecarlo.server :as mc.server])
  (:gen-class))

(defn -main
  []
  (mc.server/start-server))
