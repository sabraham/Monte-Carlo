(defproject montecarlo "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "MIT License"}
  :repositories {"local" "file:repo"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [core.async "0.1.0-SNAPSHOT"]
                 [org.clojure/math.combinatorics "0.0.4"]
                 [aleph "0.3.0-rc2"]
                 [cheshire "5.2.0"]]
  :main montecarlo.core
  :jvm-opts ["-Xmx1g" "-server"]
)
