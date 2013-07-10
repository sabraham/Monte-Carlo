(defproject montecarlo "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories {"local" "file:repo"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [local/spears "0.0.1"]
                 [core.async "0.1.0-SNAPSHOT"]]
  :jvm-opts ["-Xmx1g" "-server"]
)
