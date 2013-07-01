(ns montecarlo.http.core-test
  (:use clojure.test)
  (:require [clojure.java.io :as io]
            [montecarlo.http.core :as http]
            [clojure.string :as s]))

(deftest test-parse-request-line
  (let [request-line (-> (slurp (io/resource "montecarlo/http/001.http"))
                       (s/split #"\r\n")
                       first)]
    (is (= {:request-type :put
            :uri "/stuff/here"
            :query-string "foo=bar"
            :scheme :http
            :version "1.0"}
           (http/parse-request-line request-line)))))

(deftest test-parse-request
  (let [request (io/resource "montecarlo/http/001.http")]
    (is (= {:request-type :put
            :server-port 5984
            :server-name "127.0.0.1"
            :uri "/stuff/here"
            :query-string "foo=bar"
            :scheme :http
            :version "1.0"
            :content-type "application/json"
            :headers {:content-type "application/json"
                      :content-length "14"
                      :server "http://127.0.0.1:5984"}
            :body "{\"nom\": \"nom\"}"}
           (http/parse-request request)))))
