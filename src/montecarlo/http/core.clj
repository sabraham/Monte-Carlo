(ns montecarlo.http.core
  (require [clojure.string :as s]
           [clojure.java.io :as io]))

(defn method->keyword
  [method]
  (keyword (s/lower-case method)))

(defn request-uri->uri
  [request-uri]
  (first (s/split request-uri #"\?")))

(defn request-uri->query-string
  [request-uri]
  (second (s/split request-uri #"\?")))

(defn version->scheme
  [version]
  (-> (s/split version #"/")
      first
      s/lower-case
      keyword))

(defn version->version
  [version]
  (second (s/split version #"/")))

(defn parse-request-line
  [request-line]
  (let [[method request-uri version] (s/split request-line #" ")]
    {:request-type (method->keyword method)
     :uri (request-uri->uri request-uri)
     :query-string (request-uri->query-string request-uri)
     :version (version->version version)
     :scheme (version->scheme version)}))

(defn parse-header
  [header]
  (let [[key-name val-name] (s/split header #":" 2)]
    {(keyword (s/lower-case key-name))
     (s/lower-case (s/trim val-name))}))

(defn parse-headers
  [headers]
  (reduce merge {}
          (map parse-header headers)))

(defn parse-server
  [server]
  (let [[protocol name port] (s/split server #":")]
    {:server-name (apply str (drop 2 name))
     :server-port (Integer/parseInt port)}))

(defn parse-request
  [request]
  (with-open [rdr (io/reader request)]
    (let [lines (line-seq rdr)
          [headers _ body] (partition-by empty? lines)
          request-line (parse-request-line (first headers))
          parsed-headers (parse-headers (rest headers))]
      (-> request-line
          (merge {:headers parsed-headers})
          (merge {:body (first body)})
          (merge {:content-type (get parsed-headers :content-type)})
          (merge (parse-server (get parsed-headers :server)))
          ))))