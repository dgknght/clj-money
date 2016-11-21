(ns clj-money.url
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]))

(defn- extract-map
  [args]
  (if (map? (first args))
    [(first args) (rest args)]
    [{} args]))

(defmacro defhelper
  [helper-name comments arg-list & body]
  `(defn ~helper-name
     ~comments
     [& args#]
     (let [[~(first arg-list) ~(second arg-list)] (extract-map args#)]
       ~@body)))

(defhelper path
  "Appends segments to the path in the URL"
  [m segments]
  (update-in m [:segments] (fnil concat []) segments))

(defhelper host
  "Specifies the host for the URL"
  [m more]
  (assoc m :host (first more)))

(defhelper protocol
  "Specifies the protocol for the URL"
  [m more]
  (assoc m :protocol (first more)))

(defhelper query
  "Appends query string values for the URL"
  [m more]
  (update-in m [:query] (fnil merge {}) (first more)))

(defn- map->query-string
  [m]
  (->> m
       (map (fn [[k v]] (str (name k) "=" v)))
       (string/join "&")))

(defn format-url
  "Turns the url-hash into a final url string"
  [m]
  (let [parts (cond-> []
                (contains? m :protocol) (conj (:protocol m) "://")
                (contains? m :host) (conj (:host m) "/")
                (contains? m :segments) (conj (string/join "/" (:segments m)))
                (contains? m :query) (conj "?" (map->query-string (:query m))))]
    (string/join "" parts)))
