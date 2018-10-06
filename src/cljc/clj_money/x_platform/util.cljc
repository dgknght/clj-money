(ns clj-money.x-platform.util
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clj-money.util :refer [pprint-and-return]]))

(defmulti ^:private entry->key-value-pairs
  (fn [[_ v] _]
    (cond
      (map? v) :map
      (coll? v) :collection
      :else :default)))

(defmethod ^:private entry->key-value-pairs :map
  [[k v] prefix-vec]
  (mapcat #(entry->key-value-pairs % (conj prefix-vec k)) v))

(defmethod ^:private entry->key-value-pairs :collection
  [[k v] prefix-vec]
  (map #(vector (concat prefix-vec [k ::index])
                %)
       v))

(defmethod ^:private entry->key-value-pairs :default
  [[k v] prefix-vec]
  [[(conj prefix-vec k) v]])

(defn- prepare-key
  [[primary & remaining]]
  (str (name primary)
       (->> remaining
            (map #(if (= ::index %)
                   "[]"
                   (str "[" (name %) "]")))
            (string/join ""))))

(defn- prepare-value
  [v]
  (if (keyword? v)
    (name v)
    v))

(defn map->query-string
  [m]
  (string/join "&" (->> m
                        (mapcat #(entry->key-value-pairs % []))
                        (map #(update-in % [0] prepare-key))
                        (map #(update-in % [1] prepare-value))
                        (map #(string/join "=" %)))))

(def ^:private key-pattern
  #"(?:(^[^\[\]]+)|(?:^\[([^\]]+)\])|(^\[\]))(.*)")

(defn- key-segments
  [k]
  (let [[f r] (->> (re-find key-pattern k)
                   rest
                   (filter identity))]
    (if r
      (cons f (key-segments r))
      f)))

(defn- parse-key
  "Takes a query string key like user[first-name] and
  returns a sequence containing the keys in order, appropriate
  for use with assoc-in
  (like [:user :first-name])"
  [k]
  (->> (key-segments k)
       (map keyword)
       (map #(if (= % (keyword "[]"))
               ::index
               %))))

(defn- assoc-in-x
  "Like assoc-in except that it creates a vector (instead of a map) for a
  non-existing container if the key is ::index"
  [m [k & ks] v]
  (let [target (or m (if (= k ::index)
                       []
                       {}))
        wk (if (= k ::index)
             (count target)
             k)]
    (if ks
      (assoc target wk (assoc-in-x (get m k) ks v))
      (assoc target wk v))))

(defn- collapse-collections
  [key-value-pairs]
  (reduce (fn [result [k v]]
            (assoc-in-x result (parse-key k) v))
          {}
          key-value-pairs))

(defn query-string->map
  [query-string]
  (collapse-collections (->> (string/split query-string #"&")
                             (map #(string/split % #"=")))))
