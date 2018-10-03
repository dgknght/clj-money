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
  (map #(vector (conj prefix-vec
                      (-> k name (str "[]") keyword))
                %)
       v))

(defmethod ^:private entry->key-value-pairs :default
  [[k v] prefix-vec]
  [[(conj prefix-vec k) v]])

(defn- prepare-key
  [key-or-vec]
  (let [s (map name key-or-vec)]
    (str (first s)
         (->> (rest s)
              (map #(str "[" % "]"))
              (string/join "")))))

(defn map->query-string
  [m]
  (string/join "&" (->> m
                        (mapcat #(entry->key-value-pairs % []))
                        (map #(update-in % [0] prepare-key))
                        (map #(string/join "=" %)))))

(defmulti ^:private parse-key-segment
  (fn [segment]
    (cond
      (re-find #"\[.+\]" segment) :keyword
      (re-find #"\[\]" segment) :index
      :else :default)))

(defmethod ^:private parse-key-segment :keyword
  [segment]
  (-> segment
      (string/replace #"\[|\]" "")
      keyword))

(defmethod ^:private parse-key-segment :index
  [segment]
  ::index)

(defmethod ^:private parse-key-segment :default
  [segment]
  (keyword segment))

(defn- parse-key
  "Takes a query string key like user[first-name] and
  returns a sequence containing the keys in order, appropriate
  for use with assoc-in
  (like [:user :first-name])"
  [k]
  ; I couldn't make the lookbehind work, so we have to parse
  ; the square brackets out in a separate step
  (->> (re-find #"([^\[\]]+)(\[[^\]]*\])*" k)
       rest              ; this first position holds the entire match
       (filter identity)
       (map parse-key-segment)))

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
