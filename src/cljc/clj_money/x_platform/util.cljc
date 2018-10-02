(ns clj-money.x-platform.util
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clj-money.util :refer [pprint-and-return]]))

(defmulti ^:private entry->key-value-pairs
  (fn [[_ v] _]
    (if (map? v)
      :map
      :default)))

(defmethod ^:private entry->key-value-pairs :map
  [[k v] prefix-vec]
  (mapcat #(entry->key-value-pairs % (conj prefix-vec k)) v))

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

(defn- parse-key
  "Takes a query string key like user[first-name] and
  returns a tuple containing the most specified key
  in the first position and the prefix in the second
  (like [:first-name [:user]])"
  [k]
  ; I couldn't make the lookbehind work, so we have to parse
  ; the square brackets out in a separate step
  (->> (re-find #"([^\[\]]+)(\[[^\]]+\])*" k)
       rest              ; this first position holds the entire match
       (filter identity) ; the last position seems always to be nil
       (map #(string/replace % #"\[|\]" "") )
       (map keyword)))

(defn- collapse-collections
  [key-value-pairs]
  (reduce (fn [result [k v]]
            (let [k-vec (parse-key k)]
              (assoc-in result k-vec v)))
          {}
          key-value-pairs))

(defn query-string->map
  [query-string]
  (collapse-collections (->> (string/split query-string #"&")
                             (map #(string/split % #"=")))))
