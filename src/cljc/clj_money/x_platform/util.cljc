(ns clj-money.x-platform.util
  (:require [clojure.string :as string]
            #?(:clj [clj-time.core :as t]
               :cljs [cljs-time.core :as t])
            #?(:clj [clj-time.format :as f]
               :cljs [cljs-time.format :as f])))

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

(defn desc-periodic-seq
  ([end period-like]
   (lazy-seq (cons end
                   (desc-periodic-seq (t/minus end period-like)
                                      period-like))))
  ([start end period-like]
   (take-while #(or (t/after? % start)
                    (t/equal? % start))
                 (desc-periodic-seq end period-like))))

(defn serialize-date [d]
  (when d
    (f/unparse-local-date (f/formatters :date) d)))

(defn unserialize-date [s]
  (when (seq s)
    (f/parse-local-date (f/formatters :date) s)))

(defn format-date [d]
  (when d
    (f/unparse-local-date (f/formatter "M/d/yyyy") d)))

(defmulti update-in-criteria
  (fn [criteria attr _f]
    (when-let [value (get-in criteria [attr])]
      (if (sequential? value)
        :compound
        :simple))))

(defn ensure-keyword
  "Given a String or a keyword, returns a keyword"
  [value]
  (if (keyword? value)
    value
    (keyword value)))

(defmethod update-in-criteria :default
  [criteria _attr _f]
  criteria)

(defmethod update-in-criteria :simple
  [criteria attr f]
  (update-in criteria [attr] f))

(defmethod update-in-criteria :compound
  [criteria attr f]
  (update-in criteria [attr] (fn [value]
                               (vec (cons (-> value first ensure-keyword)
                                          (map f (rest value)))))))

(defn- ensure-string
  [value]
  (if (keyword? value)
    (name value)
    (str value)))

(defn path
  [& segments]
  (str "/" (->> segments
                (map ensure-string)
                (string/join "/"))))

(defn parse-int
  [value]
  (when value
    #?(:clj (Integer/parseInt value)
       :cljs (js/parseInt value))))

(defn model->id
  [value]
  (or (:id value)
      value))
