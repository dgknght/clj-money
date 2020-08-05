(ns clj-money.util
  (:require [clojure.string :as string]
            #?(:clj [clojure.tools.logging :as lg])
            #?(:clj [clj-time.core :as t]
               :cljs [cljs-time.core :as t])
            #?(:clj [clj-time.format :as f]
               :cljs [cljs-time.format :as f]))
  #?(:clj (:import java.util.UUID
                   java.text.NumberFormat
                   java.text.DecimalFormat)
     :cljs (:import goog.i18n.NumberFormat)))

(defn log
  ([msg] (log :debug msg))
  ([level msg]
   #?(:clj (lg/log level msg)
      :cljs (.log js/console (prn-str {level msg})))))

#?(:clj
   (defn uuid
     ([] (UUID/randomUUID))
     ([value]
      (if (instance? UUID value)
        value
        (UUID/fromString (str value))))))

(defn- number-format
  [fmt]
  #?(:clj (doto (case fmt
                  :decimal (DecimalFormat.)
                  :percent (NumberFormat/getPercentInstance))
            (.setGroupingUsed true))
     :cljs (NumberFormat. (case fmt
                            :decimal (.-DECIMAL (.-Format NumberFormat))
                            :percent (.-PERCENT (.-Format NumberFormat))))))

(defn format-decimal
  ([value] (format-decimal value {}))
  ([value {:keys [fraction-digits]
           :or {fraction-digits 2}}]
   (.format (doto (number-format :decimal)
              (.setMaximumFractionDigits fraction-digits)
              (.setMinimumFractionDigits fraction-digits))
            value)))

(defn format-percent
  ([value] (format-percent value {}))
  ([value {:keys [fraction-digits]
           :or {fraction-digits 1}}]
   (.format (doto (number-format :percent)
              (.setMaximumFractionDigits fraction-digits)
              (.setMinimumFractionDigits fraction-digits))
            value)))


(defmulti presence
  #(cond
     (string? %) :string
     (coll? %) :collection))

(defmethod presence :string
  [value]
  (when-not (empty? value)
    value))

(defmethod presence :collection
  [values]
  (when-not (empty? values)
    values))

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

(defn unserialize-date-time [s]
  (when (seq s)
    (f/parse (f/formatters :date-time-no-ms) s)))

(defn format-date-time [dt]
  (when dt
    (f/unparse (f/formatter "M/d/yyyy h:mm A") dt)))

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

(def boolean-values #{"true" "1"})

(defn parse-bool
  [value]
  (when value
    (contains? boolean-values (string/lower-case value))))

(defn model->id
  [value]
  (or (:id value)
      value))

(defn update-in-if
  [m k-path f]
  (if-let [v (get-in m k-path)]
    (assoc-in m k-path (f v))
    m))

(defn deep-contains?
  [data k]
  (cond
    (vector? data) (some #(deep-contains? % k) data)
    (map? data)    (contains? data k)
    :else          false))

(defn deep-get
  [data k]
  (cond
    (vector? data) (some #(deep-get % k) data)
    (map? data)    (get-in data [k])
    :else          nil))

(defn deep-update-in-if
  [data k f]
  (cond
    (vector? data) (mapv #(deep-update-in-if % k f) data)
    (map? data)    (update-in-if data [k] f)
    :else          data))

(defn deep-dissoc
  [data k]
  (cond
    (vector? data) (mapv #(deep-dissoc % k) data)
    (map? data)  (dissoc data k)
    :else data))

(defn- includes-time?
  [date]
  #?(:clj (instance? org.joda.time.DateTime date)
     :cljs (instance? goog.date.DateTime date)))

(defn nominal-keys
  "Given a canonical key, return all of the nominal variants"
  [canonical]
  (let [str-key (name canonical)
        [_  base-key] (re-find #"^(.*)(?:-on|-at)$" str-key)]
    (map keyword [str-key
                  (str base-key "-before")
                  (str str-key "-or-before")
                  (str base-key "-after")
                  (str str-key "-or-after")])))

(defn- nominal-key
  [key-base [oper value]]
  (let [prep (if (includes-time? value)
               "at"
               "on")]
    (keyword
      (str
        key-base
        "-"
        (case oper
          :>  "after"
          :>= (str prep "-or-after")
          :<  "before"
          :<= (str prep "-or-before"))))))

(defn- apply-to-dynamic-keys
  [m {:keys [key-base suffixes update-fn]}]
  (let [str-k (name key-base)]
    (->> suffixes
         (mapv #(keyword (str str-k %)) )
         (reduce (fn [result k]
                    (if-let [value (get-in m [k])]
                      (-> result
                          (dissoc k)
                          (update-fn str-k k value))
                      result))
                 m))))

(defn- between->nominal
  [m key-base]
  (let [[_ start end] (get-in m [key-base])]
    (if (and start end)
      (let [str-key (name key-base)
            prefix (if (string/ends-with? str-key "date")
                     "-on"
                     "")]
        (-> m
            (assoc (keyword (str str-key
                                 prefix
                                 "-or-after")) start
                   (keyword (str str-key
                                 prefix
                                 "-or-before")) end)
            (dissoc key-base)))
      m)))

(defn nominal-comparatives
  "Accepts a map and a key base and converts values with attributes
  that match the key base from symbolic comparatives into nominal
  comparatives.

  (nominal-comparatives {:end-on [:> some-date]} :end) => {:end-after some-date}"
  [m key-base]
  (-> m
      (between->nominal key-base)
      (apply-to-dynamic-keys
        {:key-base key-base
         :suffixes ["-on" "-at" nil]
         :update-fn (fn [result str-k _ value]
                      (assoc result (nominal-key str-k value)
                             (second value)))})))

(def ^:private suffix-keys
  {"before"       :<
   "on-or-before" :<=
   "at-or-before" :<=
   "after"        :>
   "on-or-after"  :>=
   "at-or-after"  :>=})

(defn nominative-variations
  [key-base]
  (let [str-key (name key-base)]
    (map #(keyword (str str-key %))
         [""
          "-before"
          "-on-or-before"
          "-at-or-before"
          "-after"
          "-on-or-after"])))

(defn- symbolic-key
  [key-base k value]
  (let [key-suffix (string/replace (name k) (str key-base "-") "")
        final-key (keyword
                    (str key-base
                         (when-not (string/ends-with? key-base "-date")
                           (if (includes-time? value) "-at" "-on"))))
        oper (get-in suffix-keys [key-suffix])]
    [final-key [oper value]]))

(defn nominal->between
  [m key-base]
  (let [str-key (name key-base)
        prefix (if (string/ends-with? str-key "date")
                 "-on"
                 "")
        start-key (keyword (str (name key-base) prefix "-or-after"))
        end-key (keyword (str (name key-base) prefix "-or-before"))
        start (get-in m [start-key])
        end (get-in m [end-key])]
    (if (and start end)
      (-> m
          (dissoc start-key end-key)
          (assoc key-base [:between start end]))
      m)))

(defn symbolic-comparatives
  "Accepts a map with comparative keys and updates the
  values with symbolic operators.

  (symbolic-comparatives {:end-after some-date} :end) => {:end-on [:> some-date]}"
  [m key-base]
  (-> m
      (nominal->between key-base)
      (apply-to-dynamic-keys
        {:key-base key-base
         :suffixes ["-before"
                    "-on-or-before"
                    "-at-or-before"
                    "-after"
                    "-on-or-after"
                    "-at-or-after"]
         :update-fn (fn [result str-k k value]
                      (let [[new-key value-with-oper] (symbolic-key str-k k value)]
                        (assoc result new-key value-with-oper)))})))

#?(:cljs
   (defn debounce
     [timeout f]
     (let [t (atom nil)]
       (fn [& args]
         (when @t (js/clearTimeout @t))
         (reset! t (js/setTimeout (fn []
                                    (reset! t nil)
                                    (apply f args))
                                  timeout))))))
