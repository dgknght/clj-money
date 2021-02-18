(ns clj-money.util
  (:require [clojure.string :as string]
            #?(:cljs [clj-money.decimal :as decimal])
            #?(:clj [clj-time.core :as t]
               :cljs [cljs-time.core :as t])
            #?(:clj [clj-time.format :as f]
               :cljs [cljs-time.format :as f])
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]]))
  #?(:clj (:import java.util.UUID
                   java.text.NumberFormat
                   java.text.DecimalFormat)
     :cljs (:import goog.i18n.NumberFormat)))

(defn trace
  [msg]
  (pprint msg))

(defn abs
  [value]
  #?(:clj (.abs value) ; we're assuming BigDecimal here
     :cljs (Math/abs value)))

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

(defmulti present?
  #(cond
     (string? %) :string
     (coll? %) :collection))

(defmethod present? :string
  [value]
  (and value (seq value)))

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

(defn parse-float
  [value]
  (when value
    #?(:clj (Float/parseFloat value)
       :cljs (js/parseFloat value))))

(def boolean-values #{"true" "1" "y" "yes" "t"})

(defn parse-bool
  [value]
  (when value
    (contains? boolean-values (string/lower-case value))))

(defn ->id
  [value]
  (or (:id value)
      value))

(defn ->coll
  [value]
  (if (coll? value)
    value
    [value]))

(defn update-in-if
  "Performs an update-in if the key already exists in the map."
  [m k-path f]
  (if-let [v (get-in m k-path)]
    (assoc-in m k-path (f v))
    m))

(defn assoc-if
  "Performs an assoc if the specified value is not nil."
  [m k v]
  (if v
    (assoc m k v)
    m))

(defn assoc-unless
  "Performs an assoc if the specified key is not already present in the map."
  [m k v]
  (if (get-in m [k])
    m
    (assoc m k v)))

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
         (mapv #(keyword (str str-k %)))
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

(defn ->indexed-map
  ([coll] (->indexed-map coll :id))
  ([coll k]
   (->> coll
        (map (juxt k identity))
        (into {}))))

(defn parse-decimal
  [input]
  #?(:cljs (decimal/->decimal input)
     :clj (bigdec input)))

(defn- conj-to-last
  [lists x]
  (conj (pop lists)
        (conj (peek lists) x)))

(defn- nest-parens
  [elems]
  (first
    (reduce (fn [lists elem]
              (case elem
                "(" (conj lists [])
                ")" (conj-to-last (pop lists)
                                  (peek lists))
                (conj-to-last lists elem)))
            '([])
            elems)))

#?(:cljs (def ^:private operations
           {"+" decimal/+
            "-" decimal/-
            "*" decimal/*
            "/" decimal//}))

(declare eval-math*)

(defn- eval-statement
  "Evaluates a traditional, simple math operation.

  E.g.:
  (eval-statement [1 \"+\" 1]) => 2"
  [[o1 oper o2]]
  (let [args (map eval-math* [o1 o2])]
    #?(:clj (eval (apply
                    list (symbol "clojure.core" oper)
                    args))
       :cljs (apply (operations oper)
                    args))))

(defn- eval-one
  "Takes a sequence describing a mathematical expression and
  evaluates one subexpression, returning the original expression
  with the one evaluated subexpression resolved"
  [elems opers]
  (->> elems
       (partition-all 3 2)
       (reduce
         (fn [result [o1 oper :as stm]]
           (if (:processed? result)
             (update-in result [:result] concat (rest stm))
             (if (= 3 (count stm))
               (if (opers oper)
                 (-> result
                     (assoc :processed? true) ; the one sub expression has been evaluated, don't evalute more this pass
                     (update-in [:result] conj (eval-statement stm)))
                 (update-in result [:result] conj o1 oper))
               (update-in result [:result] concat stm))))
         {:result []})
       :result))

(defn- until-same
  "Performs the function f on the initial value init,
  plus any additional arguments, comparing the result
  to the initial value. If they match, the result is
  returned. If not, the function is applied to the result
  until the result matches the input."
  [f init & args]
  (let [last-result (atom init)]
    (loop [result (apply f init args)]
      (if (= @last-result result)
        result
        (do
          (reset! last-result result)
          (recur (apply f result args)))))))

(defn- perform-opers
  [elems opers]
  (until-same eval-one elems opers))

(defn- mdas
  [elems]
  (loop [elems elems
         oper-sets [#{"*" "/"}
                    #{"+" "-"}]]
    (if (= 1 (count elems))
      (eval-math* (first elems))
      (when (odd? (count elems))
        (recur (perform-opers elems (first oper-sets))
               (rest oper-sets))))))

; make mulitple passes for each operator to enforce pemdas
; perform one calculation per operater set pass
; when an operator set pass returns the same result 2 times, move to the next operator set
; e.g.
; apply #{"*" "/"} to 1 + 2 + 3 => 1 + 2 + 3 (will return the unchanged input)
; apply #{"+" "-"} to 1 + 2 + 3 => 3 + 3
; apply #{"+" "-"} to 3 + 3     => 6
;
; apply #{"*" "/"} to 1 + 2 * 3 => 1 + 6    (when processing the result of the 1st pass will return the result of the 1st pass)
; apply #{"+" "-"} to 1 + 6     => 7


(defmulti eval-math*
  #(cond
     (vector? %) :vector
     (string? %) :scalar))

(defmethod eval-math* :default
  [elem]
  elem)

(defmethod eval-math* :vector
  [elems]
  (mdas elems))

(defmethod eval-math* :scalar
  [elem]
  (parse-decimal elem))

(defn eval-math
  [input]
  (->> (re-seq #"\d+(?:\.\d+)?|[)(*+/-]" input)
       nest-parens
       eval-math*))

(defn earliest
  [& ds]
  (->> ds
       (filter identity)
       sort
       first))

(defn make-series
  "Given a template and a list of maps, create a sequence
  of the template merged with each map in the list"
  [template & series]
  (map #(merge template %) series))
