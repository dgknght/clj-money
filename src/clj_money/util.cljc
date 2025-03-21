(ns clj-money.util
  (:refer-clojure :exclude [abs])
  (:require [clojure.string :as string]
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])))

#?(:clj (derive clojure.lang.PersistentVector ::vector)
   :cljs (derive cljs.core.PersistentVector ::vector))
#?(:clj (derive clojure.lang.PersistentArrayMap ::map)
   :cljs (derive cljs.core.PersistentArrayMap ::map))
#?(:clj (derive clojure.lang.PersistentHashMap ::map)
   :cljs (derive cljs.core.PersistentHashMap ::map))
#?(:clj (derive java.lang.String ::string)
   :cljs (derive js/String ::string))

(defn abs
  [value]
  #?(:clj (.abs value) ; we're assuming BigDecimal here
     :cljs (Math/abs value)))

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

(defn- includes-time?
  [date]
  #?(:clj (instance? org.joda.time.DateTime date)
     :cljs (instance? goog.date.DateTime date)))

(defn- nominal-key
  [key-base value]
  (let [[oper value] (if (sequential? value)
                       value
                       [:= value])
        prep (when-not (string/ends-with? key-base "date") (if (includes-time? value)
                                                             "at"
                                                             "on"))]
    (keyword
      (str
        key-base
        (case oper
          := (when prep (str "-" prep))
          :>  "-after"
          :>= (str "-" prep "-or-after")
          :<  "-before"
          :<= (str "-" prep "-or-before"))))))

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
  (let [value (get-in m [key-base])]
    (if (and (sequential? value)
             (= :between (first value)))
      (let [[start end] (drop 1 value)]
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
          m))
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
         :update-fn (fn [result str-k _original-key value]
                      (assoc result (nominal-key str-k value)
                             (if (sequential? value)
                               (second value)
                               value)))})))

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

(defn pp->
  [v m]
  (pprint {m v})
  v)

(defn pp->>
  [m v]
  (pprint {m v})
  v)
