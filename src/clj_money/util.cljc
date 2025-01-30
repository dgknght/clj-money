(ns clj-money.util
  (:refer-clojure :exclude [abs format])
  (:require [clojure.string :as string]
            #?(:cljs [goog.string])
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])))

(derive #?(:clj java.lang.String
           :cljs js/String)
        ::string)
(derive #?(:clj clojure.lang.Keyword
           :cljs cljs.core/Keyword)
        ::keyword)
(derive #?(:clj clojure.lang.PersistentVector
           :cljs cljs.core/PersistentVector)
        ::vector)
(derive #?(:clj clojure.lang.PersistentList
           :cljs cljs.core/List)
        ::list)
(derive #?(:clj clojure.lang.PersistentList$EmptyList
           :cljs cljs.core/EmptyList)
        ::list)
(derive ::vector ::collection)
(derive ::list ::collection)
(derive #?(:clj clojure.lang.PersistentArrayMap
           :cljs cljs.core/PersistentArrayMap)
        ::map)
(derive #?(:clj clojure.lang.PersistentHashMap
           :cljs cljs.core/PersistentHashMap)
        ::map)
(derive #?(:clj clojure.lang.MapEntry
           :cljs cljs.core/MapEntry)
        ::map-entry)

(defn format
  [msg & args]
  #?(:clj (apply clojure.core/format msg args)
     :cljs (apply goog.string/format msg args)))

(defn type-dispatch [x & _] (type x))

(defn abs
  [value]
  #?(:clj (.abs value) ; we're assuming BigDecimal here
     :cljs (Math/abs value)))

(defmulti present? type)

(defmethod present? :default
  [x]
  (not (not x)))

(defmethod present? ::string
  [value]
  (and value (seq value)))

(defmethod present? ::collection
  [col]
  (some present? col))

(def blank?
  (complement present?))

(defn presence
  [x]
  (when (present? x) x))

(defn presence-or
  [value default]
  (or (presence value) default))

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

(defn make-series
  "Given a template and a list of maps, create a sequence
  of the template merged with each map in the list"
  [template & series]
  (map #(merge template %) series))

(defn pp->
  [v m & {:keys [meta? transform]
          :or {transform identity}}]
  (binding [*print-meta* meta?]
    (pprint {m (transform v)}))
  v)

(defn pp->>
  ([m v] (pp->> m {} v))
  ([m {:keys [transform] :or {transform identity}} v]
   (pprint {m (transform v)})
   v))

(defn qualifier
  "Given a map, returns the namespace from the keys. If there is more than one
  namespace, an exception is thrown. If none of the keys are qualified, nil is
  returned."
  [m]
  {:pre [(map? m)]}
  (let [n (->> (keys m)
               (map namespace)
               (filter identity)
               (into #{}))]
    (assert (= 1 (count n))
            "The map contains more than one keyword namespace, so the qualifier cannot be inferred.")
    (first n)))

(defmulti qualify-key type-dispatch)

(defmethod qualify-key :default
  [x & _]
  x)

(defmethod qualify-key ::map-entry
  [[k :as x] nspace {:keys [ignore?]}]
  (if (ignore? k)
    x
    (update-in x [0] #(keyword nspace (name %)))))

; TODO: delete this when we upgrade to clojurescript 1.11.5
#?(:cljs
   ^{:clj-kondo/ignore [:redefined-var]}
   (defn update-keys
     [m f]
     (let [ret (persistent!
                 (reduce-kv (fn [acc k v]
                              (assoc! acc (f k) v))
                            (transient {})
                            m))]
       (with-meta ret (meta m)))))

(defn qualify-keys
  "Creates fully-qualified entity attributes by applying
  the :model-type from the meta data to the keys of the map."
  [m ns-key & {:keys [ignore]}]
  {:pre [(map? m)]}
  (let [qualifier (if (keyword? ns-key)
            (name ns-key)
            ns-key)
        ignore? (if ignore
                  (some-fn ignore namespace)
                  namespace)]
    (update-keys m (fn [k]
                     (if (ignore? k)
                       k
                       (keyword qualifier (name k)))))))

(defn model=
  [& models]
  (->> models
       (map :id)
       (apply =)))

(defn ->model-ref
  [map-or-id]
  (if (map? map-or-id)
    (select-keys map-or-id [:id])
    {:id map-or-id}))

(defn model-ref?
  [x]
  (and (map? x)
       (= #{:id} (set (keys x)))))

(defn reconstruct
  "Given a list of models and a few options, aggregates child models into their parents."
  [{:keys [children-key parent? child?]} models]
  {:pre [(seq models) children-key parent? child?]}
  ; This logic assumes the order established in deconstruct is maintained
  (loop [input models output [] current nil]
    (if-let [mdl (first input)]
      (cond
        (child? mdl)
        (recur (rest input)
               output
               (update-in current [children-key] (fnil conj []) mdl))

        (parent? mdl)
        (recur (rest input)
               (if current
                 (conj output current)
                 output)
               mdl)

        :else
        (recur (rest input)
               (if current
                 (conj output current mdl)
                 (conj output mdl))
               nil))
      (if current
        (conj output current)
        output))))

(defn cache-fn
  "Given a function that takes a single argument and returns a resource,
  return a function that caches the result of the given function and returns
  the cached value for subsequent calls."
  [f]
  (let [cache (atom {})]
    (fn [id]
      (if-let [cached (@cache id)]
        cached
        (let [retrieved (f id)]
          (swap! cache assoc id retrieved)
          retrieved)))))

(defn temp-id
  "Generates a new temporary id"
  []
  (str "temp-" (random-uuid)))

(def ^:private ->id (some-fn :id identity))

(defn temp-id?
  "Given a model or an id, returns true if the model has a temporary
  id or if the specified id is a temporary id"
  [id-or-model]
  (when-let [id (->id id-or-model)]
    (and (string? id)
         (string/starts-with? id "temp-"))))

(defn live-id
  [{:keys [id]}]
  (when-not (temp-id? id)
    id))

(def live-id? (complement temp-id?))

(def simple-keys
  [:user/email
   :account/name
   :entity/name
   :commodity/symbol
   :transaction/transaction-date
   :transaction/description
   :transaction-item/quantity
   :transaction-item/action
   :scheduled-transaction/description
   :scheduled-transaction-item/quantity
   :scheduled-transaction-item/action
   :lot/purchase-date
   :lot/shares-owned
   :lot-item/lot-action
   :lot-item/quantity
   :price/price
   :price/trade-date
   :budget/name
   :budget-item/account
   :import/entity-name
   :image/original-filename])

(defn simplify
  "Return the given model maps with non-essentail attributes removed

  (simplify account) -> {:account/name \"Checking\"}
  (simplify account :include [:account/value]) -> {:account/name \"Checking\" :account/value 100M}
  (simplify [a1 a2]) -> [{:account/name \"Checking\"} {:account/name \"Savings\"}]
  (simplify [a1 a2] :include [:account/value]) -> [{:account/name \"Checking\" :account/value 100M} {:account/name \"Savings\" :account/value 1000M}]
  (simplify :include [:account/value]) -> fn that can be applied to a model or sequence of models"
  [& [a1 & args]]
  (cond
    ; the args are the options, return a function with the specified options
    (keyword? a1)
    #(apply simplify % args)

    ; iterate over the sequence and apply any options
    (sequential? a1)
    (map #(apply simplify % args)
         a1)

    ; don't simplify model refs
    (model-ref? a1)
    a1

    ; apply to a model map
    :else
    (let [{:keys [include]} (apply hash-map args)]
      (select-keys a1 (concat simple-keys include)))))

(defn upsert-into
  "Given a collection and an item, either update the item in the collection,
  or insert the item, depending on whether another item with the same :id attribute
  already exists.

  Options:
    :sort-key - A function that extracts the value on which to sort the collection."
  [m {:keys [sort-key comp] :or {sort-key identity comp compare}} coll]
  (sort-by #(sort-key %)
           comp
           (cons m (remove #(model= m %) coll))))

(defn match?
  "Given a search term and a list of keys, returns a predicate function that
  indicates whether or not a given model map matches the search term."
  [term & ks]
  (let [t (string/lower-case term)
        fns (map (fn [k]
                   (comp #(string/includes? % t)
                         string/lower-case
                         k))
                 ks)]
    (apply some-fn fns)))
