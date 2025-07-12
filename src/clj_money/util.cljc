(ns clj-money.util
  (:refer-clojure :exclude [abs format group-by])
  (:require [clojure.string :as string]
            [clojure.set :refer [rename-keys]]
            [clojure.walk :refer [postwalk]]
            #?(:cljs [goog.string])
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            [clj-money.models.schema :as schema]))

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
(derive #?(:clj clojure.lang.PersistentHashSet
           :cljs cljs.core/PersistentHashSet)
        ::set)
(derive #?(:clj clojure.lang.MapEntry
           :cljs cljs.core/MapEntry)
        ::map-entry)

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

(def model-types
  (->> schema/models
       (map :id)
       set))

(def valid-model-type? model-types)

(declare model-type)

(defn model-type-dispatch [x & _]
  {:pre [x]}
  (model-type x))

(defn- extract-model-type
  [m-or-t]
  (if (keyword? m-or-t)
    m-or-t
    (model-type m-or-t)))

(defmulti ^:private model-type* type)

; We expect this to be a criteria with a conjunction
(defmethod model-type* ::vector
  [[_conj & cs :as x]]
  (or (-> x meta :clj-money/model-type)
      (let [ts (set (map model-type cs))]
        (when (= 1 (count ts))
          (first ts)))))

(defmethod model-type* ::map
  [x]
  (or (-> x meta :clj-money/model-type)
      (->> (keys x)
           (map namespace)
           (filter identity)
           (clojure.core/group-by identity)
           (map #(update-in % [1] count))
           (sort-by second >)
           (map first)
           first
           keyword)))

(defmethod model-type* ::keyword
  [x]
  #(model-type % x))

(defn model-type
  "The 1 arity, when given a model, retrieves the type for the given model.
  When given a keyword, returns a function that sets the model type when given
  a model.
  The 2 arity sets the type for the given model in the meta data. The 2nd argument is either a
  key identyfying the model type, or another model from which the type is to be
  extracted"
  ([x]
   {:pre [x]}
   (model-type* x))
  ([m model-or-type]
   {:pre [(or (map? m)
              (vector? m))
          (or (map? model-or-type)
              (keyword? model-or-type))]}
   (let [t (extract-model-type model-or-type)]
     (assert (valid-model-type? t))
     (vary-meta m assoc :clj-money/model-type t))))

(defn model-type?
  "The 2 arity checks if the model has the specified type. The 1 arity
  returns a predicate function that returns true if given an argument
  that has the specified model type."
  ([m-type]
   #(model-type? % m-type))
  ([model m-type]
   (= m-type (model-type model))))

(defn format
  [msg & args]
  #?(:clj (apply clojure.core/format msg args)
     :cljs (apply goog.string/format msg args)))

(defn type-dispatch [x & _] (type x))

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

(defn group-by
  ([key-fn coll]
   (group-by key-fn identity coll))
  ([key-fn val-fn coll]
   (reduce (fn [result x]
             (update-in result [(key-fn x)] (fnil conj []) (val-fn x)))
           {}
           coll)))

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

(defn id=
  "Given a list of maps, returns true if they all have the same :id attribute"
  [& models]
  (->> models
       (map :id)
       (apply =)))

(defn model=
  "Given a list of maps, returns true if they all have the same :id
  attribute and do not have conflicting db/model-type values"
  [& models]
  (let [types (->> models
                   (map model-type)
                   (filter identity)
                   set)]
    (and (<= (count types) 1)
         (apply id= models))))

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
        (and current
             (child? mdl))
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

(def ^:private ->id (some-fn :id identity))

(defn temp-id
  "Generates a new temporary id"
  []
  (str "temp-" (random-uuid)))

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn +temp-id
  [model]
  (assoc model :id (temp-id)))

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
   :price/value
   :price/trade-date
   :budget/name
   :budget-item/account
   :import/entity-name
   :image/original-filename])

(def ^:private db-oper?
  #{:clj-money.db/delete
    :clj-money.db/insert
    :clj-money.db/update})

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
    #(apply simplify % (cons a1 args))

    (and (vector? a1)
         (= 2 (count a1))
         (db-oper? (first a1)))
    (update-in a1 [1] #(apply simplify % args))

    ; iterate over the sequence and apply any options
    (sequential? a1)
    (map #(apply simplify % args)
         a1)

    ; apply to a model map
    (map? a1)
    (let [{:keys [include]} (apply hash-map args)
          selected (select-keys a1 (concat simple-keys include))]
      (if (empty? selected)
        a1
        selected))

    ; just return anything we don't recognize as-is
    :else
    a1))

(defn upsert-into
  "Given a collection and an item, either update the item in the collection,
  or insert the item, depending on whether another item with the same :id attribute
  already exists.

  Options:
    :sort-key - A function that extracts the value on which to sort the collection."
  ([m coll] (upsert-into m {} coll))
  ([m {:keys [sort-key comp] :or {sort-key identity comp compare}} coll]
   (sort-by #(sort-key %)
            comp
            (cons m (remove #(model= m %) coll)))))

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

(defn url-safe-keyword
  [k]
  (let [[space nom] ((juxt namespace name) k)]
    (if space
      (keyword (str space "_" nom))
      k)))

(defn <-url-safe-keyword
  [k]
  (let [m (re-find #"^([^_]+)_([^_]+)$" (name k))]
    (if m
      (keyword (nth m 1) (nth m 2))
      k)))

(defn remove-nils
  "Given a data structrure, return the structure with any nil map
  values removed"
  [x]
  (postwalk (fn [v]
              (if (and (map-entry? v)
                       (nil? (val v)))
                nil
                v))
            x))

(defn locate-nils
  "Given a data structure, return a list of key vectors where nils
  are found."
  ([x] (locate-nils x []))
  ([x prefix]
   (reduce (fn [res x*]
             (if (map-entry? x*)
               (let [[k v] x*]
                 (cond
                   (nil? v)
                   (conj res (conj prefix k))

                   (and (sequential? v)
                        (map? (first v)))
                   (apply concat res (map-indexed
                                       (fn [idx itm]
                                         (locate-nils itm (conj prefix k idx)))
                                       v))

                   :else
                   res))
               res))
           []
           x)))

(defn +id
  "Given a map without an :id value, adds one with a random UUID as a value"
  ([m] (+id m random-uuid))
  ([m id-fn]
   {:pre [(map? m)]}
   (if (:id m)
     m
     (assoc m :id (id-fn)))))

(defn deep-rename-keys
  "Given a data structure, rename keys in all contained maps"
  [x key-map]
  (postwalk (fn [x*]
              (if (map? x*)
                (rename-keys x* key-map)
                x*))
            x))

(defn- normalize-sort-key
  [x]
  (if (vector? x)
    (if (= 1 (count x))
      (conj x :asc)
      x)
    [x :asc]))

(defn- compare-fn
  [& ms]
  (fn [_ [k dir]]
    (let [[v1 v2] (map k ms)
          f (if (= :desc dir)
              #(compare %2 %1)
              compare)
          res (f v1 v2)]
      (if (= 0 res)
        0
        (reduced res)))))

(defn- ->comparator
  [order-by]
  (let [normalized (mapv normalize-sort-key order-by)]
    (fn [m1 m2]
      (reduce (compare-fn m1 m2)
              0
              normalized))))

(defn apply-sort
  "Given a sequence of models, apply the sort specified in the options map"
  [opts models]
  (if-let [sort-spec ((some-fn :order-by :sort) opts)]
    (sort (->comparator sort-spec)
          models)
    models))
