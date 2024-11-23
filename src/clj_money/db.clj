(ns clj-money.db
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [union]]
            [config.core :refer [env]]))

(def ^:dynamic *storage* nil)

(def model-types
  #{:user
    :identity
    :import
    :image
    :entity
    :commodity
    :price
    :cached-price
    :account
    :transaction
    :transaction-item
    :attachment
    :budget
    :budget-item
    :grant
    :scheduled-transaction
    :reconciliation
    :lot})

(def valid-model-type? model-types)

(defprotocol Storage
  "Defines the functions necessary to store and retrieve data"
  (put [this models] "Saves the specified models to the data store")
  (update [this changes criteria] "Updates records with partial data (instead of full models)")
  (select [this criteria options] "Retrieves models from the data store")
  (delete [this models] "Removes models from the data store")
  (close [this] "Releases resources hold by the connection")
  (reset [this] "Deletes all data in the data store")) ; This is only ever needed for testing. Maybe there's a better way than putting it here?

(defmulti reify-storage 
  (fn [config & _]
    (::strategy config)))

(defn storage []
  (or *storage*
      (let [active-key (get-in env [:db :active])]
        (-> env
            (get-in [:db :strategies active-key])
            #_resolve-config-refs ; TODO: add this back in when we move to k8s
            reify-storage))))

(defmacro with-db
  "Establshes a connection to the data store, executes the body, and closes the connection."
  [bindings & body]
  `(let [storage# (reify-storage ~(first bindings))]
     (try
       (binding [*storage* storage#]
         ~@body)
       (finally
         (close storage#)))))

(declare model-type)

(defn type-dispatch
  [x & _]
  (model-type x))

(defn- extract-model-type
  [m-or-t]
  (if (keyword? m-or-t)
    m-or-t
    (model-type m-or-t)))

(defn- namespaces
  "Given a criteria (map or vector containing an operator an maps) return
   all of the namespaces from the map keys in a set."
  [x]
 (cond
   (map? x) (->> (keys x)
                 (map namespace)
                 (filter identity)
                 (map keyword)
                 set)
   (sequential? x) (->> x
                        (map namespaces)
                        (reduce union))) )

(defn- single-ns
  "Give a criteria (map or vector), return the single namespace if
  only one namespace is present. Otherwise, return nil."
  [x]
  (let [namespaces (namespaces x)]
    (when (= 1 (count namespaces))
      (first namespaces))))

(defn model-type
  "The 1 arity, when given a model, retrieves the type for the given model.
  When given a keyword, returns a function that sets the model type when given
  a model.
  The 2 arity sets the type for the given model in the meta data. The 2nd argument is either a
  key identyfying the model type, or another model from which the type is to be
  extracted"
  ([arg]
   (cond
     (vector? arg)  (-> arg meta ::type)
     (map? arg)     (or (-> arg meta ::type)
                        (single-ns (dissoc arg :id)))
     (keyword? arg) #(model-type % arg)
     :else (throw (IllegalArgumentException. "Argument must be a map, vector, or keyword"))))
  ([m model-or-type]
   {:pre [(map? m)
          (or (map? model-or-type)
              (keyword? model-or-type))]}
   (let [t (extract-model-type model-or-type)]
     (assert (valid-model-type? t))
     (vary-meta m assoc ::type t))))

(defn model-type?
  "The 2 arity checks if the model has the specified type. The 1 arity
  returns a predicate function that returns true if given an argument
  that has the specified model type."
  ([m-type]
   #(model-type? % m-type))
  ([model m-type]
   (= m-type (model-type model))))
