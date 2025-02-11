(ns clj-money.db
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [union]]
            [config.core :refer [env]]
            [clj-money.util :as util]))

(def ^:dynamic *storage* nil)

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

(def type-dispatch util/type-dispatch)

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

(defmulti ^:private model-type* type)

; We expect this to be a criteria with a conjunction
(defmethod model-type* ::util/vector
  [[_conj & cs :as x]]
  (or (-> x meta ::type)
      (let [ts (set (map model-type cs))]
        (when (= 1 (count ts))
          (first ts)))))

(defmethod model-type* ::util/map
  [x]
  (or (-> x meta ::type)
      (single-ns (dissoc x :id))))

(defmethod model-type* ::util/keyword
  [x]
  #(model-type % x))

(def model-type util/model-type)

(def model-type? util/model-type?)
