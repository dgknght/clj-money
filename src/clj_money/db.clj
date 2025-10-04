(ns clj-money.db
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clj-money.config :refer [env]]))

(s/def ::operation #{::insert ::update ::delete})

(def ^:dynamic *storage* nil)

(defprotocol Storage
  "Defines the functions necessary to store and retrieve data"
  (put [this models] "Saves the specified models to the data store")
  (select [this criteria options] "Retrieves models from the data store")
  (update [this changes criteria] "Performs a batch data update")
  (delete [this models] "Removes models from the data store")
  (close [this] "Releases an resources held by the instance")
  (reset [this] "Deletes all data in the data store")) ; This is only ever needed for testing. Maybe there's a better way than putting it here?

(defmulti reify-storage 
  (fn [config & _]
    (::strategy config)))

(defn storage []
  (or *storage*
      (let [active-key (get-in env [:db :active])]
        (-> env
            (get-in [:db :strategies active-key])
            reify-storage))))

(defmacro with-storage
  [bindings & body]
  `(let [storage# (reify-storage ~(first bindings))]
     (try
       (binding [*storage* storage#]
         ~@body)
       (finally
         (close storage#)))))
