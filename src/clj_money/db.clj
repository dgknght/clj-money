(ns clj-money.db
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [config.core :refer [env]]))

(def ^:dynamic *storage* nil)

(defprotocol Storage
  "Defines the functions necessary to store and retrieve data"
  (put [this models] "Saves the specified models to the data store")
  (update [this changes criteria] "Updates records with partial data (instead of full models)")
  (select [this criteria options] "Retrieves models from the data store")
  (delete [this models] "Removes models from the data store")
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
