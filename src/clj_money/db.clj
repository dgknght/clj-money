(ns clj-money.db
  (:refer-clojure :exclude [update find])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clj-money.otel :refer [with-tracing]]
            [clj-money.util :as util]
            [clj-money.config :refer [env]]))

(s/def ::operation #{::insert ::update ::delete})

(def ^:dynamic *storage* nil)

(defprotocol Storage
  "Defines the functions necessary to store and retrieve data"
  (put [this entities] "Saves the specified entities to the data store")
  (find [this id] "Fetches the entity with the given id")
  (select [this criteria options] "Retrieves entities from the data store")
  (update [this changes criteria] "Performs a batch data update")
  (delete [this entities] "Removes entities from the data store")
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

(defn tracing-storage
  [storage prefix]
  (reify Storage
    (put [_ entities]
      (with-tracing [span (format "%s/put %s"
                                  prefix
                                  (-> entities first util/entity-type))]
        (put storage entities)))

    (find [_ id]
      (with-tracing [span (format "%s/find %s"
                                  prefix
                                  id)]
        (find storage id)))
    (select [_ criteria opts]
      (with-tracing [span (format "%s/select %s"
                                  prefix
                                  (util/entity-type criteria))]
        (select storage criteria opts)))

    (delete [_ entities]
      (with-tracing [span (format "%s/delete %s"
                                  prefix
                                  (-> entities first util/entity-type))]
        (delete storage entities)))

    (update [_ changes criteria]
      (with-tracing [span (format "%s/update %s"
                                  prefix
                                  (util/single-ns changes))]
        (update storage changes criteria)))

    (close [_]
      (with-tracing [span (format "%s/close"
                                  prefix)]
        (close storage)))

    (reset [_]
      (with-tracing [span (format "%s/reset"
                                  prefix)]
        (reset storage)))))
