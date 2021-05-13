(ns clj-money.api.entities
  (:refer-clojure :exclude [update])
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.api :as api]))

(defn- after-read
  [entity]
  (update-in-if entity
                [:settings :monitored-account-ids]
                set))

(defn select
  [success-fn error-fn]
  (api/get (api/path :entities)
           (comp success-fn
                 #(map after-read %))
           error-fn))

(defn create
  [entity success-fn error-fn]
  (api/post (api/path :entities)
            entity
            (comp
              success-fn
              after-read)
            error-fn))

(defn update
  [entity success-fn error-fn]
  (api/patch (api/path :entities (:id entity))
             entity
             (comp
               success-fn
               after-read)
             error-fn))

(defn save
  [entity success-fn error-fn]
  (if (:id entity)
    (update entity success-fn error-fn)
    (create entity success-fn error-fn)))

(defn delete
  [entity success-fn error-fn]
  (api/delete (api/path :entities (:id entity))
              success-fn
              error-fn))
