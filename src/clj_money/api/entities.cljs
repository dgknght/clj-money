(ns clj-money.api.entities
  (:refer-clojure :exclude [update])
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.api-async :as api]
            [clj-money.api :refer [handle-ex]]))

(defn- after-read
  [entity]
  (update-in-if entity
                [:settings :monitored-account-ids]
                set))

(defn- transform
  [xf]
  (comp (api/apply-fn after-read)
        xf))

(defn select
  [xf]
  (api/get (api/path :entities)
           {}
           {:transform (transform xf)
            :handle-ex (handle-ex "Unable to retrieve the entities: %s")}))

(defn create
  [entity xf]
  (api/post (api/path :entities)
            entity
            {:transform (transform xf)
             :handle-ex (handle-ex "Unable to create the entity: %s")}))

(defn update
  [entity xf]
  (api/patch (api/path :entities (:id entity))
             entity
             {:transform (transform xf)
              :handle-ex (handle-ex "Unable to update the entity: %s")}))

(defn save
  [entity xf]
  (let [f (if (:id entity)
            update
            create)]
    (f entity xf)))

(defn delete
  [entity xf]
  (api/delete (api/path :entities (:id entity))
              {:transform xf
               :handle-ex (handle-ex "Unable to remove the entity: %s")}))
