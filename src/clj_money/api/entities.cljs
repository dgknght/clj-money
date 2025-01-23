(ns clj-money.api.entities
  (:refer-clojure :exclude [update])
  (:require [clj-money.api :as api :refer [handle-ex]]))

(defn select
  [xf]
  (api/get (api/path :entities)
           {}
           {:post-xf xf
            :handle-ex (handle-ex "Unable to retrieve the entities: %s")}))

(defn create
  [entity xf]
  (api/post (api/path :entities)
            entity
            {:post-xf xf
             :handle-ex (handle-ex "Unable to create the entity: %s")}))

(defn update
  [entity xf]
  (api/patch (api/path :entities (:id entity))
             entity
             {:post-xf xf
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
              {:post-xf xf
               :handle-ex (handle-ex "Unable to remove the entity: %s")}))
