(ns clj-money.api.entities
  (:refer-clojure :exclude [update])
  (:require [clj-money.api :as api :refer [add-error-handler]]))

(defn select
  [& {:as opts}]
  (api/get (api/path :entities)
           {}
           (add-error-handler
             opts
             "Unable to retrieve the entities: %s")))

(defn create
  [entity opts]
  (api/post (api/path :entities)
            entity
            (add-error-handler
              opts
              "Unable to create the entity: %s")))

(defn update
  [entity opts]
  (api/patch (api/path :entities (:id entity))
             entity
             (add-error-handler
               opts
               "Unable to update the entity: %s")))

(defn save
  [entity & {:as opts}]
  (let [f (if (:id entity)
            update
            create)]
    (f entity opts)))

(defn delete
  [entity & {:as opts}]
  (api/delete (api/path :entities (:id entity))
              (add-error-handler
                opts
                "Unable to delete the entity: %s")))
