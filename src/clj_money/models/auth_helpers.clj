(ns clj-money.models.auth-helpers
  (:require [clj-money.models.entities :as entities]))

(defn user-entity-ids
  [storage-spec user]
  (->> (entities/select storage-spec (:id user))
       (map :id)
       (into #{})))
