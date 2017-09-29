(ns clj-money.models.auth-helpers
  (:require [environ.core :refer [env]]
            [clj-money.models.entities :as entities]
            [clj-money.authorization :as authorization]))

(defn user-entity-ids
  [storage-spec user]
  (->> (entities/select storage-spec (:id user))
       (map :id)
       (into #{})))

(defn user-owns-entity?
  [user resource context]
  (contains? (user-entity-ids (:storage-spec context) user)
             (:entity-id resource)))

(authorization/->context :storage-spec (env :db))
