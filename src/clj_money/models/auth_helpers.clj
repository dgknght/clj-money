(ns clj-money.models.auth-helpers
  (:require [environ.core :refer [env]]
            [clj-money.models.entities :as entities]
            [clj-money.authorization :as authorization]))

(defn user-entity-ids
  [user context]
  (->> (entities/select (:storage-spec context) (:id user))
       (map :id)
       (into #{})))

(defn user-owns-entity?
  [user resource context]
  (contains? (user-entity-ids user context)
             (:entity-id resource)))

(authorization/->context :storage-spec (env :db))
