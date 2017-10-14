(ns clj-money.models.auth-helpers
  (:require [environ.core :refer [env]]
            [clj-money.models.entities :as entities]
            [clj-money.authorization :as authorization]))

(defn user-entity-ids
  ([user context]
   (user-entity-ids user context {}))
  ([user context options]
   (->> (entities/select (:storage-spec context) (:id user) options)
        (map :id)
        (into #{}))))

(defn all-user-entity-ids
  [user context]
  (user-entity-ids user context {:include-grants? true}))

(defn user-owns-entity?
  [user resource context]
  (contains? (user-entity-ids user context)
             (:entity-id resource)))

(authorization/->context :storage-spec (env :db))
