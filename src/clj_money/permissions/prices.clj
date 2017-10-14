(ns clj-money.permissions.prices
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [user-owns-entity?]]
            [clj-money.models.entities :as entities]
            [clj-money.models.commodities :as commodities]))

(authorization/allow :price
                     (fn [user resource _ context]
                       (let [commodity (commodities/find-by-id
                                         (:storage-spec context)
                                         (:commodity-id resource))]
                         (user-owns-entity? user commodity context))))

; TODO This logic query could bog down pretty easily, need to consider alternative strategies here
(authorization/set-scope
  :price
  {:commodity-id (fn [user {storage-spec :storage-spec}]
                   (let [entity-ids (->> (:id user)
                                         (entities/select storage-spec)
                                         (map :id))]
                     (if (seq entity-ids)
                       (->> {:entity-id entity-ids}
                            (commodities/search storage-spec)
                            (map :id)
                            (into #{}))
                       #{})))})
