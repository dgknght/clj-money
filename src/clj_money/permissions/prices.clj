(ns clj-money.permissions.prices
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [user-owns-entity?
                                                   user-granted-access?
                                                   all-user-entity-ids]]
            [clj-money.models.entities :as entities]
            [clj-money.models.commodities :as commodities]))

(authorization/allow :price user-owns-entity?)
(authorization/allow :price user-granted-access?)

; TODO This logic query could bog down pretty easily, need to consider alternative strategies here
(authorization/set-scope
  :price
  {:commodity-id (fn [user {storage-spec :storage-spec :as context}]
                   (let [entity-ids (all-user-entity-ids user context)]
                     (if (seq entity-ids)
                       (->> {:entity-id entity-ids}
                            (commodities/search storage-spec)
                            (map :id)
                            (into #{}))
                       #{})))})
