(ns clj-money.entities.auth-helpers
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.entities.grants :as grants]))

(defmulti ^:private fetch-entity util/entity-type-dispatch)

(defn- fetch-entity*
  [entity-or-ref]
  (if (util/entity-ref? entity-or-ref)
    (entities/find entity-or-ref :entity)
    entity-or-ref))

(defmethod fetch-entity :entity
  [resource]
  resource)

(defmethod fetch-entity :commodity
  [{:commodity/keys [entity]}]
  (fetch-entity* entity))

(defmethod fetch-entity :account
  [{:account/keys [entity]}]
  (fetch-entity* entity))

(defmethod fetch-entity :transaction
  [{:transaction/keys [entity]}]
  (fetch-entity* entity))

(defmethod fetch-entity :scheduled-transaction
  [{:scheduled-transaction/keys [entity]}]
  (fetch-entity* entity))

(defmethod fetch-entity :attachment
  [{:attachment/keys [transaction]}]
  (entities/find-by
    (util/entity-type
      {:transaction/_self transaction}
      :entity)))

(defmethod fetch-entity :budget
  [{:budget/keys [entity]}]
  (fetch-entity* entity))

(defmethod fetch-entity :budget-item
  [budget-item]
  (entities/find-by
    (util/entity-type
      {:budget-item/_self budget-item}
      :entity)))

(defmethod fetch-entity :price
  [{:price/keys [commodity]}]
  (entities/find-by
    (util/entity-type
      {:commodity/_self commodity}
      :entity)))

(defmethod fetch-entity :reconciliation
  [{:reconciliation/keys [account]}]
  (entities/find-by
    (util/entity-type
      {:account/_self account}
      :entity)))

(defmethod fetch-entity :trade
  [{:trade/keys [entity]}]
  (fetch-entity* entity))

(defn user-granted-access?
  [resource entity user action]
  (when-let [g (entities/find-by #:grant{:user user
                                       :entity entity})]
    (grants/has-permission? g
                            (util/entity-type resource)
                            action)))

(defn owner-or-granted?
  [resource user action]
  (let [entity (fetch-entity resource)]
    (or (util/entity= (:entity/user entity) user)
        (user-granted-access? resource entity user action))))
