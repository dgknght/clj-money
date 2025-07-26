(ns clj-money.models.auth-helpers
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.util :as util]
            [clj-money.models :as models]
            [clj-money.models.grants :as grants]))

(defmulti ^:private fetch-entity util/model-type-dispatch)

(defn- fetch-entity*
  [model-or-ref]
  (if (util/model-ref? model-or-ref)
    (models/find model-or-ref :entity)
    model-or-ref))

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
  [{:attachment/keys [transaction transaction-date]}]
  (models/find-by
    (util/model-type
      {:transaction/_self transaction
       :transaction/transaction-date transaction-date}
      :entity)))

(defmethod fetch-entity :budget
  [{:budget/keys [entity]}]
  (fetch-entity* entity))

(defmethod fetch-entity :budget-item
  [budget-item]
  (models/find-by
    (util/model-type
      {:budget-item/_self budget-item}
      :entity)))

(defmethod fetch-entity :price
  [{:price/keys [commodity]}]
  (models/find-by
    (util/model-type
      {:commodity/_self commodity}
      :entity)))

(defmethod fetch-entity :reconciliation
  [{:reconciliation/keys [account]}]
  (models/find-by
    (util/model-type
      {:account/_self account}
      :entity)))

(defmethod fetch-entity :trade
  [{:trade/keys [entity]}]
  (fetch-entity* entity))

(defn user-granted-access?
  [resource entity user action]
  (when-let [g (models/find-by #:grant{:user user
                                       :entity entity})]
    (grants/has-permission? g
                            (util/model-type resource)
                            action)))

(defn owner-or-granted?
  [resource user action]
  (let [entity (fetch-entity resource)]
    (or (util/model= (:entity/user entity) user)
        (user-granted-access? resource entity user action))))
