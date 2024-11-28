(ns clj-money.models.auth-helpers
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.util :as util]
            [clj-money.db :as db]
            [clj-money.models :as models]
            [clj-money.models.grants :as grants]))

(defmulti ^:private fetch-entity db/type-dispatch)

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

(defmethod fetch-entity :attachment
  [{:attachment/keys [transaction transaction-date]}]
  (models/find-by
    (db/model-type
      {:transaction/id (:id transaction)
       :transaction/transaction-date transaction-date}
      :entity)))

(defmethod fetch-entity :budget
  [{:budget/keys [entity]}]
  (fetch-entity* entity))

(defmethod fetch-entity :budget-item
  [{:budget-item/keys [budget]}]
  (models/find-by
    (db/model-type
      {:budget/id (:id budget)}
      :entity)))

(defmethod fetch-entity :price
  [{:price/keys [commodity]}]
  (models/find-by
    (db/model-type
      {:commodity/id (:id commodity)}
      :entity)))

(defmethod fetch-entity :reconciliation
  [{:reconciliation/keys [account]}]
  (models/find-by
    (db/model-type
      {:account/id (:id account)}
      :entity)))

(defn user-granted-access?
  [resource entity user action]
  (when-let [g (models/find-by #:grant{:user user
                                       :entity entity})]
    (grants/has-permission? g
                            (db/model-type resource)
                            action)))

(defn owner-or-granted?
  [resource user action]
  (let [entity (fetch-entity resource)]
    (or (util/model= (:entity/user entity) user)
        (user-granted-access? resource entity user action))))
