(ns clj-money.entities.auth-helpers
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.entities.grants :as grants]
            [clj-money.authorization :as-alias authorization]
            [clj-money.authorization.scheduled-transactions :as-alias scheduled-transactions]))

(defmulti ^:private fetch-entity util/entity-type-dispatch)

(def ^:private fetch-entity* entities/find)

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

(defmethod fetch-entity :receipt-ingestion
  [{:receipt-ingestion/keys [entity]}]
  (fetch-entity* entity))

(defmethod fetch-entity :statement-ingestion
  [{:statement-ingestion/keys [entity]}]
  (fetch-entity* entity))

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

(defmethod fetch-entity :lot
  [{:lot/keys [account]}]
  (entities/find-by
    (util/entity-type
      {:account/_self account}
      :entity)))

(defmethod fetch-entity :transaction-item
  [{:transaction-item/keys [account]}]
  (entities/find-by
    (util/entity-type
      {:account/_self account}
      :entity)))

(defmethod fetch-entity :lot-note
  [{:lot-note/keys [lots]}]
  (fetch-entity (entities/find (first lots))))

; A grant's permissions hold plain actions (see grants/actions)
(def ^:private grant-actions
  {::authorization/index :index
   ::authorization/show :show
   ::authorization/create :create
   ::authorization/update :update
   ::authorization/destroy :delete
   ::authorization/recalculate :update
   ::scheduled-transactions/realize :create})

; Resource types that can't be granted on their own (see
; grants/resource-types) take the permissions of the type they belong to
(def ^:private grant-resource-types
  {:transaction-item :transaction
   :scheduled-transaction :transaction
   :lot :account
   :lot-note :account
   :trade :account
   :receipt-ingestion :attachment
   :statement-ingestion :attachment})

(defn- grant-resource-type
  [resource-type]
  (get grant-resource-types resource-type resource-type))

(defn user-granted-access?
  [resource entity user action]
  (when-let [g (entities/find-by #:grant{:user user
                                       :entity entity})]
    (some->> (grant-actions action)
             (grants/has-permission? g (grant-resource-type
                                         (util/entity-type resource))))))

(defn- scope-entities
  "Returns references to the entities the user owns, and those the user
  holds a grant on that permits the action on resources of the type or,
  for ::authorization/manage, any action on them"
  [user resource-type action]
  (let [permitted? (if (= ::authorization/manage action)
                     seq
                     #(contains? % (grant-actions action)))
        grant-type (grant-resource-type resource-type)]
    (->> (entities/select #:grant{:user user})
         (filter #(permitted? (get-in % [:grant/permissions grant-type])))
         (map :grant/entity)
         (concat (entities/select #:entity{:user user}))
         (mapv util/->entity-ref))))

(defn entity-scope
  "Returns criteria limiting resources of the type to the entities the user
  can reach them in for the action (see authorization/scope), where attr
  refers from the resource, or a resource joined to it, to the entity, or
  is :id, for the entities themselves"
  [resource-type attr user action]
  (let [entities (scope-entities user resource-type action)]
    (util/entity-type (cond
                        ; SQL can't query IN (), so match the entities
                        ; the user owns, of which there are none
                        (empty? entities) {:entity/user user}
                        (= :id attr) {:id [:in (mapv :id entities)]}
                        :else {attr [:in entities]})
                      resource-type)))

(defn owner-or-granted?
  [resource user action]
  (let [entity (fetch-entity resource)]
    (or (util/entity= (:entity/user entity) user)
        (user-granted-access? resource entity user action))))
