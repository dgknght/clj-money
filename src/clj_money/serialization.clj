(ns clj-money.serialization
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.transactions :as transactions]))

(defn- create-users
  [storage-spec users]
  (mapv (fn [attributes]
         (users/create storage-spec attributes))
       users))

(defn- realize-users
  [storage-spec context]
  (update-in context [:users] #(create-users storage-spec %)))

(defn- find-user
  [context email]
  (->> context
       :users
       (filter #(or (nil? email)
                    (= email (:email %))))
       first))

(defn- resolve-user
  [context model]
  (update-in model [:user-id] #(:id (find-user context %))))

(defn- create-entities
  [storage-spec context entities]
  (mapv (fn [attributes]
          (if (:id attributes)
            attributes
            (entities/create storage-spec (resolve-user context attributes))))
        entities))

(defn- realize-entities
  [storage-spec context]
  (update-in context [:entities] #(create-entities storage-spec context %)))

(defn- find-entity
  [context entity-name]
  (->> context
       :entities
       (filter #(or (nil? entity-name)
                    (= (:name %) entity-name)))
       first))

(defn- resolve-entity
  [context model]
  (update-in model [:entity-id] #(:id (find-entity context %))))

(defn- resolve-parent
  [storage-spec account]
  (if (:parent-id account)
    (let [parent (accounts/find-by-name storage-spec (:entity-id account) (:parent-id account))]
      (assoc account :parent-id (:id parent)))
    account))

(defn- create-account
  [storage-spec context attributes]
  (if (:id attributes)
    attributes
    (accounts/create storage-spec (->> attributes
                                       (resolve-entity context)
                                       (resolve-parent storage-spec)))))

(defn- create-accounts
  "Creates the specified accounts.
  
  Accounts can be a sequence of maps containing account properties,
  or a map where the keys are account types and the values
  are vectors of account properties"
  [storage-spec context accounts]
  (let [account-list (if (map? accounts)
                       (mapcat (fn [[account-type acct-list]]
                                 (map #(assoc % :type account-type)
                                      acct-list))
                               accounts)
                       accounts)]
    (mapv #(create-account storage-spec context %) account-list)))

(defn- realize-accounts
  [storage-spec context]
  (update-in context [:accounts] #(create-accounts storage-spec context %)))

(defn- find-account
  [context account-name]
  (->> context
       :accounts
       (filter #(= account-name (:name %)))
       first))

(defn- resolve-account
  [context model]
  (update-in model [:account-id] #(:id (find-account context %))))

(defn- coerce-amount
  [item]
  (update-in item [:amount] bigdec))

(defn- prepare-item
  [context item]
  (->> item
       (resolve-account context)
       coerce-amount))

(defn- prepare-items
  [context transaction]
  (update-in transaction [:items] (fn [items]
                                    (map #(prepare-item context %)
                                         items))))

(defn- create-transactions
  [storage-spec context transactions]
  []
  (mapv (fn [attributes]
         (transactions/create storage-spec (->> attributes
                                            (resolve-entity context)
                                            (prepare-items context))))
       transactions))

(defn- realize-transactions
  [storage-spec context]
  (update-in context [:transactions] #(create-transactions storage-spec context %)))

(defn- append-budget-items
  [storage-spec context items budget]
  (assoc budget :items (->> items
                            (map #(resolve-account context %))
                            (mapv #(assoc % :budget-id (:id budget)))
                            (mapv #(budgets/create-item storage-spec %)))))

(defn- create-budgets
  [storage-spec context budgets]
  (mapv (fn [attributes]
          (->> attributes
               (resolve-entity context)
               (budgets/create storage-spec)
               (append-budget-items storage-spec context (:items attributes))))
        budgets))

(defn- realize-budgets
  [storage-spec context]
  (update-in context [:budgets] #(create-budgets storage-spec context %)))

(defn realize
  "Realizes a test context"
  [storage-spec input]
  (->> input
      (realize-users storage-spec)
      (realize-entities storage-spec)
      (realize-accounts storage-spec)
      (realize-budgets storage-spec)
      (realize-transactions storage-spec)))
