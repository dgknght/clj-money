(ns clj-money.serialization
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.util :refer [pprint-and-return]]
            [clj-money.models.helpers :refer [with-transacted-storage]]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.reconciliations :as reconciliations]))

(defn- create-users
  [storage users]
  (mapv (fn [attributes]
         (users/create storage attributes))
       users))

(defn- realize-users
  [storage context]
  (update-in context [:users] #(create-users storage %)))

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
  [storage context entities]
  (mapv (fn [attributes]
          (if (:id attributes)
            attributes
            (entities/create storage (resolve-user context attributes))))
        entities))

(defn- realize-entities
  [storage context]
  (update-in context [:entities] #(create-entities storage context %)))

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
  [storage account]
  (if (:parent-id account)
    (let [parent (accounts/find-by-name storage (:entity-id account) (:parent-id account))]
      (assoc account :parent-id (:id parent)))
    account))

(defn- create-account
  [storage context attributes]
  (if (:id attributes)
    attributes
    (accounts/create storage (->> attributes
                                       (resolve-entity context)
                                       (resolve-parent storage)))))

(defn- create-accounts
  "Creates the specified accounts.
  
  Accounts can be a sequence of maps containing account properties,
  or a map where the keys are account types and the values
  are vectors of account properties"
  [storage context accounts]
  (let [account-list (if (map? accounts)
                       (mapcat (fn [[account-type acct-list]]
                                 (map #(assoc % :type account-type)
                                      acct-list))
                               accounts)
                       accounts)]
    (mapv #(create-account storage context %) account-list)))

(defn- realize-accounts
  [storage context]
  (update-in context [:accounts] #(create-accounts storage context %)))

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
  [storage context transactions]
  []
  (mapv (fn [attributes]
         (transactions/create storage (->> attributes
                                            (resolve-entity context)
                                            (prepare-items context))))
       transactions))

(defn- realize-transactions
  [storage context]
  (update-in context [:transactions] #(create-transactions storage context %)))

(defn- append-budget-items
  [storage context items budget]
  (assoc budget :items (->> items
                            (map #(resolve-account context %))
                            (map #(assoc % :budget-id (:id budget)))
                            (mapv #(budgets/create-item storage %)))))

(defn- create-budgets
  [storage context budgets]
  (mapv (fn [attributes]
          (->> attributes
               (resolve-entity context)
               (budgets/create storage)
               (append-budget-items storage context (:items attributes))))
        budgets))

(defn- realize-budgets
  [storage context]
  (update-in context [:budgets] #(create-budgets storage context %)))

(defn- resolve-transaction-item-ids
  [context items]
  (->> items
       (map #(resolve-account context %))
       (mapv (fn [{:keys [transaction-date account-id amount]}]
               (->> context
                    :transactions
                    (filter #(= transaction-date (:transaction-date %)))
                    (mapcat :items)
                    (filter #(and (= account-id (:account-id %))
                                  (= amount (:amount %))))
                    (map :id)
                    first)))))

(defn- resolve-reconciliation-transaction-item-ids
  [context reconciliation]
  (update-in reconciliation [:item-ids] #(resolve-transaction-item-ids context %)))

(defn- create-reconciliations
  [storage context reconciliations]
  (mapv (fn [attributes]
          (->> attributes
               (resolve-account context)
               (resolve-reconciliation-transaction-item-ids context)
               (reconciliations/create storage)))
        reconciliations))

(defn- realize-reconciliations
  [storage context]
  (update-in context [:reconciliations] #(create-reconciliations storage context %)))

(defn realize
  "Realizes a test context"
  [storage-spec input]
  (with-transacted-storage [s storage-spec]
  (->> input
      (realize-users s)
      (realize-entities s)
      (realize-accounts s)
      (realize-budgets s)
      (realize-transactions s)
      (realize-reconciliations s))))
