(ns clj-money.serialization
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.util :refer [pprint-and-return]]
            [clj-money.io :refer [read-bytes]]
            [clj-money.models.helpers :refer [with-transacted-storage]]
            [clj-money.validation :as validation]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]
            [clj-money.models.lots :as lots]
            [clj-money.models.lot-transactions :as lot-transactions]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.attachments :as attachments]
            [clj-money.models.reconciliations :as reconciliations]
            [clj-money.models.images :as images]
            [clj-money.models.imports :as imports]))

(defn- throw-on-invalid
  [model]
  (if (validation/has-error? model)
    (throw (ex-info (format "Unable to create the model. %s"
                            (prn-str (validation/error-messages model)))
                    model))
    model))

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

(defn- resolve-transaction
  [context model]
  (let [trans-ref (:transaction-id model)]
    (assoc model :transaction-id (->> context
                                      :transactions
                                      (filter #(and
                                                 (= (:transaction-date %)
                                                    (:transaction-date trans-ref))
                                                 (= (:description %)
                                                    (:description trans-ref))))
                                      first
                                      :id))))

(defn- resolve-image
  [context model]
  (assoc model :image-id (->> context
                              :images
                              (filter #(= (:original-filenamme %)
                                          (:image-id model)))
                              first)))

(defn- create-attachments
  [storage context attachments]
  (mapv #(->> %
              (resolve-transaction context)
              (resolve-image context)
              (attachments/create storage))
        attachments))

(defn- realize-attachments
  [storage context]
  (update-in context [:attachments] #(create-attachments storage context %)))

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

(defn- create-commodities
  [storage context commodities]
  (mapv (fn [attributes]
          (->> attributes
               (resolve-entity context)
               (commodities/create storage)))
        commodities))

(defn- realize-commodities
  [storage context]
  (update-in context [:commodities] #(create-commodities storage context %)))

(defn- find-commodity
  [context ticker-symbol]
  (->> context
       :commodities
       (filter #(= (:symbol %) ticker-symbol))
       first))

(defn- resolve-commodity
  [context model]
  (update-in model [:commodity-id] #(:id (find-commodity context %))))

(defn- create-prices
  [storage context prices]
  (mapv (fn [attributes]
          (->> attributes
               (resolve-commodity context)
               (prices/create storage)))
        prices))

(defn- realize-prices
  [storage context]
  (update-in context [:prices] #(create-prices storage context %)))

(defn- create-lots
  [storage context lots]
  (mapv (fn [attributes]
          (->> attributes
               (resolve-commodity context)
               (resolve-account context)
               (lots/create storage)))
        lots))

(defn- realize-lots
  [storage context]
  (update-in context [:lots] #(create-lots storage context %)))

(defn- find-lot
  [context attr]
  (let [{:keys [account-id
                commodity-id
                purchase-date]} (->> attr
                                     (resolve-account context)
                                     (resolve-commodity context))]
    (->> context
         :lots
         (filter #(and (= account-id (:account-id %))
                       (= commodity-id (:commodity-id %))
                       (= purchase-date (:purchase-date %))))
         first)))

(defn- resolve-lot
  [context model]
  (update-in model [:lot-id] #(:id (find-lot context %))))

(defn- create-lot-transactions
  [storage context lot-transactions]
  (mapv (fn [attributes]
          (->> attributes
               (resolve-lot context)
               (lot-transactions/create storage)))
        lot-transactions))

(defn- realize-lot-transactions
  [storage context]
  (update-in context [:lot-transactions] #(create-lot-transactions storage context %)))

(defn- resolve-transaction-item-ids
  [context account-id items]
  (mapv (fn [{:keys [transaction-date amount]}]
          (or (->> context
               :transactions
               (filter #(= transaction-date (:transaction-date %)))
               (mapcat :items)
               (filter #(and (= account-id (:account-id %))
                             (= amount (:amount %))))
               (map :id)
               first)
              (throw (Exception. (format "Unable to find a transaction with date=%s, amount=%s"
                                         transaction-date
                                         amount)))))
        items))

(defn- resolve-reconciliation-transaction-item-ids
  [context reconciliation]
  (update-in reconciliation
             [:item-ids]
             #(resolve-transaction-item-ids context
                                            (:account-id reconciliation)
                                            %)))

(defn- create-reconciliations
  [storage context reconciliations]
  (mapv (fn [attributes]
          (->> attributes
               (resolve-account context)
               (resolve-reconciliation-transaction-item-ids context)
               (reconciliations/create storage)
               throw-on-invalid))
        reconciliations))

(defn- realize-reconciliations
  [storage context]
  (update-in context [:reconciliations] #(create-reconciliations storage context %)))

(defn- read-file
  [image]
  (update-in image [:body] #(read-bytes %)))

(defn- create-images
  [storage context images]
  (mapv (fn [attributes]
          (->> attributes
               (resolve-user context)
               read-file
               (images/create storage)
               throw-on-invalid))
        images))

(defn- realize-images
  [storage context]
  (update-in context [:images] #(create-images storage context %)))

(defn- find-image
  [context original-filename]
  (->> (:images context)
       (filter #(= original-filename (:original-filename %)))
       first))

(defn- resolve-image
  [context model]
  (update-in model [:image-id] #(:id (find-image context %))))

(defn- create-imports
  [storage context imports]
  (mapv (fn [attributes]
          (->> attributes
               (resolve-user context)
               (resolve-image context)
               (imports/create storage)
               throw-on-invalid))
        imports))

(defn- realize-imports
  [storage context]
  (update-in context [:imports] #(create-imports storage context %)))

(defn- get-commodity
  [context symbol]
  (->> context
       :commodities
       (filter #(= symbol (:symbol %)))
       first))

(defn- resolve-default-commodity-id
  [storage context entity]
  (if (-> entity :settings :default-commodity-id)
    (entities/update storage (update-in entity
                                        [:settings :default-commodity-id]
                                        #(:id (get-commodity context %))))
    entity))

(defn- resolve-default-commodity-ids
  [storage context]
  (update-in context
             [:entities]
             #(map (partial resolve-default-commodity-id storage context) %)))

(defn realize
  "Realizes a test context"
  [storage-spec input]
  (with-transacted-storage [s storage-spec]
  (->> input
      (realize-users s)
      (realize-images s)
      (realize-imports s)
      (realize-entities s)
      (realize-commodities s)
      (resolve-default-commodity-ids s)
      (realize-accounts s)
      (realize-prices s)
      (realize-lots s)
      (realize-lot-transactions s)
      (realize-budgets s)
      (realize-transactions s)
      (realize-attachments s)
      (realize-reconciliations s))))
