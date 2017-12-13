(ns clj-money.serialization
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.util :refer [pprint-and-return]]
            [clj-money.io :refer [read-bytes]]
            [clj-money.models.helpers :refer [with-storage]]
            [clj-money.validation :as validation]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.grants :as grants]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]
            [clj-money.models.lots :as lots]
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
  [context storage]
  (update-in context [:users] #(create-users storage %)))

(defn- find-user
  [context email]
  (->> context
       :users
       (filter #(or (nil? email)
                    (= email (:email %))))
       first))

(defn- resolve-user
  [model context]
  (update-in model [:user-id] #(:id (find-user context %))))

(defn- create-entities
  [storage context entities]
  (mapv (fn [attributes]
          (if (:id attributes)
            attributes
            (entities/create storage (resolve-user attributes context))))
        entities))

(defn- realize-entities
  [context storage]
  (update-in context [:entities] #(create-entities storage context %)))

(defn- find-entity
  [context entity-name]
  (->> (:entities context)
       (filter #(= (:name %) entity-name))
       first))

(defn- resolve-entity
  [model context]
  (update-in model [:entity-id] (fn [entity-name]
                                  (if entity-name
                                    (:id (find-entity context entity-name))
                                    (-> context :entities first :id)))))

(defn create-grants
  [storage context grants]
  (mapv (fn [attributes]
          (grants/create storage (-> attributes
                                     (resolve-user context)
                                     (resolve-entity context))))
        grants))

(defn- realize-grants
  [context storage]
  (update-in context [:grants] #(create-grants storage context %)))

(defn- resolve-parent
  [account storage]
  (if (:parent-id account)
    (let [parent (accounts/find-by-name storage (:entity-id account) (:parent-id account))]
      (assoc account :parent-id (:id parent)))
    account))

(defn- find-commodity
  [context ticker-symbol]
  (->> context
       :commodities
       (filter #(= (:symbol %) ticker-symbol))
       first))

(defn- resolve-commodity
  [model context]
  (update-in model [:commodity-id] (fn [ticker-symbol]
                                     (if ticker-symbol
                                       (:id (find-commodity context ticker-symbol))
                                       (->> (:commodities context)
                                            (filter #(= :currecy (:type %)))
                                            first)))))

(defn- create-account
  [storage context attributes]
  (if (:id attributes)
    attributes
    (accounts/create storage (-> attributes
                                 (resolve-entity context)
                                 (resolve-commodity context)
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
  [context storage]
  (update-in context [:accounts] #(create-accounts storage context %)))

(defn- find-account
  [context account-name]
  (->> context
       :accounts
       (filter #(= account-name (:name %)))
       first))

(defn- resolve-account
  [model context]
  (update-in model [:account-id] #(:id (find-account context %))))

(defn- coerce-amount
  [item]
  (update-in item [:amount] bigdec))

(defn- prepare-item
  [context item]
  (-> item
      (resolve-account context)
      coerce-amount))

(defn- prepare-items
  [transaction context]
  (update-in transaction [:items] (fn [items]
                                    (map #(prepare-item context %)
                                         items))))

(defn- create-transactions
  [storage context transactions]
  []
  (mapv (fn [attributes]
          (transactions/create storage (-> attributes
                                           (resolve-entity context)
                                           (prepare-items context))))
        transactions))

(defn- realize-transactions
  [context storage]
  (update-in context [:transactions] #(create-transactions storage context %)))

(defn- resolve-transaction
  [model context]
  (let [trans-ref (:transaction-id model)]
    (assoc model :transaction-id (->> context
                                      :transactions
                                      (filter #(and
                                                 (= (:transaction-date %)
                                                    (:transaction-date trans-ref))
                                                 (= (:description %)
                                                    (:description trans-ref))))
                                      (map (juxt :id :transaction-date))
                                      first))))

(defn- resolve-image
  [model context]
  (assoc model :image-id (->> context
                              :images
                              (filter #(= (:original-filenamme %)
                                          (:image-id model)))
                              first)))

(defn- create-attachment
  [model storage]
  (attachments/create storage model))

(defn- rearrange-transaction-attributes
  [attachment]
  (assoc attachment :transaction-id (-> attachment :transaction-id first)
                    :transaction-date (-> attachment :transaction-id second)))

(defn- create-attachments
  [storage context attachments]
  (mapv #(-> %
             (resolve-transaction context)
             rearrange-transaction-attributes
             (resolve-image context)
             (create-attachment storage)
             throw-on-invalid)
        attachments))

(defn- realize-attachments
  [context storage]
  (update-in context [:attachments] #(create-attachments storage context %)))

(defn- append-budget-items
  [budget storage context items]
  (assoc budget :items (->> items
                            (map #(resolve-account % context))
                            (map #(assoc % :budget-id (:id budget)))
                            (mapv #(budgets/create-item storage %)))))

(defn- create-budget
  [model storage]
  (budgets/create storage model))

(defn- create-budgets
  [storage context budgets]
  (mapv (fn [attributes]
          (-> attributes
              (resolve-entity context)
              (create-budget storage)
              (append-budget-items storage context (:items attributes))))
        budgets))

(defn- realize-budgets
  [context storage]
  (update-in context [:budgets] #(create-budgets storage context %)))

(defn- create-commodity
  [model storage]
  (commodities/create storage model))

(defn- create-commodities
  [storage context commodities]
  (mapv (fn [attributes]
          (-> attributes
              (resolve-entity context)
              (create-commodity storage)))
        commodities))

(defn- realize-commodities
  [context storage]
  (update-in context [:commodities] #(create-commodities storage context %)))

(defn- create-price
  [model storage]
  (prices/create storage model))

(defn- create-prices
  [storage context prices]
  (mapv (fn [attributes]
          (-> attributes
              (resolve-commodity context)
              (create-price storage)))
        prices))

(defn- realize-prices
  [context storage]
  (update-in context [:prices] #(create-prices storage context %)))

(defn- create-lot
  [model storage]
  (lots/create storage model))

(defn- create-lots
  [storage context lots]
  (mapv (fn [attributes]
          (-> attributes
              (resolve-commodity context)
              (resolve-account context)
              (create-lot storage)))
        lots))

(defn- realize-lots
  [context storage]
  (update-in context [:lots] #(create-lots storage context %)))

(defn- find-lot
  [context attr]
  (let [{:keys [account-id
                commodity-id
                purchase-date]} (-> attr
                                    (resolve-account context)
                                    (resolve-commodity context))]
    (->> context
         :lots
         (filter #(and (= account-id (:account-id %))
                       (= commodity-id (:commodity-id %))
                       (= purchase-date (:purchase-date %))))
         first)))

(defn- resolve-lot
  [model context]
  (update-in model [:lot-id] #(:id (find-lot context %))))

(defn- resolve-transaction-item-ids
  [context account-id items]
  (mapv (fn [{:keys [transaction-date amount]}]
          (or (->> context
               :transactions
               (filter #(= transaction-date (:transaction-date %)))
               (mapcat :items)
               (filter #(and (= account-id (:account-id %))
                             (= amount (:amount %))))
               (map (juxt :id :transaction-date))
               first)
              (throw (Exception. (format "Unable to find a transaction with date=%s, amount=%s"
                                         transaction-date
                                         amount)))))
        items))

(defn- resolve-reconciliation-transaction-item-ids
  [reconciliation context]
  (update-in reconciliation
             [:item-ids]
             #(resolve-transaction-item-ids context
                                            (:account-id reconciliation)
                                            %)))

(defn- create-reconciliation
  [model storage]
  (reconciliations/create storage model))

(defn- create-reconciliations
  [storage context reconciliations]
  (mapv (fn [attributes]
          (-> attributes
              (resolve-account context)
              (resolve-reconciliation-transaction-item-ids context)
              (create-reconciliation storage)
              throw-on-invalid))
        reconciliations))

(defn- realize-reconciliations
  [context storage]
  (update-in context [:reconciliations] #(create-reconciliations storage context %)))

(defn- read-file
  [image]
  (update-in image [:body] #(read-bytes %)))

(defn- create-image-from-file
  [model storage]
  (images/create storage (read-file model)))

(defn- create-images
  [storage context images]
  (mapv (fn [attributes]
          (-> attributes
              (resolve-user context)
              (create-image-from-file storage)
              throw-on-invalid))
        images))

(defn- realize-images
  [context storage]
  (update-in context [:images] #(create-images storage context %)))

(defn- find-image
  [context original-filename]
  (->> (:images context)
       (filter #(= original-filename (:original-filename %)))
       first))

(defn- resolve-image
  [model context]
  (update-in model [:image-id] #(:id (find-image context %))))

(defn- create-import
  [model storage]
  (imports/create storage model))

(defn- create-imports
  [storage context imports]
  (mapv (fn [attributes]
          (-> attributes
              (resolve-user context)
              (resolve-image context)
              (create-import storage)
              throw-on-invalid))
        imports))

(defn- realize-imports
  [context storage]
  (update-in context [:imports] #(create-imports storage context %)))

(defn- get-commodity
  [context symbol]
  (->> context
       :commodities
       (filter #(= symbol (:symbol %)))
       first))

(defn- resolve-default-commodity-id
  [storage context entity]
  (if-let [symbol (-> entity :settings :default-commodity-id)]
    (entities/update storage (assoc-in entity
                                       [:settings :default-commodity-id]
                                       (:id (get-commodity context symbol))))
    entity))

(defn- resolve-default-commodity-ids
  [context storage]
  (update-in context
             [:entities]
             #(map (partial resolve-default-commodity-id storage context) %)))

(defn realize
  "Realizes a test context"
  [storage-spec input]
  (with-storage [s storage-spec]
    (-> input
        (realize-users s)
        (realize-images s)
        (realize-imports s)
        (realize-entities s)
        (realize-grants s)
        (realize-commodities s)
        (resolve-default-commodity-ids s)
        (realize-accounts s)
        (realize-prices s)
        (realize-lots s)
        (realize-budgets s)
        (realize-transactions s)
        (realize-attachments s)
        (realize-reconciliations s))))
