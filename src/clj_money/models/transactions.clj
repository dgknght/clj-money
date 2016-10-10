(ns clj-money.models.transactions
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clj-time.coerce :as tc]
            [schema.core :as schema]
            [clj-money.validation :as validation]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.helpers :refer [storage]]
            [clj-money.models.storage :refer [create-transaction
                                              create-transaction-item
                                              find-transaction-by-id
                                              find-transaction-item-by-index
                                              find-transaction-item-preceding-date
                                              select-transaction-items-by-transaction-id]])
  (:import java.util.Date
           org.joda.time.LocalDate))

(def NewTransaction
  {:entity-id schema/Int
   :transaction-date LocalDate
   :items [{:account-id schema/Int
            :action (schema/enum :debit :credit)
            :amount BigDecimal
            :balance BigDecimal
            (schema/optional-key :next-item-id) schema/Int
            (schema/optional-key :previous-item-id) schema/Int}]})

(defn- before-save-item
  "Makes pre-save adjustments for a transaction item"
  [item]
  (update-in item [:action] name))

(defn- prepare-item-for-return
  "Makes adjustments to a transaction item in prepartion for return
  from the data store"
  [item]
  (if (map? item)
    (update-in item [:action] keyword)
    item))

(defn- item-amount-must-be-greater-than-zero
  [transaction]
  (let [errors (map #(when (> 0 (:amount %))
                       "Amout must be greater than zero")
                    (:items transaction))]
    {:model transaction
     :errors (if (seq (filter identity errors))
               [[:items errors]]
               [])}))

(defn- item-amount-sum
  "Returns the sum of items in the transaction having
  the specified action"
  [transaction action]
  (reduce + 0 (->> (:items transaction)

                   (filter #(= action (:action %)))
                   (map :amount))))

(defn- sum-of-credits-must-equal-sum-of-debits
  [transaction]
  (let [[debits credits] (map #(item-amount-sum transaction %)
                              [:debit :credit])]
    {:model transaction
     :errors (if (= debits credits)
               []
               [[:items
                 (str "The total debits (" debits ") does not match the total credits (" credits ")")]])}))

(defn- validation-rules
  [schema]
  [(partial validation/apply-schema schema)
   item-amount-must-be-greater-than-zero
   sum-of-credits-must-equal-sum-of-debits])

(defn- before-validation
  "Performs operations required before validation"
  [transaction]
  (update-in transaction [:items] (fn [items]
                                    (map #(assoc % :balance (bigdec 0)) items))))

(defn- before-save
  "Returns a transaction ready for insertion into the
  database"
  [transaction]
  (-> transaction
      (dissoc :items)
      (update-in [:transaction-date] tc/to-sql-date)))

(defn- prepare-for-return
  "Returns a transaction that is ready for public use"
  [transaction]
  (update-in [:transaction-date] tc/to-local-date))

(defn- get-previous-item
  "Finds the transaction item that immediately precedes the specified item"
  [storage item transaction-date]
  (if-let [index (:index item)]
    (if (= 0 index)
      nil
      (find-transaction-item-by-index storage
                                      (:account-id item)
                                      (- index 1)))
    (find-transaction-item-preceding-date storage
                                          (:account-id item)
                                          transaction-date)))

(defn- update-balances
  "Updates transaction item and account balances resulting from the
  specified transaction.
  
  The balance for each transaction item is the sum of the balance of
  the previous transaction item and the polarized transaction amount.
  
  The polarized transaction amount is the positive or negative change
  on the balance of the associated account and is dependant on the action
  (debit or credit) and the account type (asset, liability, equity,
  income, or expense)"
  [storage transaction]
  (update-in transaction
             [:items]
             #(map (fn [item]
                     (let [previous-item (get-previous-item storage item (:transaction-date transaction))
                           previous-balance (or (get previous-item :balance)
                                                (bigdec 0))
                           next-index (+ 1 (or (get previous-item :index) -1))
                           polarized-amount (accounts/polarize-amount storage item)]
                       (-> item
                           (assoc :balance (+ previous-balance
                                              polarized-amount)
                                  :index next-index)))) %)))

(defn create
  "Creates a new transaction"
  [storage-spec transaction]
  (let [validated (-> (before-validation transaction)
                      (validation/validate-model (validation-rules NewTransaction)))]
    (if (validation/has-error? validated)
      validated
      (let [storage (storage storage-spec)
            with-balances (update-balances storage validated)
            result (create-transaction storage (before-save with-balances))
            items (into [] (map #(->> (assoc % :transaction-id (:id result))
                                      before-save-item
                                      (create-transaction-item storage)
                                      prepare-item-for-return)
                                (:items with-balances)))]
        (assoc result :items items)))))

(defn find-by-id
  "Returns the specified transaction"
  [storage-spec id]
  (let [storage (storage storage-spec)]
    (-> (find-transaction-by-id storage id)
        (assoc :items (select-transaction-items-by-transaction-id storage id)))))
