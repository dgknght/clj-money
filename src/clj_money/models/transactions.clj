(ns clj-money.models.transactions
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [difference]]
            [clj-time.coerce :as tc]
            [schema.core :as schema]
            [clj-money.validation :as validation]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.helpers :refer [storage]]
            [clj-money.models.storage :refer [select-transactions-by-entity-id
                                              create-transaction
                                              create-transaction-item
                                              find-transaction-by-id
                                              update-transaction
                                              find-transaction-item-by-id
                                              find-transaction-items-preceding-date
                                              find-last-transaction-item-on-or-before
                                              select-transaction-items-by-account-id
                                              select-transaction-items-by-account-id-and-starting-index
                                              select-transaction-items-by-account-id-on-or-after-date
                                              select-transaction-items-by-transaction-id
                                              update-transaction-item
                                              update-transaction-item-index-and-balance
                                              delete-transaction
                                              delete-transaction-item
                                              delete-transaction-items-by-transaction-id]])
  (:import java.util.Date
           org.joda.time.LocalDate))

(def BaseTransaction
  {:description schema/Str
   :transaction-date LocalDate
   :items [{:account-id schema/Int
            :action (schema/enum :debit :credit)
            :amount BigDecimal
            :balance BigDecimal
            (schema/optional-key :next-item-id) schema/Int
            (schema/optional-key :previous-item-id) schema/Int}]})

(def NewTransaction
  (merge BaseTransaction
         {:entity-id schema/Int}))

(def Transaction
  (-> BaseTransaction
      (assoc :id schema/Int
             (schema/optional-key :entity-id) schema/Any
             (schema/optional-key :updated-at) schema/Any
             (schema/optional-key :created-at) schema/Any)
      (assoc-in [:items 0 (schema/optional-key :updated-at)] schema/Any)
      (assoc-in [:items 0 (schema/optional-key :created-at)] schema/Any)
      (assoc-in [:items 0 (schema/optional-key :index)] schema/Any)
      (assoc-in [:items 0 (schema/optional-key :transaction-id)] schema/Int)
      (assoc-in [:items 0 (schema/optional-key :id)] schema/Int)))

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

(defn- items-must-be-present
  [{:keys [items] :as transaction}]
  {:model transaction
   :errors (if (and items (<= 2 (count items)))
             []
             [[:items
               "The transaction must have at least two items"]])})

(defn- validation-rules
  [schema]
  [(partial validation/apply-schema schema)
   items-must-be-present
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
      (update-in [:transaction-date] tc/to-long)))

(defn- prepare-for-return
  "Returns a transaction that is ready for public use"
  [transaction]
  (update-in transaction [:transaction-date] tc/to-local-date))

(defn- get-previous-item
  "Finds the transaction item that immediately precedes the specified item"
  [storage item transaction-date]
  (->> (find-transaction-items-preceding-date storage
                                              (:account-id item)
                                              (tc/to-long transaction-date))
       (remove #(= (:id %) (:id item)))
       first))

(defn- process-item-balance-and-index
  "Accepts a context containing
    :previous-index   - The index of the previous item
    :previous-balance - The balance of the previous item
    :items            - The items processed so far
    :storage          - Storage service

  Returns the context with these updates
    :previous-index   - updated to the index of the item just processed
    :previous-balance - updated to the balance of the item just processed
    :items            - appended with the item just processed
    :storage          - Unchanged storage service"
  [context item]
  (let [next-index (+ 1 (:previous-index context))
        polarized-amount (accounts/polarize-amount (:storage context) item)
        next-balance (+ (:previous-balance context) polarized-amount)
        updated-item (-> item
                         (assoc :balance next-balance
                                :index next-index))]
    (-> context
        (assoc :previous-index next-index
               :previous-balance next-balance)
        (update-in [:items] #(conj % updated-item)))))

(defn- add-item-balance-and-index
  "Updates the specified items, which belong to the specified account
  with new balance and index values"
  [storage transaction-date [account-id items]]
  (let [sorted-items (sort-by :index items)
        previous-item (get-previous-item storage (first sorted-items) transaction-date)
        previous-balance (or (get previous-item :balance)
                             (bigdec 0))
        previous-index (or (get previous-item :index) -1)]
    (->> sorted-items
         (reduce process-item-balance-and-index
                 {:previous-index previous-index
                  :previous-balance previous-balance
                  :items []
                  :storage storage})
         :items)))

(defn- calculate-balances-and-indexes
  "Updates transaction item and account balances resulting from the
  specified transaction.
  
  The balance for each transaction item is the sum of the balance of
  the previous transaction item and the polarized transaction amount.
  
  The polarized transaction amount is the positive or negative change
  on the balance of the associated account and is dependant on the action
  (debit or credit) and the account type (asset, liability, equity,
  income, or expense)"
  [storage transaction-date items]
  (->> items
       (group-by :account-id)
       (mapcat (partial add-item-balance-and-index
                     storage
                     transaction-date))))

(defn- subsequent-items
  "Returns items in the specified account on or after the specified
  transaction date, excluding the reference-item"
  ([storage-spec reference-item]
   (->> (select-transaction-items-by-account-id-and-starting-index
          (storage storage-spec)
          (:account-id reference-item)
          (:index reference-item))
        (remove #(= (:id reference-item) (:id %)))
        (map prepare-item-for-return)))
  ([storage-spec reference-item transaction-date]
   (->> (select-transaction-items-by-account-id-on-or-after-date
          (storage storage-spec)
          (:account-id reference-item)
          (tc/to-long transaction-date))
        (remove #(= (:id reference-item) (:id %)))
        (map prepare-item-for-return))))

(defn- upsert-item
  "Updates the specified transaction item"
  [storage item]
  (if (:id item)
    (do
      (update-transaction-item storage item)
      (find-transaction-item-by-id storage (:id item)))
    (create-transaction-item storage item)))

(defn- update-item-index-and-balance
  "Updates only the index and balance attributes of an item, returning true if
  the values where changed as a result of the update, or false if the specified
  values match the existing values"
  [storage-spec item]
  (let [records-affected (first (update-transaction-item-index-and-balance
                                  (storage storage-spec)
                                  item))]
    (> records-affected 0)))

(defn- calculate-item-index-and-balance
  "Accepts a transaction item and a context containing
    :index   - a transaction item index
    :balance - a transaction item balance
    :storage - service used to communicate with the data store

  Calculates the new index and balance for the item and updates
  them in the data store

  Returns a context containing
    :index   - the new index
    :balance - the new balance
    :storage - unchanged

  This is done in the context of updating existing transaction
  items that are affected by a new or updated transaction"
  [{:keys [index balance storage]} item]
  (let [new-index (+ 1 index)
        polarized-amount (accounts/polarize-amount storage item)
        new-balance (+ balance polarized-amount)
        value-changed (update-item-index-and-balance storage (-> item
                                                                 (assoc :index new-index
                                                                        :balance new-balance)
                                                                 before-save-item))
        result {:index new-index
                :balance new-balance
                :storage storage}]
    (if value-changed
      result
      (-> result
          (assoc ::skip-account-update true)
          reduced))))

(defn- update-affected-balances
  "Updates transaction items and corresponding accounts that
  succeed the specified items"
  ([storage-spec items] (update-affected-balances storage-spec items nil))
  ([storage-spec items transaction-date]
   (doseq [[account-id items] (group-by :account-id items)]
     (let [last-item (last items) ; these should already be sorted
           subsequent-items (if transaction-date
                              (subsequent-items storage-spec last-item transaction-date)
                              (subsequent-items storage-spec last-item))
           final (reduce calculate-item-index-and-balance
                         (assoc last-item :storage storage-spec)
                         subsequent-items)]
       (when-not (::skip-account-update final)
         (accounts/update storage-spec {:id account-id
                                        :balance (:balance final)}))))))

(defn- validate
  [schema transaction]
  (-> (before-validation transaction)
      (validation/validate-model (validation-rules schema))))

(defn- append-items
  [storage transaction]
  (assoc transaction
         :items
         (->> (:id transaction)
              (select-transaction-items-by-transaction-id storage)
              (map prepare-item-for-return))))

(defn select-by-entity-id
  "Returns the transactions that belong to the specified entity"
  [storage-spec entity-id]
  (let [storage (storage storage-spec)]
    (->>
      (select-transactions-by-entity-id storage entity-id)
      (map prepare-for-return)
      (map #(append-items storage %)))))

(defn create
  "Creates a new transaction"
  [storage-spec transaction]
  (let [validated (validate NewTransaction transaction)]
    (if (validation/has-error? validated)
      validated
      (let [storage (storage storage-spec)
            items-with-balances (calculate-balances-and-indexes storage
                                                                (:transaction-date validated)
                                                                (:items validated))
            _ (update-affected-balances storage-spec items-with-balances (:transaction-date validated))
            result (->> (assoc validated :items items-with-balances)
                        before-save
                        (create-transaction storage)
                        prepare-for-return)
            items (into [] (map #(->> (assoc % :transaction-id (:id result))
                                      before-save-item
                                      (create-transaction-item storage)
                                      prepare-item-for-return)
                                items-with-balances))]
        (assoc result :items items)))))

(defn find-by-id
  "Returns the specified transaction"
  [storage-spec id]
  (let [storage (storage storage-spec)]
    (->> (find-transaction-by-id storage id)
        prepare-for-return
        (append-items storage))))

(defn items-by-account
  "Returns the transaction items for the specified account"
  [storage-spec account-id]
  (select-transaction-items-by-account-id (storage storage-spec) account-id))

(defn- process-item-upserts
  "Process items in a transaction update operation"
  [storage items]
  (->> items
       (map before-save-item)
       (mapv #(upsert-item storage %))))

(defn reload
  "Returns an updated copy of the transaction"
  [storage-spec transaction]
  (find-by-id storage-spec (:id transaction)))

(defn- process-removals
  "Given a transaction being updated, deletes an transaction
  items that are no longer present and returns a list of base
  items to be used to propagate updates for accounts there were
  referenced in the transaction but no longer are"
  [storage transaction]
  (let [existing-trans (reload storage transaction)
        removed-item-ids (apply difference
                                (map #(->> (:items %)
                                           (map :id)
                                           (into #{}))
                                     [existing-trans transaction]))
        dereferenced-account-ids (apply difference
                                        (map #(->> (:items %)
                                                   (map :account-id)
                                                   (into #{}))
                                             [existing-trans transaction]))]
    (doseq [id removed-item-ids]
      (delete-transaction-item storage id))
    (->> dereferenced-account-ids
         ; fake out an item because that's what get-previous-item expects
         (map #(hash-map :account-id % :id -1))
         (map #(or (get-previous-item storage % (:transaction-date transaction))
                   (assoc % :index -1 :balance (bigdec 0)))))))

(defn update
  "Updates the specified transaction"
  [storage-spec transaction]
  (let [storage (storage storage-spec)
        validated (validate Transaction transaction)]
    (if (validation/has-error? validated)
      validated
      (let [dereferenced-base-items (process-removals
                                      storage
                                      validated)
            upserted-items (->> (:items validated)
                                (calculate-balances-and-indexes
                                  storage
                                  (:transaction-date validated))
                                ; new items need the transaction-id added
                                (map #(assoc % :transaction-id (:id transaction)))
                                (process-item-upserts storage))]
        (->> validated
             before-save
             (update-transaction storage))
        (update-affected-balances storage
                                  (concat upserted-items
                                          dereferenced-base-items)
                                  (:transaction-date validated))
        (reload storage validated)))))

(defn- get-preceding-items
  "Returns the items that precede each item in the
  specified transaction.

  If an item in the transaction is the first item for an account
  a fake item is generate to seed the following items."
  [storage transaction]
  (->> (:items transaction)
       (group-by :account-id)
       (map second)
       (map #(sort-by :index %))
       (map first)
       (map #(or (get-previous-item storage % (:transaction-date transaction))
                 {:account-id (:account-id %)
                  :index -1
                  :balance 0}))))
(defn delete
  "Removes the specified transaction from the system"
  [storage-spec transaction-id]
  (let [storage (storage storage-spec)
        transaction (find-by-id storage transaction-id)
        preceding-items (get-preceding-items storage transaction)]
    (delete-transaction-items-by-transaction-id storage transaction-id)
    (delete-transaction storage transaction-id)
    (update-affected-balances storage preceding-items)))

(defn- find-last-item-before
  [storage-spec account-id date]
  (first (find-transaction-items-preceding-date (storage storage-spec)
                                                account-id
                                                date)))

(defn- find-last-item-on-or-before
  [storage-spec account-id date]
  (find-last-transaction-item-on-or-before (storage storage-spec)
                                           account-id
                                           date))

(defn balance-delta
  "Returns the change in balance during the specified period for the specified account"
  [storage-spec account-id start end]
  (let [t1 (find-last-item-before storage-spec account-id (tc/to-long start))
        t2 (find-last-item-on-or-before storage-spec account-id (tc/to-long end))
        prior-balance (if t1 (:balance t1) 0)]
    (if t2
      (- (:balance t2) prior-balance)
      0)))
