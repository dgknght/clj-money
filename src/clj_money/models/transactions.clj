(ns clj-money.models.transactions
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clojure.set :refer [difference]]
            [clj-time.coerce :as tc]
            [clj-money.util :refer [ensure-local-date pprint-and-return]]
            [clj-money.coercion :as coercion]
            [clj-money.validation :as validation]
            [clj-money.authorization :as authorization]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.helpers :refer [with-storage with-transacted-storage]]
            [clj-money.models.storage :refer [select-transactions
                                              count-transactions
                                              create-transaction
                                              create-transaction-item
                                              find-transaction-by-id
                                              update-transaction
                                              select-transaction-items
                                              select-lots-transactions-by-transaction-id
                                              update-transaction-item
                                              update-transaction-item-index-and-balance
                                              delete-transaction
                                              delete-transaction-item
                                              delete-transaction-items-by-transaction-id
                                              create-lot->transaction-link
                                              delete-lot->transaction-link]])
  (:import org.joda.time.LocalDate))

(s/def ::account-id integer?)
(s/def ::action #{:debit :credit})
; Amount is the quantity of the commodity that is exchanged
(s/def ::amount validation/positive-big-dec?)
; Balance is the running total of amounts for the account to which
; the item belongs
(s/def ::balance (partial instance? BigDecimal))
; Value is the value of the line item expressed in the entity's
; default currency. For most transactions, this will be the same
; as the amount. For transactions involving foreign currencies
; and commodity purchases (like stock trades) it will be different.
(s/def ::value validation/positive-big-dec?)
(s/def ::description validation/non-empty-string?)
(s/def ::memo #(or (nil? %) (string? %)))
(s/def ::transaction-date (partial instance? LocalDate))
(s/def ::id integer?)
(s/def ::entity-id integer?)
(s/def ::lot-id integer?)
(s/def ::lot-action #{:buy :sell})
(s/def ::shares decimal?)
(s/def ::lot-item (s/keys :req-un [::lot-id ::shares ::lot-action ::price]))
(s/def ::lot-items (s/coll-of ::lot-item))
(s/def ::index integer?)
(s/def ::transaction-item (s/keys :req-un [::account-id
                                           ::action
                                           ::amount]
                                  :opt-un [::balance
                                           ::index
                                           ::memo]))
(s/def ::items (s/coll-of ::transaction-item :min-count 2))
(s/def ::new-transaction (s/keys :req-un [::description
                                          ::transaction-date
                                          ::items
                                          ::entity-id]
                                 :opt-un [::memo
                                          ::lot-items]))
(s/def ::existing-transaction (s/keys :req-un [::id
                                               ::transaction-date
                                               ::items]
                                      :opt-un [::entity-id
                                               ::memo
                                               ::lot-items]))

(def ambient-settings
  (atom {}))

(defn- delay-balances?
  [entity-id]
  true
  (get-in @ambient-settings [entity-id :delay-balances?]))

(defn- before-save-item
  "Makes pre-save adjustments for a transaction item"
  [item]
  (cond-> item
    true (update-in [:action] name)
    (and (string? (:memo item))
         (empty? (:memo item))) (dissoc :memo)))

(defn polarize-item-amount
  [item account]
  (assoc item :polarized-amount (accounts/polarize-amount item account)))

(defn- after-item-read
  "Makes adjustments to a transaction item in prepartion for return
  from the data store"
  ([item] (after-item-read item nil))
  ([item account]
   (if (map? item)
     (cond-> item
       true (update-in [:action] keyword)
       true (assoc :reconciled? (= "completed" (:reconciliation-status item)))
       (:transaction-date item) (update-in [:transaction-date] tc/to-local-date)
       account (polarize-item-amount account))
     item)))

(defn- item-value-sum
  "Returns the sum of values of the items in the transaction having
  the specified action"
  [transaction action]
  (reduce + 0 (->> (:items transaction)

                   (filter #(= action (:action %)))
                   (map :value))))

(defn- ^{:clj-money.validation/message "The total debits does not match the total credits"
         :clj-money.validation/path [:items]}
  sum-of-credits-must-equal-sum-of-debits
  [transaction]
  (->> [:debit :credit]
       (map #(item-value-sum transaction %))
       (apply =)))

(defn- before-item-validation
  [item]
  (cond-> item
    true (update-in [:value] #(or % (:amount item)))
    true (assoc :balance (bigdec 0))
    (string? (:account-id item)) (update-in [:account-id] #(Integer. %))
    (nil? (:id item)) (dissoc :id)
    (and
      (string? (:id item))
      (empty? (:id item))) (dissoc :id)
    (and
      (string? (:id item))
      (not (empty? (:id item)))) (update-in [:id] #(Integer. %))))

(def ^:private coercion-rules
  [(coercion/rule :integer [:id])
   (coercion/rule :integer [:entity-id])
   (coercion/rule :local-date [:transaction-date])])

(defn- before-validation
  "Performs operations required before validation"
  [transaction]
  (-> (coercion/coerce coercion-rules transaction)
      (update-in [:items] #(map before-item-validation %))))

(defn- before-save
  "Returns a transaction ready for insertion into the
  database"
  [transaction]
  (-> transaction
      (dissoc :items)
      (update-in [:transaction-date] tc/to-long)
      (update-in [:lot-items] #(when %
                                 (map (fn [i]
                                        (update-in i [:lot-action] name))
                                      %)))))

(defn- fetch-lot-items
  [storage transaction-id]
  (select-lots-transactions-by-transaction-id storage transaction-id))

(defn- append-items
  [transaction storage]
  (when transaction
    (assoc transaction
           :items
           (->> (select-transaction-items storage
                                          {:transaction-id (:id transaction)}
                                          {:sort [[:action :desc] [:amount :desc]]})
                (map after-item-read)))))

(defn- append-lot-items
  [transaction storage]
  (if transaction
    (let [lot-items (fetch-lot-items storage (:id transaction))]
      (if (seq lot-items)
        (assoc transaction
               :lot-items
               (->> lot-items
                    (map #(-> %
                              (dissoc :transaction-id)
                              (update-in [:lot-action] keyword)))))
        transaction))
    transaction))

(defn- after-read
  "Returns a transaction that is ready for public use"
  [storage transaction]
  (when transaction
    (-> transaction
        (update-in [:transaction-date] tc/to-local-date)
        (append-items storage)
        (append-lot-items storage)
        (authorization/tag-resource :transaction))))

(defn- get-previous-item
  "Finds the transaction item that immediately precedes the specified item"
  [storage item transaction-date]
  (->> (select-transaction-items storage
                                 {:i.account-id (:account-id item)
                                  :t.transaction-date [:< (tc/to-long transaction-date)]}
                                 {:sort [[:t.transaction-date :desc] [:i.index :desc]]})
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
        account (accounts/find-by-id (:storage context) (:account-id item))
        polarized-amount (accounts/polarize-amount item account)
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
   (with-storage [s storage-spec]
     (->> (select-transaction-items
            s
            {:i.account-id (:account-id reference-item)
             :index [:>= (:index reference-item)]}
            {:sort [[:index :desc]]})
          (remove #(= (:id reference-item) (:id %)))
          (map after-item-read))))
  ([storage-spec reference-item transaction-date]
   (with-storage [s storage-spec]
     (->> (select-transaction-items
            s
            {:i.account-id (:account-id reference-item)
             :t.transaction-date [:>= (tc/to-long transaction-date)]}
            {:sort [:index]})
          (remove #(= (:id reference-item) (:id %)))
          (map after-item-read)))))

(defn find-item-by-id
  [storage-spec id]
  (first (select-transaction-items storage-spec {:i.id id} {:limit 1})))

(defn- upsert-item
  "Updates the specified transaction item"
  [storage item]
  (if (:id item)
    (do
      (update-transaction-item storage item)
      (find-item-by-id storage (:id item)))
    (create-transaction-item storage item)))

(defn- update-item-index-and-balance
  "Updates only the index and balance attributes of an item, returning true if
  the values where changed as a result of the update, or false if the specified
  values match the existing values"
  [storage-spec item]
  (with-storage [s storage-spec]
    (let [records-affected (first (update-transaction-item-index-and-balance
                                    s
                                    item))]
      (> records-affected 0))))

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
        account (accounts/find-by-id storage (:account-id item))
        polarized-amount (accounts/polarize-amount item account)
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
           account (accounts/find-by-id storage-spec account-id)
           subsequent-items (if transaction-date
                              (subsequent-items storage-spec last-item transaction-date)
                              (subsequent-items storage-spec last-item))
           final (reduce calculate-item-index-and-balance
                         (assoc last-item :storage storage-spec)
                         subsequent-items)]
       (when-not (::skip-account-update final)
         (accounts/update storage-spec (assoc account :balance (:balance final))))))))

(declare reload)
(defn- no-reconciled-items-changed?
  [storage transaction]
  (if (:id transaction)
    (let [existing (reload storage transaction)
          reconciled (->> existing
                          :items
                          (filter :reconciled?)
                          (map #(select-keys % [:id :amount :account-id :action]))
                          set)
          ids (->> reconciled
                   (map :id)
                   set)
          incoming (->> transaction
                        :items
                        (filter #(ids (:id %)))
                        (map #(select-keys % [:id :amount :account-id :action]))
                        set)]
      (= incoming reconciled))
    true))

(defn- validation-rules
  [storage]
  [#'sum-of-credits-must-equal-sum-of-debits
   (validation/create-rule (partial no-reconciled-items-changed? storage)
                           [:items]
                           "A reconciled transaction item cannot be changed")])

(defn- validate
  [storage spec transaction]
  (->> transaction
       before-validation
       (validation/validate spec (validation-rules storage))))

(s/def ::page validation/positive-integer?)
(s/def ::per-page validation/positive-integer?)
(s/def ::select-options (s/keys :req-un [::page ::per-page]))

(defn search
  "Returns the transactions that belong to the specified entity"
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (let [coerced-options (coercion/coerce [(coercion/rule :integer [:page])
                                           (coercion/rule :integer [:per-page])]
                                          options )
         parsed-options (if (s/valid? ::select-options coerced-options)
                          coerced-options
                          {:page 1
                           :per-page 10})]
     (with-storage [s storage-spec]
       (map #(after-read s %)
            (select-transactions s criteria parsed-options))))))

(defn select-items-by-reconciliation-id
  "Returns the transaction items associated with the specified reconciliation"
  [storage-spec reconciliation-id]
  (with-storage [s storage-spec]
    (map after-item-read
         (select-transaction-items s
                                   {:reconciliation-id reconciliation-id}
                                   {:sort [[:t.transaction-date :desc] [:i.index :desc]]}))))

(defn record-count
  "Returns the number of transactions that belong to the specified entity"
  [storage-spec criteria]
  (with-storage [s storage-spec]
    (count-transactions s criteria)))

(defn- create-transaction-and-lot-links
  [storage transaction]
  (let [result (create-transaction storage transaction)]
    (when-let [lot-items (:lot-items transaction)]
      (doseq [lot-item lot-items]
        (create-lot->transaction-link storage
                                      (assoc lot-item
                                             :transaction-id
                                             (:id result)))))
    result))

(defn- create-transaction-without-balances
  [storage {:keys [entity-id] :as transaction}]
  (swap! ambient-settings
         update-in
         [entity-id :delayed-account-ids]
         #(into % (map :account-id (:items transaction))))
  (let [result (->> transaction
                    before-save
                    (create-transaction-and-lot-links storage)
                    (after-read storage))]
    (assoc result :items (mapv (fn [item]
                                 (create-transaction-item
                                   storage
                                   (-> item
                                       (assoc :transaction-id (:id result)
                                              :balance 0M
                                              :index 0)
                                       before-save-item)))
                               (:items transaction)))))

(defn- create-transaction-and-adjust-balances
  [storage transaction]
  (let [items-with-balances (calculate-balances-and-indexes
                              storage
                              (:transaction-date transaction)
                              (:items transaction))
        _ (update-affected-balances storage items-with-balances
                                    (:transaction-date transaction))
        result (->> (assoc transaction :items items-with-balances)
                    before-save
                    (create-transaction-and-lot-links storage)
                    (after-read storage))
        items (into [] (map #(->> (assoc % :transaction-id (:id result))
                                  before-save-item
                                  (create-transaction-item storage)
                                  after-item-read)
                            items-with-balances))]
    (assoc result :items items)))

(defn create
  "Creates a new transaction"
  [storage-spec transaction]
  (with-transacted-storage [s storage-spec]
    (let [validated (validate s ::new-transaction transaction)]
      (if (validation/has-error? validated)
        validated
        (if (delay-balances? (:entity-id transaction))
          (create-transaction-without-balances s validated)
          (create-transaction-and-adjust-balances s validated))))))

(defn find-by-id
  "Returns the specified transaction"
  [storage-spec id]
  (with-storage [s storage-spec]
    (->> (find-transaction-by-id s id)
         (after-read s))))

(defn find-by-item-id
  "Returns the transaction that has the specified transaction item"
  [storage-spec item-id]
  (with-storage [s storage-spec]
    (when-let [transaction-id (:transaction-id (find-item-by-id s item-id))]
      (find-by-id s transaction-id))))

(defn find-items-by-ids
  [storage-spec ids]
  (with-storage [s storage-spec]
    (->> (select-transaction-items s {:i.id ids})
         (map after-item-read))))

(defn items-by-account
  "Returns the transaction items for the specified account"
  [storage-spec account-id]
  (with-storage [s storage-spec]
    (let [account (accounts/find-by-id storage-spec account-id)]
      (map #(after-item-read % account)
           (select-transaction-items s
                                     {:i.account-id account-id}
                                     {:sort [[:t.transaction-date :desc] [:i.index :desc]]})))))

(defn count-items-by-account
  "Returns the number of transaction items in the account"
  [storage-spec account-id]
  (with-storage [s storage-spec]
    (select-transaction-items
      s
      {:i.account-id account-id}
      {:count true})))

(defn unreconciled-items-by-account
  "Returns the unreconciled transaction items for the specified account"
  [storage-spec account-id]
  (with-storage [s storage-spec]
    (let [account (accounts/find-by-id storage-spec account-id)]
      (map #(after-item-read % account)
           (select-transaction-items s {:i.account-id account-id
                                        :reconciliation-id nil})))))

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
  (with-transacted-storage [storage storage-spec]
    (let [validated (validate storage ::existing-transaction transaction)]
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
                                  (map #(assoc % :transaction-id (:id validated)))
                                  (process-item-upserts storage))]
          (->> validated
               before-save
               (update-transaction storage))
          (update-affected-balances storage
                                    (concat upserted-items
                                            dereferenced-base-items)
                                    (:transaction-date validated))
          (reload storage validated))))))

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

(defn can-delete?
  [transaction]
  (->> transaction
       :items
       (filter :reconciled?)
       empty?))

(defn- ensure-deletable
  "Throws an exception if the transaction cannot be deleted"
  [transaction]
  (let [reconciled-items (->> transaction
                              :items
                              (filter :reconciled?))]
    (when (seq reconciled-items)
      (throw (ex-info "A transaction with reconciled items cannot be deleted."
                      {:reconciled-items reconciled-items})))))

(defn delete
  "Removes the specified transaction from the system"
  [storage-spec transaction-id]
  (with-storage [s storage-spec]
    (let [transaction (find-by-id s transaction-id)
          _ (ensure-deletable transaction)
          preceding-items (get-preceding-items s transaction)]
      (delete-transaction-items-by-transaction-id s transaction-id)
      (delete-transaction s transaction-id)
      (update-affected-balances s preceding-items))))

(defn- find-last-item-before
  [storage-spec account-id date]
  (with-storage [s storage-spec]
    (first (select-transaction-items
             s
             {:i.account-id account-id
              :t.transaction-date [:< date]}
             {:sort [[:t.transaction-date :desc] [:i.index :desc]]}))))

(defn- find-last-item-on-or-before
  [storage-spec account-id date]
  (with-storage [s storage-spec]
    (first (select-transaction-items s
                                     {:i.account-id account-id
                                      :t.transaction-date [:<= date]}
                                     {:sort [[:t.transaction-date :desc] [:i.index :desc]]
                                      :limit 1}))))

(defn balance-delta
  "Returns the change in balance during the specified period for the specified account"
  [storage-spec account-id start end]
  (let [t1 (find-last-item-before storage-spec account-id (tc/to-long start))
        t2 (find-last-item-on-or-before storage-spec account-id (tc/to-long end))
        prior-balance (if t1 (:balance t1) 0M)]
    (if t2
      (- (:balance t2) prior-balance)
      0M)))

(defn balance-as-of
  "Returns the balance for the specified account as of the specified date"
  [storage-spec account-id as-of]
  (let [t (find-last-item-on-or-before storage-spec account-id (tc/to-long as-of))]
    (if t
      (:balance t)
      0M)))

(defn search-items
  "Returns transaction items matching the specified criteria"
  [storage-spec criteria]
  (with-storage [s storage-spec]
    (->> criteria
         (select-transaction-items s)
         (map after-item-read))))

(defn recalculate-balances
  "Recalculates balances for the specified account and all
  related transaction items"
  [storage-spec account-id]
  (with-storage [s storage-spec]
    (let [result (->> {:i.account-id account-id} ; Remove the table prefix here
                      (search-items storage-spec)
                      (reduce calculate-item-index-and-balance {:index 0
                                                                :balance 0M
                                                                :storage s}))]
      (when-not (::skip-account-update result)
        (accounts/update storage-spec (-> (accounts/find-by-id s account-id)
                                          (assoc :balance (:balance result))))))))


(defmacro with-delayed-balancing
  [storage-spec entity-id & body]
  `(do
     ; Make a note that balances should not be calculated
     ; for this entity
     (swap! ambient-settings update-in
                             [~entity-id]
                             (fnil #(assoc % :delay-balances? true
                                             :delayed-account-ids #{})
                                   {}))
     ~@body

     ; Recalculate balances for affected accounts
     (doseq [account-id# (get-in @ambient-settings [~entity-id :delayed-account-ids])]
       (recalculate-balances ~storage-spec account-id#))

     ; clean up the ambient settings as if we were never here
     (swap! ambient-settings dissoc ~entity-id)))
