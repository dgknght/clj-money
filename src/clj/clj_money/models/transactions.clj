(ns clj-money.models.transactions
  (:refer-clojure :exclude [update])
  (:require [clojure.spec.alpha :as s]
            [clojure.set :refer [difference]]
            [clojure.tools.logging :as log]
            [clj-time.core :as t]
            [stowaway.core :as storage :refer [with-storage with-transacted-storage]]
            [clj-money.util :refer [ensure-local-date uuid]]
            [clj-money.coercion :as coercion]
            [clj-money.validation :as validation]
            [clj-money.models :as models]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.prices :as prices]
            [clj-money.models.lot-transactions :as l-t]
            [clj-money.models.date-helpers :refer [parse-date-range
                                                   available-date-range]]
            [clj-money.x-platform.util :refer [deep-update-in-if
                                               deep-contains?]]
            [clj-money.x-platform.accounts :refer [polarize-quantity]])
  (:import org.joda.time.LocalDate))

(s/def ::account-id integer?)
(s/def ::action #{:debit :credit})
(s/def ::quantity validation/big-dec-not-less-than-zero?)
; Balance is the running total of quantities for the account to which
; the item belongs
(s/def ::balance (partial instance? BigDecimal))
; Value is the value of the line item expressed in the entity's
; default commodity. For most transactions, this will be the same
; as the quantity. For transactions involving foreign currencies
; and commodity purchases (like stock trades) it will be different.
(s/def ::value validation/positive-big-dec?)
(s/def ::description validation/non-empty-string?)
(s/def ::memo #(or (nil? %) (string? %)))
(s/def ::transaction-date (partial instance? LocalDate))
(s/def ::id uuid?)
(s/def ::entity-id integer?)
(s/def ::lot-id integer?)
(s/def ::lot-action #{:buy :sell})
(s/def ::shares decimal?)
(s/def ::lot-item (s/keys :req-un [::lot-id ::shares ::lot-action ::price]))
(s/def ::lot-items (s/coll-of ::lot-item))
(s/def ::index integer?)
(s/def ::transaction-item (s/keys :req-un [::account-id
                                           ::action
                                           ::quantity]
                                  :opt-un [::balance
                                           ::index
                                           ::memo]))
(s/def ::items (s/coll-of ::transaction-item :min-count 1))
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
  (get-in @ambient-settings [entity-id :delay-balances?]))

(defn- remove-empty-strings
  [model & keys]
  (reduce (fn [m k]
            (if (and (string? (k m))
                     (empty? (k m)))
              (dissoc m k)
              m))
          model
          keys))

(defn- before-save-item
  "Makes pre-save adjustments for a transaction item"
  [item]
  (-> item
      (storage/tag ::models/transaction-item)
      (update-in [:value] (fnil identity (:quantity item))) ; TODO need to calculate the correct value
      (update-in [:action] name)
      (remove-empty-strings :memo)))

(defn- after-item-read
  "Makes adjustments to a transaction item in prepartion for return
  from the data store"
  [{:keys [quantity negative reconciliation-status] :as item}]
  (if (map? item)
    (-> item
        (storage/tag ::models/transaction-item)
        (update-in [:action] keyword)
        (assoc :reconciled? (= "completed" reconciliation-status)
               :polarized-quantity (if negative
                                     (* -1 quantity)
                                     quantity)))
    item))

(defn- item-value-sum
  "Returns the sum of values of the items in the transaction having
  the specified action"
  [transaction action]
  (reduce + 0M (->> (:items transaction)

                    (filter #(= action (:action %)))
                    (map :value))))

(defn- ^{:clj-money.validation/message "The total debits does not match the total credits"
         :clj-money.validation/path [:items]}
  sum-of-credits-must-equal-sum-of-debits
  [transaction]
  (->> [:debit :credit]
       (map #(item-value-sum transaction %))
       (apply =)))

(defn- ^{:clj-money.validation/message "Each item must have the same transaction-date as the transaction"
         :clj-money.validation/path [:items]}
  transaction-dates-must-match
  [{:keys [transaction-date items]}]
  (->> items
       (map (comp ensure-local-date :transaction-date))
       (apply = transaction-date)))

(defn- before-item-validation
  [item]
  (cond->
    (-> item
        (update-in [:value] #(or % (:quantity item)))
        (assoc :balance (bigdec 0))
        (update-in [:index] (fnil identity (Integer/MAX_VALUE))))

    (string? (:account-id item))
    (update-in [:account-id] #(Integer. %))

    (or (nil? (:id item))
        (and
          (string? (:id item))
          (empty? (:id item))))
    (dissoc :id)

    (and
      (string? (:id item))
      (seq (:id item)))
    (update-in [:id] uuid))) ; TODO: use coercion rule for this

(def ^:private coercion-rules
  [(coercion/rule :uuid [:id])
   (coercion/rule :integer [:entity-id])
   (coercion/rule :local-date [:transaction-date])
   (coercion/rule :local-date [:original-transaction-date])])

(def ^:private item-coercion-rules
  [(coercion/rule :decimal [:quantity])
   (coercion/rule :keyword [:action])])

(defn- expand-simplified-items
  [{:keys [items quantity debit-account-id credit-account-id] :as transaction}]
  (if (and (not items) quantity debit-account-id credit-account-id)
    (-> transaction
        (assoc :items [{:action :debit
                        :quantity quantity
                        :account-id debit-account-id}
                       {:action :credit
                        :quantity quantity
                        :account-id credit-account-id}])
        (dissoc :quantity :debit-account-id :credit-account-id))
    transaction))

(defn- before-validation
  "Performs operations required before validation"
  [transaction]
  (-> transaction
      expand-simplified-items
      (coercion/coerce coercion-rules)
      (update-in [:items] (fn [items]
                            (->> items
                                 (map #(coercion/coerce % item-coercion-rules))
                                 (map #(merge % (select-keys
                                                  transaction
                                                  [:transaction-date
                                                   :origin-transaction-date])))
                                 (map before-item-validation))))))

(defn- after-validation
  [{transaction-date :transaction-date :as transaction}]
  (update-in transaction [:items] (fn [items]
                                    (map #(assoc % :transaction-date transaction-date)
                                         items))))
(defn- before-save
  "Returns a transaction ready for insertion into the
  database"
  [transaction]
  (-> transaction
      (storage/tag ::models/transaction)
      (dissoc :items)
      (assoc :value (->> (:items transaction)
                         (filter #(= :credit (:action %)))
                         (map #(some (fn [k] (k %)) [:value :quantity])) ; TODO this should already be :value
                         (reduce +)))
      (update-in [:lot-items] #(when %
                                 (map (fn [i]
                                        (update-in i [:lot-action] name))
                                      %)))))

(defn- fetch-lot-items
  [storage transaction-id]
  (l-t/search storage {:transaction-id transaction-id}))

(defn- prepare-criteria
  [criteria tag]
  {:pre [(deep-contains? criteria :transaction-date)]}
  (-> criteria
      (deep-update-in-if :id #(if (sequential? %)
                                (map uuid %)
                                (uuid %)))
      (deep-update-in-if :transaction-date parse-date-range)
      (storage/tag tag)))

(defn search-items
  "Returns transaction items matching the specified criteria"
  ([storage-spec criteria]
   (search-items storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (map after-item-read
          (storage/select s
                          (prepare-criteria criteria ::models/transaction-item)
                          options)))))

(defn- append-items
  [{:keys [id transaction-date] :as transaction} storage]
  (when transaction
    (assoc transaction
           :items
           (vec (search-items storage {:transaction-id id
                                       :transaction-date transaction-date}
                              {:sort [[:action :desc] [:quantity :desc]]})))))

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
  ([storage transaction]
   (after-read storage transaction {}))
  ([storage transaction options]
   (when transaction
     (cond-> transaction
       (:include-items? options)
       (append-items storage)

       (:include-lot-items? options)
       (append-lot-items storage)

       true
       (storage/tag ::models/transaction)))))

(defn find-item
  "Returns the first item matching the specified criteria"
  ([storage-spec criteria]
   (find-item storage-spec criteria {}))
  ([storage-spec criteria options]
   (first (search-items storage-spec criteria (merge options {:limit 1})))))

(defn find-item-by-id
  [storage-spec id transaction-date]
  (find-item storage-spec
             {:id id
              :transaction-date transaction-date}))

; This is public to support the unit test
(defn upsert-item
  "Updates the specified transaction item"
  [storage {:keys [id transaction-date] :as item}]
  (let [to-save (before-save-item item)]
    (if (:id to-save)
      (do
        (storage/update storage to-save)
        (find-item-by-id storage id transaction-date))
      (storage/create storage to-save))))

(defn update-items
  [storage-spec attr criteria]
  (with-storage [s storage-spec]
    (storage/update s (storage/tag attr ::models/transaction-item) criteria)))

(defn update-item-index-and-balance
  "Updates only the index and balance of an item, returning true if
  the values where changed as a result of the update, or false if the specified
  values match the existing values"
  [storage-spec item]
  (with-storage [s storage-spec]
    (let [records-affected (storage/update
                             s
                             (select-keys (before-save-item item) [:balance
                                                                   :index
                                                                   :negative])
                             [:and
                              {:id (:id item)
                               :transaction-date (some #(% item)  [:original-transaction-date
                                                                   :transaction-date])}
                              [:or
                               {:balance [:!= (:balance item)]}
                               {:index [:!= (:index item)]}]])]
      (> records-affected 0))))

(declare reload)
(defn- no-reconciled-items-changed?
  [storage transaction]
  (if (:id transaction)
    (let [existing (reload storage transaction)
          reconciled (->> existing
                          :items
                          (filter :reconciled?)
                          (map #(select-keys % [:id :quantity :account-id :action]))
                          set)
          ids (->> reconciled
                   (map :id)
                   set)
          incoming (->> transaction
                        :items
                        (filter #(ids (:id %)))
                        (map #(select-keys % [:id :quantity :account-id :action]))
                        set)]
      (= incoming reconciled))
    true))

(defn- validation-rules
  [storage]
  [#'sum-of-credits-must-equal-sum-of-debits
   #'transaction-dates-must-match
   (validation/create-rule (partial no-reconciled-items-changed? storage)
                           [:items]
                           "A reconciled transaction item cannot be changed")])

(defn- validate
  [storage spec transaction]
  (-> transaction
      before-validation
      (validation/validate spec (validation-rules storage))
      after-validation))

(s/def ::page validation/positive-integer?)
(s/def ::per-page validation/positive-integer?)
(s/def ::select-options (s/keys :req-un [::page ::per-page]))

(defn search
  "Returns the transactions that belong to the specified entity"
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (let [coerced-options (coercion/coerce options ; TODO: Don't coerce these here, expect them in the correct format
                                          [(coercion/rule :integer [:page])
                                           (coercion/rule :integer [:per-page])])
         parsed-options (if (s/valid? ::select-options coerced-options)
                          coerced-options
                          {:page 1
                           :per-page 10})]
     (with-storage [s storage-spec]
       (map #(after-read s % options)
            (storage/select s
                            (prepare-criteria criteria ::models/transaction)
                            parsed-options))))))

(defn select-items-by-reconciliation
  "Returns the transaction items associated with the specified reconciliation"
  [storage-spec reconciliation]
  (with-storage [s storage-spec]
    (map after-item-read
         (search-items s
                       {:reconciliation-id (:id reconciliation)
                        :transaction-date [:between
                                           (t/minus (:end-of-period reconciliation) (t/years 1))
                                           (t/plus (:end-of-period reconciliation) (t/months 1))]}
                       {:sort [[:transaction-date :desc] [:index :desc]]}))))

(defn record-count
  "Returns the number of transactions that match the specified criteria"
  [storage-spec criteria]
  (with-storage [s storage-spec]
    (search s criteria {:count true})))

(defn- create-transaction-item*
  [storage item]
  (->> item
       before-save-item
       (storage/create storage)
       after-item-read))

(defn- earlier
  [d1 d2]
  (->> [d1 d2]
       (filter identity)
       sort
       first))

(defn- later
  [d1 d2]
  (->> [d1 d2]
       (filter identity)
       (sort #(compare %2 %1))
       first))

(defn- link-lots
  [storage {:keys [id transaction-date]} lot-items]
  (mapv (comp #(l-t/create storage %)
              #(assoc %
                      :transaction-id id
                      :transaction-date transaction-date))
        lot-items))

(defn- find-base-item
  "Given an account ID and a date, finds the transaction item for that
  account that immediately precents the given date"
  [storage account-id as-of]
  ; TODO If no item is found, we need to know if it's because
  ; there is no item in this partition, or no item at all.
  ; I'm not sure of the storage layer is already walking back,
  ; but I'm guessing it is not.
  (find-item storage
             {:transaction-date [:< as-of]
              :account-id account-id}
             {:sort [[:transaction-date :desc]
                     [:index :desc]]
              :limit 1}))

(defn- process-items
  "Recalculates and updates statistics in the specifed items.
  
  This function is designedto short-circuit the process if it finds
  that the newly calculated index and balance values are the same
  as the values that are already stored for any particular item.
  
  There are two considerations that will prevent the short-circuit
  from happening:
    - The force option is passed in
    - The transaction date is the first date for which items are
      being processed. This is based on the assumption that the
      listing of items being processed start on a particular day
      and that the item that has been changed may not be the first
      item that day (based on index)."

  [storage
   account
   {:keys [index balance]
    :or {index -1 balance 0M}}
   items
   {:keys [force]}]
  (loop [item (first items)
         remaining (rest items)
         last-index index
         last-balance balance
         first-date (:transaction-date item)]
    (let [new-index (+ last-index 1)
          polarized-quantity (polarize-quantity item account)
          new-balance (+ last-balance polarized-quantity)]
      (if (and (not= first-date (:transaction-date item))
               (not force)
               (= new-index (:index item))
               (= new-balance (:balance item)))
        nil
        (do
          (update-item-index-and-balance storage (assoc item
                                                        :negative (> 0 polarized-quantity)
                                                        :balance new-balance
                                                        :index new-index))
          (if (seq remaining)
            (recur (first remaining)
                   (rest remaining)
                   new-index
                   new-balance
                   first-date)
            [new-index new-balance (:transaction-date item)]))))))

(defmulti ^:private account-value
  (fn [_storage _balance {:keys [tags]}]
    (tags :tradable)))

(defmethod ^:private account-value :default
  [_storage balance _account]
  balance)

(defmethod ^:private account-value :tradable
  [storage balance {:keys [commodity-id
                           earliest-transaction-date
                           latest-transaction-date]}]
  (let [[earliest latest] (available-date-range)]
    (if-let [price (first (prices/search storage
                                         {:commodity-id commodity-id
                                          :trade-date [:between
                                                       (or earliest-transaction-date earliest)
                                                       (or latest-transaction-date latest)]}
                                         {:sort [[:trade-date :desc]]
                                          :limit 1}))]
      (* (:price price) balance)
      0M)))


(defn recalculate-account
  "Recalculates statistics for items in the the specified account
  as of the specified date"
  ([storage account-id as-of]
   (recalculate-account storage account-id as-of {}))
  ([storage account-id as-of options]
   (let [base-item (find-base-item storage account-id as-of)
         items (search-items storage
                             {:account-id account-id
                              :transaction-date [:>= as-of]}
                             {:sort [:transaction-date :index]})
         account (accounts/find-by-id storage account-id)
         [last-index
          balance
          last-date] (if (seq items)
                       (process-items storage account base-item items options)
                       (if base-item
                         ((juxt :index :quantity) base-item )
                         [0 0M]))]
     (when (not (nil? last-index))
       (let [value (account-value storage balance account)]
         (log/debugf "update account summary data for \"%s\": quantity=%s, value=%s, earliest-transaction-date=%s, latest-transaction-date=%s"
                     (:name account)
                     balance
                     value
                     (earlier (:earliest-transaction-date account) as-of)
                     (later (:latest-transaction-date account) last-date))
         (accounts/update storage (-> account
                                      (assoc :quantity balance)
                                      (assoc :value value)
                                      (update-in [:earliest-transaction-date] earlier as-of)
                                      (update-in [:latest-transaction-date] later last-date))))))))

(defn- extract-account-ids
  [transaction]
  (->> (:items transaction)
       (map :account-id)
       (into #{})))

(defn- save-delayed-info
  [settings entity-id account-ids transaction-date]
  (-> settings
      (update-in [entity-id :delayed-account-ids]
                 into
                 account-ids)
      (update-in [entity-id :earliest-date] earlier transaction-date)))

(defn create
  "Creates a new transaction"
  [storage-spec transaction]
  (with-transacted-storage [s storage-spec]
    (let [validated (validate s ::new-transaction transaction)]
      (if (validation/has-error? validated)
        validated
        (let [created (storage/create s (before-save validated))
              _  (link-lots s created (:lot-items validated))
              account-ids (extract-account-ids validated)]
          (mapv (comp #(create-transaction-item* s %)
                      before-save-item
                      #(assoc %
                              :transaction-id (:id created)
                              :transaction-date (:transaction-date created)
                              :index -1))
                (:items validated))
          (if (delay-balances? (:entity-id validated))
            (swap! ambient-settings
                   save-delayed-info
                   (:entity-id validated)
                   account-ids
                   (:transaction-date validated))
            (doseq [account-id account-ids]
              (recalculate-account s account-id (:transaction-date validated))))
          (reload s created))))))

(defn find-by-id
  "Returns the specified transaction"
  [storage-spec id transaction-date]
  (first (search storage-spec
                 {:id id
                  :transaction-date transaction-date}
                 {:limit 1
                  :include-items? true
                  :include-lot-items? true})))

(defn find-by-item-id
  "Returns the transaction that has the specified transaction item"
  [storage-spec item-id transaction-date]
  (with-storage [s storage-spec]
    (when-let [{:keys [transaction-id
                       transaction-date]} (find-item-by-id
                                            s
                                            item-id
                                            transaction-date)]
      (find-by-id s transaction-id transaction-date))))

(defn items-by-account
  "Returns the transaction items for the specified account"
  ([storage-spec account-id date-spec]
   (items-by-account storage-spec account-id date-spec {}))
  ([storage-spec account-id date-spec options]
   (search-items storage-spec
                 {:account-id account-id
                  :transaction-date (if (coll? date-spec)
                                      [:between
                                       (first date-spec)
                                       (second date-spec)]
                                      date-spec)}
                 (merge options
                        {:sort [[:transaction-date :desc]
                                [:index :desc]]}))))

(defn unreconciled-items-by-account
  "Returns the unreconciled transaction items for the specified account"
  [storage-spec account-id]
  (search storage-spec {:account-id account-id
                        :reconciliation-id nil}))

(defn reload
  "Returns an updated copy of the transaction"
  [storage-spec {:keys [id transaction-date]}]
  (find-by-id storage-spec id transaction-date))

(defn- find-existing-transaction
  "Given a transaction that has been updated, find the existing
  transaction in storage. If none can be found, throw an exception."
  [storage {:keys [id transaction-date original-transaction-date]}]
  (let [search-date (or original-transaction-date transaction-date)]
    (or (find-by-id storage id search-date)
        (throw (ex-info
                 (format "Unable to find transaction with id %s and date %s"
                         id
                         search-date)
                 {:id id
                  :search-date search-date})))))

(defn- update-transaction*
  [storage transaction]
  (->> transaction
       before-save
       (storage/update storage)))

(defn- update-full-transaction
  "Update the transaction and associated items."
  [storage transaction]
  (update-transaction* storage transaction)
  (doseq [item (:items transaction)]
    (as-> item i
      (assoc i :transaction-id (:id transaction))
      (upsert-item storage i))))

; Processing a transaction
; 1. Save the transaction and item records
; 2. Identify starting items for account rebalancing
;   a. Find the item immediately before (chronologically) this transaction in
;      each account with a item in this transaction. If this is an update,
;      this must be based on the earlier of the original transaction date
;      and the current transaction date.
;   b. Find the item immediately before (chronologically) this transaction in
;      each account that has been dereferenced from this transaction. This is
;      based only on the original transaction date (which will be stored in
;      the dereferenced transaction item.) For new transactions this is always
;      empty.
;   c. Recalculate statistics for each of the items identified in identified in
;      steps a and b.

(defn update
  "Updates the specified transaction"
  [storage-spec transaction]
  (with-transacted-storage [storage storage-spec]
    (let [validated (validate storage ::existing-transaction transaction)]
      (if (validation/has-error? validated)
        validated
        (let [existing (find-existing-transaction storage transaction)
              dereferenced-items (->> (:items existing)
                                      (remove #(some (fn [i] (= (:id i)
                                                                (:id %)))
                                                     (:items validated))))
              dereferenced-account-ids (difference (->> (:items existing)
                                                        (map :account-id)
                                                        (into #{}))
                                                   (->> (:items validated)
                                                        (map :account-id)
                                                        (into #{})))
              recalc-base-date (earlier (:transaction-date existing)
                                        (:transaction-date validated))
              recalc-account-ids (->> (:items validated)
                                      (map :account-id)
                                      (concat dereferenced-account-ids)
                                      (into #{}))]
          (update-full-transaction storage validated)
          (doseq [item dereferenced-items]
            (storage/delete storage item))
          (doseq [account-id recalc-account-ids]
            (recalculate-account storage account-id recalc-base-date))
          (reload storage validated))))))

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
  [storage-spec {:keys [id transaction-date]}]
  (with-storage [s storage-spec]
    (let [transaction (find-by-id s id transaction-date)]
      (ensure-deletable transaction)
      (storage/delete s transaction)
      (doseq [account-id (extract-account-ids transaction)]
        (recalculate-account s account-id (:transaction-date transaction))))))

(defn- find-last-item-before
  [storage-spec account date]
  (find-item storage-spec
             {:account-id (:id account)
              :transaction-date [:between (:earliest-transaction-date account) (t/minus date (t/days 1))]}
             {:sort [[:transactions.transaction-date :desc] [:transaction_items.index :desc]]}))

(defn- find-last-item-on-or-before
  [storage-spec account date]
  (find-item storage-spec
             {:account-id (:id account)
              :transaction-date [:between (:earliest-transaction-date account) date]}
             {:sort [[:transactions.transaction-date :desc]
                     [:transaction_items.index :desc]]}))

(defn balance-delta
  "Returns the change in balance during the specified period for the specified account"
  [storage-spec account start end]
  (let [t1 (find-last-item-before storage-spec account start)
        t2 (find-last-item-on-or-before storage-spec account end)]
    (- (or (:balance t2) 0M)
       (or (:balance t1) 0M))))

(defn balance-as-of
  "Returns the balance for the specified account as of the specified date"
  [storage-spec account as-of]
  (or (:balance
        (find-last-item-on-or-before storage-spec
                                     account as-of))
      0M))

(defn find-items-by-ids
  [storage-spec ids date-range]
  (search-items
    storage-spec
    {:id ids
     :transaction-date (vec (concat [:between] date-range))}))

(defmacro with-delayed-balancing
  [storage-spec entity-id & body]
  `(do
     ; Make a note that balances should not be calculated
     ; for this entity
     (swap! ambient-settings
            assoc
            ~entity-id
            {:delay-balances? true
             :delayed-account-ids #{}})
     (let [result# (do ~@body)]
       ; Recalculate balances for affected accounts
       (let [{account-ids# :delayed-account-ids
              as-of# :earliest-date} (get @ambient-settings ~entity-id)]
         (with-transacted-storage [s# ~storage-spec]
           (doseq [account-id# account-ids#]
             (recalculate-account s# account-id# as-of#))))

       ; clean up the ambient settings as if we were never here
       (swap! ambient-settings dissoc ~entity-id)
       result#)))
