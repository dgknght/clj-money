(ns clj-money.models.transactions
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.set :refer [difference]]
            [clojure.tools.logging :as log]
            [clj-time.core :as t]
            [clj-money.util :refer [pprint-and-return
                                    ensure-local-date]]
            [clj-money.coercion :as coercion]
            [clj-money.validation :as validation]
            [clj-money.authorization :as authorization]
            [clj-money.models.accounts :as accounts]
            [clj-money.x-platform.accounts :refer [polarize-quantity]]
            [clj-money.models.helpers :refer [with-storage with-transacted-storage]]
            [clj-money.models.storage :refer [select-transactions
                                              create-transaction
                                              create-transaction-item
                                              update-transaction
                                              select-transaction-items
                                              select-lots-transactions-by-transaction-id
                                              update-transaction-item
                                              update-transaction-item-index-and-balance
                                              delete-transaction
                                              delete-transaction-item
                                              delete-transaction-items-by-transaction-id
                                              create-lot->transaction-link
                                              delete-lot->transaction-link
                                              get-setting]])
  (:import org.joda.time.LocalDate
           java.util.UUID))

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

(defn- to-entity-currency
  [amount account]
  ; TODO look up the entity currency and convert if needed
  amount)

(defn- before-save-item
  "Makes pre-save adjustments for a transaction item"
  [item]
  (-> item
      (update-in [:value] (fnil identity (:quantity item))) ; TODO need to calculate the correct value
      (update-in [:action] name)
      (remove-empty-strings :memo)
      (update-in [:negative] (fnil identity false))))

(defn- after-item-read
  "Makes adjustments to a transaction item in prepartion for return
  from the data store"
  ([item] (after-item-read item nil))
  ([{:keys [quantity negative reconciliation-status] :as item} account]
   (if (map? item)
     (-> item
         (update-in [:action] keyword)
         (assoc :reconciled? (= "completed" reconciliation-status)
                :polarized-quantity (if negative
                                      (* -1 quantity)
                                      quantity)))
     item)))

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
  [{:keys [transaction-date items] :as transaction}]
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
      (not (empty? (:id item))))
    (update-in [:id] #(UUID/fromString %)))) ; TODO: use coercion rule for this

(def ^:private coercion-rules
  [(coercion/rule :uuid [:id])
   (coercion/rule :integer [:entity-id])
   (coercion/rule :local-date [:transaction-date])
   (coercion/rule :local-date [:original-transaction-date])])

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
      (coercion/coerce coercion-rules)
      expand-simplified-items
      (update-in [:items] (fn [items]
                            (->> items
                                 (map #(assoc % :transaction-date
                                                (:transaction-date transaction)
                                                :original-transaction-date
                                                (:original-transaction-date transaction)))
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
  (select-lots-transactions-by-transaction-id storage transaction-id))

(defn- append-items
  [{:keys [id transaction-date] :as transaction} storage]
  (when transaction
    (assoc transaction
           :items
           (mapv after-item-read
                 (select-transaction-items
                   storage
                   {:transaction-id id
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
       (authorization/tag-resource :transaction)))))

(defn- get-previous-item
  "Finds the transaction item that immediately precedes the specified item,
  or a fake 'before first' item if there are no preceding items"
  [storage {:keys [account-id transaction-date transaction-id] :as item}]
  (or (->> (select-transaction-items storage
                                     {:account-id account-id
                                      :transaction-id [:<> transaction-id]
                                      :transaction-date [:<= transaction-date]}
                                     {:sort [[:transaction-date :desc] [:index :desc]]
                                      :limit 1})
           first)
      {:account-id account-id
       :transaction-date transaction-date
       :index -1
       :balance 0M}))

(defn find-item-by-id
  [storage-spec id transaction-date]
  (first (select-transaction-items storage-spec
                                   {:id id
                                    :transaction-date transaction-date}
                                   {:limit 1})))

; This is public to support the unit test
(defn upsert-item
  "Updates the specified transaction item"
  [storage {:keys [id transaction-date] :as item}]
  (let [to-save (before-save-item item)]
    (if (:id to-save)
      (do
        (update-transaction-item storage to-save)
        (find-item-by-id storage id transaction-date))
      (create-transaction-item storage to-save))))

(defn- update-item-index-and-balance
  "Updates only the index and balance attributes of an item, returning true if
  the values where changed as a result of the update, or false if the specified
  values match the existing values"
  [storage-spec item]
  (with-storage [s storage-spec]
    (let [records-affected (first (update-transaction-item-index-and-balance
                                    s
                                    (before-save-item item)))]
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

(defmulti ^:private parse-date-range
  type)

(defmethod ^:private parse-date-range :default
  [value]
  value)

(defmethod ^:private parse-date-range String
  [value]
  (let [[year month day] (->> (re-matches #"(\d{4})(?:-(\d{2})(?:-(\d{2}))?)?"
                                          value)
                              rest
                              (map #(when % (Integer. %))))]
    (cond

      day
      (t/local-date year month day)

      month
      [:between
       (t/first-day-of-the-month year month)
       (t/last-day-of-the-month year month)]

      :else
      [:between
       (t/local-date year 1 1)
       (t/local-date year 12 31)])))

(defn- ensure-transaction-date-type
  [criteria]
  (if-let [transaction-date (:transaction-date criteria)]
    (assoc criteria :transaction-date (parse-date-range transaction-date))
    criteria))

(defn- ensure-id-type
  [criteria]
  (if-let [id (:id criteria)]
    (if (string? id)
      (assoc criteria :id (java.util.UUID/fromString id))
      criteria)
    criteria))

(defn- prepare-criteria
  [criteria]
  (-> criteria
      ensure-transaction-date-type
      ensure-id-type))

(defn search
  "Returns the transactions that belong to the specified entity"
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (let [coerced-options (coercion/coerce options
                                          [(coercion/rule :integer [:page])
                                           (coercion/rule :integer [:per-page])])
         parsed-options (if (s/valid? ::select-options coerced-options)
                          coerced-options
                          {:page 1
                           :per-page 10})]
     (with-storage [s storage-spec]
       (map #(after-read s % options)
            (select-transactions s
                                 (prepare-criteria criteria)
                                 parsed-options))))))

(defn select-items-by-reconciliation
  "Returns the transaction items associated with the specified reconciliation"
  [storage-spec reconciliation]
  (with-storage [s storage-spec]
    (map after-item-read
         (select-transaction-items s
                                   {:reconciliation-id (:id reconciliation)
                                    :transaction-date [:between
                                                       (t/minus (:end-of-period reconciliation) (t/years 1))
                                                       (t/plus (:end-of-period reconciliation) (t/months 1))]}
                                   {:sort [[:transaction-date :desc] [:index :desc]]}))))

(defn record-count
  "Returns the number of transactions that belong to the specified entity"
  [storage-spec criteria]
  (with-storage [s storage-spec]
    (select-transactions s criteria {:count true})))

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

(defn- create-transaction-item*
  [storage item]
  (->> item
       before-save-item
       (create-transaction-item storage)
       after-item-read))

(defn search-items
  "Returns transaction items matching the specified criteria"
  ([storage-spec criteria]
   (search-items storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (map after-item-read
          (select-transaction-items s
                                    (prepare-criteria criteria)
                                    options)))))

(defn find-item
  "Returns the first item matching the specified criteria"
  ([storage-spec criteria]
   (find-item storage-spec criteria {}))
  ([storage-spec criteria options]
   (first (search-items storage-spec criteria options))))

(defn- earlier
  [d1 d2]
  (->> [d1 d2]
       sort
       (filter identity)
       first))

(defn- later
  [d1 d2]
  (->> [d1 d2]
       (sort #(compare %2 %1))
       (filter identity)
       first))

(defn- link-lots
  [storage transaction-id lot-items]
  (when lot-items
    (doseq [lot-item lot-items]
      (create-lot->transaction-link storage
                                    (assoc lot-item
                                           :transaction-id
                                           transaction-id)))))

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
  "Recalculates and updates statistics in the specifed items"
  [storage account {:keys [index balance] :or {index -1 balance 0M}} items]
  (loop [item (first items)
         remaining (rest items)
         last-index index
         last-balance balance]
    (let [new-index (+ last-index 1)
          new-balance (+ last-balance (polarize-quantity item account))]
      (if (and (= new-index (:index item))
               (= new-balance (:balance item)))
        nil ; short-circuit updates if they aren't necessary
        (do
          ; TODO extract update, this is always an update
          (upsert-item storage (assoc item
                                      :balance new-balance
                                      :index new-index))
          (if (seq remaining)
            (recur (first remaining)
                   (rest remaining)
                   new-index
                   new-balance)
            [new-index new-balance]))))))

(defn recalculate-account
  "Recalculates statistics for items in the the specified account
  as of the specified date"
  [storage account-id as-of]
  (let [base-item (find-base-item storage account-id as-of)
        items (search-items storage
                            {:account-id account-id
                             :transaction-date [:>= as-of]}
                            {:sort [:transaction-date :index]})
        account (accounts/find-by-id storage account-id)
        [last-index
         balance] (if (seq items)
                    (process-items storage account base-item items)
                    (if base-item
                      ((juxt :index :quantity) base-item )
                      [0 0M]))]
    (when (not (nil? last-index))
      (accounts/update storage (-> account
                                   (assoc :quantity balance)
                                   (assoc :value balance) ; TODO need to calculate this for real
                                   (update-in [:earliest-transaction-date] #((fnil earlier as-of) % as-of))
                                   (update-in [:latest-transaction-date] #((fnil later as-of) % as-of)))))))

(defn- extract-account-ids
  [transaction]
  (->> (:items transaction)
       (map :account-id)
       (into #{})))

(defn- save-delayed-info
  [settings {:keys [entity-id transaction-date] :as transaction}]
  (-> settings
      (update-in [entity-id :delayed-account-ids]
                 concat
                 (extract-account-ids transaction))
      (update-in [entity-id :earliest-date] #(if %
                                               (earlier % transaction-date)
                                               transaction-date))))

(defn create
  "Creates a new transaction"
  [storage-spec transaction]
  (with-transacted-storage [s storage-spec]
    (let [validated (validate s ::new-transaction transaction)]
      (if (validation/has-error? validated)
        validated
        (let [created (->> validated
                           before-save
                           (create-transaction s))
              _  (link-lots s (:id created) (:lot-items validated))
              account-ids (extract-account-ids validated)]
          (doall (->> (:items validated)
                      (map #(assoc %
                                   :transaction-id (:id created)
                                   :transaction-date (:transaction-date validated)
                                   :index -1))
                      (map before-save-item)
                      (map #(create-transaction-item* s %))))
          (if (delay-balances? (:entity-id validated))
            (swap! ambient-settings #(save-delayed-info % validated))
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
   (with-storage [s storage-spec]
     (let [account (accounts/find-by-id storage-spec account-id)]
       (map #(after-item-read % account)
            (select-transaction-items s
                                      {:account-id account-id
                                       :transaction-date (if (coll? date-spec)
                                                             [:between
                                                              (first date-spec)
                                                              (second date-spec)]
                                                             date-spec)}
                                      (merge options
                                             {:sort [[:transaction-date :desc]
                                                     [:index :desc]]})))))))

(defn count-items-by-account
  "Returns the number of transaction items in the account"
  [storage-spec account-id]
  (with-storage [s storage-spec]
    (select-transaction-items
      s
      {:account-id account-id}
      {:count true})))

(defn unreconciled-items-by-account
  "Returns the unreconciled transaction items for the specified account"
  [storage-spec account-id]
  (with-storage [s storage-spec]
    (let [account (accounts/find-by-id storage-spec account-id)]
      (map #(after-item-read % account)
           (select-transaction-items s {:account-id account-id
                                        :reconciliation-id nil})))))

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
       (update-transaction storage)))

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
        (do
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
              (delete-transaction-item storage (:id item) (:transaction-date existing)))
            (doseq [account-id recalc-account-ids]
              (recalculate-account storage account-id recalc-base-date))
            (reload storage validated)))))))

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
  [storage-spec transaction-id transaction-date]
  (with-storage [s storage-spec]
    (let [transaction (find-by-id s transaction-id transaction-date)
          _ (ensure-deletable transaction)
          account-ids (extract-account-ids transaction)]
      (delete-transaction-items-by-transaction-id s transaction-id transaction-date)
      (delete-transaction s transaction-id transaction-date)
      (doseq [account-id (extract-account-ids transaction)]
        (recalculate-account s account-id (:transaction-date transaction))))))

(defn- find-last-item-before
  [storage-spec account-id date]
  (with-storage [s storage-spec]
    (->> (select-transaction-items
           s
           {:account-id account-id
            :transaction-date [:< date]}
           {:sort [[:t.transaction-date :desc] [:i.index :desc]]
            :limit 1})
         first)))

(defn- find-last-item-on-or-before
  [storage-spec account-id date]
  (with-storage [s storage-spec]
    (first (select-transaction-items s
                                     {:account-id account-id
                                      :transaction-date [:<= date]}
                                     {:sort [[:i.transaction-date :desc] [:i.index :desc]]
                                      :limit 1}))))

(defn balance-delta
  "Returns the change in balance during the specified period for the specified account"
  [storage-spec account-id start end]
  (let [t1 (find-last-item-before storage-spec account-id start)
        t2 (find-last-item-on-or-before storage-spec account-id end)
        prior-balance (if t1 (:balance t1) 0M)]
    (if t2
      (- (:balance t2) prior-balance)
      0M)))

(defn balance-as-of
  "Returns the balance for the specified account as of the specified date"
  [storage-spec account-id as-of]
  (let [t (find-last-item-on-or-before storage-spec account-id as-of)]
    (if t
      (:balance t)
      0M)))

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
     (swap! ambient-settings update-in
            [~entity-id]
            (fnil #(assoc %
                          :delay-balances? true
                          :delayed-account-ids #{})
                  {}))
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

(defn available-date-range
  [storage-spec]
  (with-storage [s storage-spec]
    (->> ["earliest-partition-date"
          "latest-partition-date"]
         (map #(get-setting s %))
         (map read-string))))
