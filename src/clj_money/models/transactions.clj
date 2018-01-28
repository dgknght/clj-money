(ns clj-money.models.transactions
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clojure.set :refer [difference]]
            [clj-time.core :as t]
            [clj-money.util :refer [pprint-and-return
                                    ensure-local-date]]
            [clj-money.coercion :as coercion]
            [clj-money.validation :as validation]
            [clj-money.authorization :as authorization]
            [clj-money.models.accounts :as accounts]
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
    (update-in [:action] name)
    (remove-empty-strings :memo)
    (update-in [:negative] (fnil identity false))))

(defn- after-item-read
  "Makes adjustments to a transaction item in prepartion for return
  from the data store"
  ([item] (after-item-read item nil))
  ([{:keys [amount negative reconciliation-status] :as item} account]
   (if (map? item)
     (-> item
         (update-in [:action] keyword)
         (assoc :reconciled? (= "completed" reconciliation-status)
                :polarized-amount (if negative
                                    (* -1 amount)
                                    amount)))
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
        (update-in [:value] #(or % (:amount item)))
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

(defn- before-validation
  "Performs operations required before validation"
  [transaction]
  (-> (coercion/coerce coercion-rules transaction)
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
                         (map :value)
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
           (->> (select-transaction-items storage
                                          {:transaction-id id
                                           :transaction-date transaction-date}
                                          {:sort [[:action :desc] [:amount :desc]]})
                (mapv after-item-read)))))

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

(defn- upsert-item
  "Updates the specified transaction item"
  [storage {:keys [id transaction-date] :as item}]
  (if (:id item)
    (do
      (update-transaction-item storage item)
      (find-item-by-id storage id transaction-date))
    (create-transaction-item storage item)))

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
   #'transaction-dates-must-match
   (validation/create-rule (partial no-reconciled-items-changed? storage)
                           [:items]
                           "A reconciled transaction item cannot be changed")])

(defn- validate
  [storage spec transaction]
  (->> transaction
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

(defn- prepare-criteria
  [criteria]
  (if (:transaction-date criteria)
    (update-in criteria [:transaction-date] parse-date-range)
    criteria))

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

(defn- create-transaction*
  [storage transaction]
  (assoc (->> transaction
              before-save
              (create-transaction storage))
         :items (:items transaction)
         :lot-items (:lot-items transaction)))

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

(defn- recalculate-account-item
  "Accepts a processing context and an item, updates
  the items :index and :balance attributes, and returns
  the context with the updated :last-index and :last-balance
  values"
  [{:keys [account storage] :as context} item]
  (let [new-index (+ 1 (:last-index context))
        polarized-amount (accounts/polarize-amount item account)
        new-balance (+ (:last-balance context)
                       polarized-amount)
        changed (update-item-index-and-balance
                  storage
                  (assoc item
                         :negative (< polarized-amount 0M)
                         :index new-index
                         :balance new-balance))]
    ; if the index and balance didn't change, we can short circuit the update
    (if changed
      (assoc context
             :last-index new-index
             :last-balance new-balance)
      (-> context
          (dissoc :last-balance)
          reduced))))

(defn recalculate-account-items
  "Accepts a tuple containing an account-id and a base item,
  selects all subsequent items and recalculates the items until
  all item for the account have been recalculated or until
  a balance is unchanged.
  Returns the new balance of the account of the balance changed,
  or nil if te balance didn't change."
  [storage {:keys [account-id transaction-date index balance id] :as item}]
  (let [account (accounts/find-by-id storage account-id)
        items (search-items storage
                            (cond-> {:account-id account-id
                                     :index [:> index]}
                              transaction-date
                              (assoc :transaction-date [:>= transaction-date]))
                            {:sort [:transaction-date :index]})
        result (reduce recalculate-account-item
                       {:account account
                        :storage storage
                        :last-index index
                        :last-balance balance}
                       (remove #(= id (:id %)) items))]
    (when-let [last-balance (:last-balance result)]
      (accounts/update storage (assoc account :balance last-balance)))))

(defn- link-lots
  [storage transaction]
  (when-let [lot-items (:lot-items transaction)]
    (doseq [lot-item (:lot-items transaction)]
      (create-lot->transaction-link storage
                                    (assoc lot-item
                                           :transaction-id
                                           (:id transaction)))))
  transaction)

(defn- create-transaction-and-adjust-balances
  [storage transaction]
  (let [created (->> transaction
                     (create-transaction* storage)
                     (link-lots storage))]
    (doall (->> (:items transaction)

                ; create database records
                (map #(assoc % :transaction-id (:id created)))
                (map #(create-transaction-item* storage %))

                ; process indexes and balances, update affected accounts
                (group-by :account-id)
                (map second)
                (map #(sort-by :index %))
                (map first)
                (map #(get-previous-item storage %))
                (map #(recalculate-account-items storage %))))
    (reload storage created)))

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

(defn- process-item-upserts
  "Process items in a transaction update operation"
  [storage items]
  (->> items
       (map before-save-item)
       (mapv #(upsert-item storage %))))

(defn reload
  "Returns an updated copy of the transaction"
  [storage-spec {:keys [id transaction-date]}]
  (find-by-id storage-spec id transaction-date))

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
      (delete-transaction-item storage id (:transaction-date transaction)))
    (->> dereferenced-account-ids
         ; fake out an item because that's what get-previous-item expects
         (map #(hash-map :account-id %
                         :id (UUID/fromString "00000000-0000-0000-0000-000000000000")
                         :transaction-date (:transaction-date transaction)))
         (map #(or (get-previous-item storage %)
                   (assoc % :index -1 :balance (bigdec 0)))))))

(defn process-dereferenced-items
  "Removes transaction items that have been
  removed from the transaction and returns
  base items for the affected accounts"
  [{:keys [storage transaction existing] :as context}]
  (let [base-items (->> (:items existing)
                        (remove (fn [{existing-item-id :id}]
                                  (some #(= existing-item-id
                                            (:id %))
                                        (:items transaction))))
                        (map (fn [item]
                               (delete-transaction-item
                                 storage
                                 (:id item)
                                 (:transaction-date item))
                               item))
                        (mapv #(get-previous-item storage %)))]
    (update-in context [:base-items] #(concat % base-items))))

(defn- process-dereferenced-accounts
  "Find accounts IDs that are no longer referenced
  by the transaction and looks up base items for
  recalculating affected account items"
  [{:keys [storage transaction existing] :as context}]
  (let [dereferenced-account-ids (apply difference (->> [existing transaction]
                                                        (map :items)
                                                        (map #(map :account-id %))
                                                        (map set)))
        base-items (->> dereferenced-account-ids
                        (map #(hash-map :account-id %
                                        :transaction-date (:transaction-date transaction) ; TODO: get the earlier date
                                        :index (Integer/MAX_VALUE)))
                        (map #(get-previous-item storage %)))]
    (update-in context [:base-items] #(concat % base-items))))

(defn- older-item
  "Given a transaction item and an existing transaction
  (in the context of processing a transaction update),
  returns the earlier of the specified item
  or the corresponding item from the
  existing transaction"
  [existing-tx current-item]
  (if-let [existing-item (some #(= (:id %) (:id current-item))
                               (:items existing-tx))]
    (if (> 0 (compare (:transaction-date current-item)
                      (:transaction-date existing-tx)))
      existing-item 
      current-item)
    current-item))

(defn- process-updated-items
  "Processes the updated items in the transaction
  in the context and appends items to the :base-items attribute
  with the list of items that need to be processed for
  index and balance."
  [{:keys [storage transaction existing-tx] :as context}]
  (let [base-items (->> (:items transaction)
                        (map #(as-> % i
                                (assoc i :transaction-id (:id transaction))
                                (before-save-item i)
                                (upsert-item storage i)))
                        (map #(older-item existing-tx %))
                        (mapv #(get-previous-item storage %)))]
    (update-in context [:base-items] #(concat % base-items))))

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

(defn- assoc-existing-transaction
  [{:keys [storage transaction] :as context}]
  (assoc context :existing (find-existing-transaction storage transaction)))

(defn- recalculate-indexes-and-balances-for-update
  [{:keys [storage base-items] :as context}]
  (doseq [item base-items]
    (recalculate-account-items storage item))
  context)

(defn- update-transaction*
  [{:keys [storage transaction] :as context}]
  (->> transaction
       before-save
       (update-transaction storage))
  (assoc context :transaction (reload storage transaction)))

(defn update
  "Updates the specified transaction"
  [storage-spec transaction]
  (with-transacted-storage [storage storage-spec]
    (let [validated (validate storage ::existing-transaction transaction)]
      (if (validation/has-error? validated)
        validated
        (-> {:transaction validated
             :storage storage
             :base-items []}
            assoc-existing-transaction
            process-dereferenced-items
            process-dereferenced-accounts
            process-updated-items
            recalculate-indexes-and-balances-for-update
            update-transaction*
            :transaction)))))

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
       (map #(or (get-previous-item storage %)
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
  [storage-spec transaction-id transaction-date]
  (with-storage [s storage-spec]
    (let [transaction (find-by-id s transaction-id transaction-date)
          _ (ensure-deletable transaction)
          preceding-items (get-preceding-items s transaction)]
      (delete-transaction-items-by-transaction-id s transaction-id transaction-date)
      (delete-transaction s transaction-id transaction-date)
      (doseq [item preceding-items]
        (recalculate-account-items s item)))))

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
            (fnil #(assoc % :delay-balances? true
                          :delayed-account-ids #{})
                  {}))
     (let [result# (do ~@body)]

       ; Recalculate balances for affected accounts
       (->> (get-in @ambient-settings [~entity-id :delayed-account-ids])
            (map #(hash-map :account-id % :index -1 :balance 0M))
            (map #(recalculate-account-items ~storage-spec %))
            doall)

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
