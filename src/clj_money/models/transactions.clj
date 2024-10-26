(ns clj-money.models.transactions
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.set :refer [difference
                                 rename-keys]]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clojure.core.async :as a]
            [java-time.api :as t]
            [config.core :refer [env]]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage with-transacted-storage]]
            [dgknght.app-lib.core :refer [uuid
                                          index-by]]
            [dgknght.app-lib.models :refer [->id]]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :as util]
            [clj-money.dates :as dates]
            [clj-money.transactions :as trxs]
            [clj-money.models :as models]
            [clj-money.models.settings :as settings]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.prices :as prices]
            [clj-money.models.lot-transactions :as l-t]
            [clj-money.models.lots :as lots]
            [clj-money.models.entities :as entities]
            [clj-money.accounts :refer [polarize-quantity]]))

(declare reload)

(defn- no-reconciled-quantities-changed?
  [transaction]
  (let [existing (models/find transaction)
        submitted (->> (:transaction/items transaction)
                       (filter :id)
                       (map #(select-keys % [:id
                                             :transaction-item/quantity
                                             :transaction-item/account
                                             :transaction-item/action]))
                       (index-by :id))
        invalid (->> (:transaction/items existing)
                     (filter :transaction-item/reconciled?)
                     (map (comp #(hash-map :before %
                                           :after (submitted (:id %)))
                                #(select-keys % [:id
                                                 :transaction-item/quantity
                                                 :transaction-item/account
                                                 :transaction-item/action])))
                     (remove #(= (:before %) (:after %))))]
    (log/warnf "Attempt to change reconciled item(s): %s %s %s"
               (->> invalid
                    (map #(assoc %
                                 :account (:account/name
                                            (models/find
                                              (get-in % [:before
                                                         :transaction-item/account
                                                         :id])
                                              :account))))
                    (into []))
               submitted
               existing)
    (empty? invalid)))

(v/reg-spec no-reconciled-quantities-changed? {:message "A reconciled quantity cannot be updated"
                                               :path [:transaction/items]})

(defn- not-a-trading-transaction?
  [{:keys [id] :transaction/keys [original-transaction-date]}]
  (zero? (models/count {:lot-transaction/transaction  {:id id}
                        :lot-transaction/transaction-date original-transaction-date})))

(v/reg-spec not-a-trading-transaction? {:message "A trading transaction cannot be updated."
                                        :path []})

(defn- sum-by
  "Returns the sum of values of the items in the transaction having
  the specified action"
  [attr items]
  (->> items
       (map attr)
       (reduce + 0M)))

(defn- sum-of-credits-equals-sum-of-debits?
  [items]
  (->> items
       (group-by :transaction-item/action)
       (map (comp #(sum-by :transaction-item/value %)
                  second))
       (apply =)))
(v/reg-msg sum-of-credits-equals-sum-of-debits? "Sum of debits must equal the sum of credits")

(defn- transaction-dates-match?
  [{:transaction/keys [transaction-date items]}]
  (->> items
       (map :transaction-item/transaction-date)
       (apply = transaction-date)))
(v/reg-msg transaction-dates-match? "All transaction items must have the same date as the transaction")

(s/def :transaction-item/account ::models/model-ref)
(s/def :transaction-item/action #{:debit :credit})
(s/def :transaction-item/quantity v/big-dec-not-less-than-zero?)
; Balance is the running total of quantities for the account to which
; the item belongs
(s/def :transaction-item/balance (partial instance? BigDecimal))
; Value is the value of the line item expressed in the entity's
; default commodity. For most transactions, this will be the same
; as the quantity. For transactions involving foreign currencies
; and commodity purchases (like stock trades) it will be different.
(s/def :transaction-item/value v/positive-big-dec?)
(s/def :transaction-item/memo (s/nilable string?))
(s/def :transaction-item/index integer?)

(s/def :transaction/description v/non-empty-string?)
(s/def :transaction/transaction-date t/local-date?)
(s/def ::id uuid?)
(s/def :transaction/entity ::models/model-ref)
(s/def :transaction-lot-item/lot ::models/model-ref)
(s/def :transaction-lot-item/lot-action #{:buy :sell})
(s/def :transaction-lot-item/shares decimal?)
(s/def ::models/lot-item (s/keys :req [:transaction-lot-item/lot
                                       :transaction-lot-item/shares
                                       :transaction-lot-item/lot-action
                                       :transaction-lot-item/price]))
(s/def :transaction/lot-items (s/coll-of ::models/lot-item))

(s/def ::models/transaction-item (s/keys :req [:transaction-item/account
                                               :transaction-item/action
                                               :transaction-item/quantity]
                                         :opt [:transaction-item/balance
                                               :transaction-item/index
                                               :transaction-item/memo]))
(s/def :transaction/items (s/and (s/coll-of ::models/transaction-item :min-count 2)
                                 sum-of-credits-equals-sum-of-debits?))
(s/def ::models/transaction (s/keys :req [:transaction/description
                                          :transaction/transaction-date
                                          :transaction/items
                                          :transaction/entity]
                                    :opt [:transaction/memo
                                          :transaction/lot-items]))

(s/def ::existing-transaction (s/and (s/keys :req-un [::id
                                               ::transaction-date
                                               ::items]
                                      :opt-un [::entity-id
                                               ::memo
                                               ::lot-items])
                                     not-a-trading-transaction?
                                     no-reconciled-quantities-changed?
                                     transaction-dates-match?))

(def ambient-settings
  (atom {}))

(defn- delay-balances?
  [entity-id]
  (get-in @ambient-settings [entity-id]))

(defn- remove-empty-strings
  [model & keys]
  (reduce (fn [m k]
            (if (and (string? (k m))
                     (empty? (k m)))
              (dissoc m k)
              m))
          model
          keys))

(defmethod models/before-validation :transaction
  [trx]
  (update-in trx
             [:transaction/items]
             (fn [items]
               (map (fn [{:as item :transaction-item/keys [quantity]}]
                      (-> item
                          (update-in [:transaction-item/value]
                                     (fnil identity quantity)) ; TODO need to calculate the correct value
                          (remove-empty-strings :transaction-item/memo)))
                    items))))

(defn- after-item-read
  "Makes adjustments to a transaction item in prepartion for return
  from the data store"
  [{:keys [quantity negative reconciliation-status] :as item}]
  (if (map? item)
    (-> item
        (tag ::models/transaction-item)
        (update-in [:action] keyword)
        (assoc :reconciled? (= "completed" reconciliation-status)
               :polarized-quantity (if negative
                                     (* -1 quantity)
                                     quantity)))
    item))

(defn- before-item-validation
  [item]
  (cond->
   (-> item
       (update-in [:value] #(or % (:quantity item)))
       (assoc :balance (bigdec 0))
       (update-in [:index] (fnil identity Integer/MAX_VALUE)))

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
    (update-in [:id] uuid)))

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
      (update-in [:items] (fn [items]
                            (->> items
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

(defmethod models/before-save :transaction
  [trx]
  (assoc trx :transaction/value (trxs/value trx)))

(defn- fetch-lot-items
  [transaction-id transaction-date]
  (l-t/search {:transaction-id transaction-id
               :transaction-date transaction-date}))

(defn search-items
  "Returns transaction items matching the specified criteria"
  ([criteria]
   (search-items criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (map after-item-read
          (storage/select (tag criteria ::models/transaction-item)
                          options)))))

(defn- append-items
  [{:keys [id] :as transaction} items]
  (assoc transaction
         :items
         (get-in items [id])))

(defn- append-lot-items
  [transaction]
  (when transaction
    (if-let [lot-items (seq (fetch-lot-items (:id transaction)
                                             (:transaction-date transaction)))]
      (assoc transaction
             :lot-items
             (->> lot-items
                  (mapv #(-> %
                             (dissoc :transaction-id)
                             (update-in [:lot-action] keyword)))))
      transaction)))

(defn- after-read
  "Returns a transaction that is ready for public use"
  ([transaction]
   (after-read transaction {}))
  ([{:keys [transaction-date] :as transaction}
    {:keys [include-items?
            items
            include-lot-items?]}]
   (when transaction
     (cond-> (-> transaction
                 (assoc :original-transaction-date transaction-date)
                 (tag ::models/transaction))
       include-items?     (append-items items)
       include-lot-items? append-lot-items))))

(defn find-item-by
  "Returns the first item matching the specified criteria"
  ([criteria]
   (find-item-by criteria {}))
  ([criteria options]
   (first (search-items criteria (merge options {:limit 1})))))

(defn find-item
  ([{:keys [id transaction-date]}]
   (find-item id transaction-date))
  ([id transaction-date]
   (find-item-by {:id id
                  :transaction-date transaction-date})))

; This is public to support the unit test
(defn ^:deprecated upsert-item
  "Updates the specified transaction item"
  [_]
  (throw (UnsupportedOperationException. "upsert-item is deprecated"))
  #_(let [to-save (before-save-item item)]
    (if id
      (do
        (storage/update to-save)
        (find-item id transaction-date))
      (storage/create to-save))))

(defn update-items
  [attr criteria]
  (with-storage (env :db)
    (storage/update (tag attr ::models/transaction-item) criteria)))

(defn ^:deprecated update-item-index-and-balance
  "Updates only the index and balance of an item, returning true if
  the values where changed as a result of the update, or false if the specified
  values match the existing values"
  [_]
  (throw (UnsupportedOperationException. "update-item-index-and-balance is deprecated"))
  #_(with-storage (env :db)
    (let [records-affected (storage/update
                            (-> item
                                before-save-item
                                (select-keys [:balance
                                              :index
                                              :negative]))
                            [:and
                             {:id (:id item)
                              :transaction-date (some #(% item)  [:original-transaction-date
                                                                  :transaction-date])}
                             [:or
                              {:balance [:!= (:balance item)]}
                              {:index [:!= (:index item)]}]])]
      (> records-affected 0))))

(defn- validate
  [transaction spec]
  (-> transaction
      before-validation
      (v/validate spec)
      after-validation))

(defn- transactions->item-criteria
  [transactions]
  (tag
    (reduce (fn [m {:keys [id transaction-date]}]
              (-> m
                  (update-in [:transaction-id] conj id)
                  (update-in [:transaction-date 1] #(first (sort t/before? (filter identity [% transaction-date]))))
                  (update-in [:transaction-date 2] #(first (sort t/after? (filter identity [% transaction-date]))))))
            {:transaction-date [:between nil nil]
             :transaction-id []}
            transactions)
    ::models/transaction-item))

(defn- compare-items
  [i1 i2]
  (reduce (fn [result k]
            (if (zero? result)
              (compare (get-in i1 [k])
                       (get-in i2 [k]))
              (reduced result)))
          0
          [:action :quantity]))

(defn- sort-items
  [items]
  (->> items
       (sort-by (juxt :action :quantity) compare-items)
       (into [])))

(defn search
  "Returns the transactions that belong to the specified entity"
  ([criteria]
   (search criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (let [transactions (storage/select (tag criteria ::models/transaction)
                                        options)
           items (when (and (:include-items? options)
                            (seq transactions))
                   (->> (storage/select (transactions->item-criteria transactions) {})
                        (map after-item-read)
                        (group-by :transaction-id)
                        (map #(update-in % [1] sort-items))
                        (into {})))]
       (map #(after-read % (assoc options :items items)) transactions)))))

(defn find-by
  ([criteria] (find-by criteria {}))
  ([criteria options]
   (first (search criteria (assoc options :limit 1)))))

(defn select-items-by-reconciliation
  "Returns the transaction items associated with the specified reconciliation"
  [reconciliation]
  (with-storage (env :db)
    (map after-item-read
         (search-items {:reconciliation-id (:id reconciliation)
                        :transaction-date [:between
                                           (t/minus (:end-of-period reconciliation) (t/years 1))
                                           (t/plus (:end-of-period reconciliation) (t/months 1))]}
                       {:sort [[:transaction-date :desc] [:index :desc]]}))))

(defn record-count
  "Returns the number of transactions that match the specified criteria"
  [criteria]
  (with-storage (env :db)
    (search criteria {:count true})))

#_(defn- create-transaction-item*
  [item]
  (->> item
       before-save-item
       storage/create
       after-item-read))

(defn- link-lots
  [{:keys [id transaction-date] :as trans} lot-items]
  (mapv (comp l-t/create
              #(assoc %
                      :transaction-id id
                      :transaction-date transaction-date))
        lot-items)
  trans)

(defn- find-base-item
  "Given an account ID and a date, finds the transaction item for that
  account that immediately precents the given date"
  [account-id as-of]
  ; TODO If no item is found, we need to know if it's because
  ; there is no item in this partition, or no item at all.
  ; I'm not sure of the storage layer is already walking back,
  ; but I'm guessing it is not.
  (find-item-by {:transaction-date [:< as-of]
                 :account-id account-id}
                {:sort [[:transaction-date :desc]
                        [:index :desc]]}))

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

  [account
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
          (update-item-index-and-balance (assoc item
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
  (fn [_balance {:keys [system-tags]}]
    (system-tags :tradable)))

(defmethod ^:private account-value :default
  [balance _account]
  balance)

(defmethod ^:private account-value :tradable
  [balance {:keys [commodity-id
                   earliest-transaction-date
                   latest-transaction-date]}]
  (when (and earliest-transaction-date latest-transaction-date)
    (if-let [price (first (prices/search {:commodity-id commodity-id
                                          :trade-date [:between
                                                       earliest-transaction-date
                                                       latest-transaction-date]}
                                         {:sort [[:trade-date :desc]]
                                          :limit 1}))]
      (* (:price price) balance)
      0M)))

(defn recalculate-account
  "Recalculates statistics for items in the the specified account
  as of the specified date"
  ([account-or-id as-of]
   (recalculate-account account-or-id as-of {}))
  ([account-or-id as-of options]
   {:pre [account-or-id]}

   (let [account (if (map? account-or-id)
                   account-or-id
                   (accounts/find account-or-id))
         _ (assert account "Unable to find the account.")
         base-item (find-base-item (:id account) as-of)
         items (search-items {:account-id (:id account)
                              :transaction-date [:>= as-of]}
                             {:sort [:transaction-date :index]})
         earliest-date (:transaction-date (first items))
         [last-index
          balance
          last-date] (if (seq items)
                       (process-items account base-item items options)
                       (if base-item
                         ((juxt :index :quantity) base-item)
                         [0 0M]))]
     (when (not (nil? last-index))
       (let [value (account-value
                     balance (-> account
                                 (update-in [:earliest-transaction-date] dates/earliest earliest-date)
                                 (update-in [:latest-transaction-date] dates/latest last-date)))
             updated (-> account
                         (assoc :quantity balance
                                :value value)
                         (update-in [:earliest-transaction-date] dates/earliest earliest-date)
                         (update-in [:latest-transaction-date] dates/latest last-date))]
         (log/debugf "update account summary data \"%s\""
                     (select-keys account [:name
                                           :balance
                                           :value
                                           :earliest-transaction-date
                                           :latest-transaction-date]))
         (accounts/update updated))))))

(defn migrate-account
  "Moves all transaction items from from-account to to-account and recalculates the accounts"
  [from-account to-account]
  {:pre [(= (:entity-id from-account)
            (:entity-id to-account))]}
  (let [entity (entities/find (:entity-id from-account))
        as-of (or (->> [from-account to-account]
                       (map :earliest-transaction-date)
                       (filter identity)
                       sort
                       first)
                  (get-in entity [:settings :earliest-transaction-date])
                  (settings/get :earliest-partition-date))]
    (assert as-of "Unable to find the earliest transaction date.")
    (with-transacted-storage (env :db)
      (storage/update (tag {:account-id (:id to-account)
                            :index 0
                            :balance nil}
                           ::models/transaction-item)
                      {:account-id (:id from-account)
                       :transaction-date [:>= as-of]})
      (doseq [account [from-account to-account]]
        (recalculate-account account as-of {:force true})))))

(defn- extract-account-ids
  [transaction]
  (->> (:items transaction)
       (map :account-id)
       (into #{})))

(defn- apply-account-ids
  [m entity-id transaction-date account-ids]
  (reduce (fn [r account-id]
            (-> r
                (update-in [entity-id :accounts account-id :earliest-date]
                           dates/earliest
                           transaction-date)
                (update-in [entity-id :accounts account-id :latest-date]
                           dates/latest
                           transaction-date)))
          m
          account-ids))

(defn- save-delayed-info
  [m entity-id account-ids transaction-date]
  (-> m
      (apply-account-ids entity-id transaction-date account-ids)
      (update-in [entity-id :earliest-date] dates/earliest transaction-date)
      (update-in [entity-id :latest-date] dates/latest transaction-date)))

#_(defn- update-transaction-date-boundaries
  [{:transaction/keys [entity transaction-date]}]
  (let [entity (entities/find entity-id)
        updated (-> entity
                    (update-in [:settings :earliest-transaction-date] dates/earliest transaction-date)
                    (update-in [:settings :latest-transaction-date] dates/latest transaction-date)
                    )]
    (when-not (= entity updated)
      (entities/update updated))))

#_(defn- post-create
  [{:keys [entity-id transaction-date] :as transaction}]
  (let [account-ids (extract-account-ids transaction)]
    (if (delay-balances? entity-id)
      (swap! ambient-settings
             save-delayed-info
             entity-id
             account-ids
             transaction-date)
      (do
        (doseq [account-id account-ids]
          (recalculate-account account-id transaction-date))
        (update-transaction-date-boundaries transaction))))
  transaction)

#_(defn- process-item-creation
  [trans items]
  (assoc trans :items (mapv
                       #(-> %
                            (assoc
                             :transaction-id (:id trans)
                             :transaction-date (:transaction-date trans)
                             :index -1)
                            before-save-item
                            create-transaction-item*)
                       items)))

(defn ^:deprecated create
  "Creates a new transaction"
  [_transaction]
  (throw (UnsupportedOperationException. "Deprecated. Use clj-money.models/put instead.")))
  #_(with-transacted-storage (env :db)
    (let [validated (validate transaction ::new-transaction)]
      (if (v/has-error? validated)
        validated
        (-> validated
            before-save
            storage/create
            (link-lots (:lot-items validated))
            (process-item-creation (:items validated))
            post-create
            reload))))

(defn find
  "Returns the specified transaction"
  ([model]
   {:pre [(:transaction-date model)
          (some #(% model) [:id :transaction-id])]}

   (let [{:keys [id transaction-date]} (rename-keys model {:transaction-id :id})]
     (find id transaction-date)))
  ([id transaction-date]
   {:pre [id transaction-date]}

   (find-by {:id id
             :transaction-date transaction-date}
            {:include-items? true
             :include-lot-items? true})))

(defn find-by-item-id
  "Returns the transaction that has the specified transaction item"
  [item-id transaction-date]
  (with-storage (env :db)
    (when-let [{:keys [transaction-id
                       transaction-date]} (find-item
                                           item-id
                                           transaction-date)]
      (find transaction-id transaction-date))))

(defn items-by-account
  "Returns the transaction items for the specified account"
  ([account-or-id date-spec]
   (items-by-account account-or-id date-spec {}))
  ([account-or-id date-spec options]
   (search-items {:account-id (->id account-or-id)
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
  [account-id]
  (search {:account-id account-id
           :reconciliation-id nil}))

(defn reload
  "Returns an updated copy of the transaction"
  [transaction]
  (find transaction))

(defn- find-existing-transaction
  "Given a transaction that has been updated, find the existing
  transaction in storage. If none can be found, throw an exception."
  [{:keys [id transaction-date original-transaction-date]}]
  (let [search-date (or original-transaction-date transaction-date)]
    (or (find id search-date)
        (throw (ex-info
                (format "Unable to find transaction with id %s and date %s"
                        id
                        search-date)
                {:id id
                 :search-date search-date})))))

#_(defn- update-transaction*
  [transaction]
  (->> transaction
       before-save
       storage/update))

#_(defn- update-full-transaction
  "Update the transaction and associated items."
  [transaction]
  (update-transaction* transaction)
  (doseq [item (:items transaction)]
    (as-> item i
      (assoc i :transaction-id (:id transaction))
      (upsert-item i))))

(defn- update-lots
  [{:keys [transaction-date] :as after} before]
  (when-not (->> [before after]
                 (map :transaction-date)
                 (apply =))
    (when-let [lot-item (->> (fetch-lot-items (:id after) transaction-date)
                             (filter #(= :buy (:lot-action %)))
                             first)]
      (l-t/update (assoc lot-item :transaction-date transaction-date))
      (lots/update (assoc (lots/find (:lot-id lot-item))
                          :purchase-date transaction-date)))))

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

(defn ^:deprecated update
  "Updates the specified transaction"
  [_transaction]
  (throw (Exception. "Deprecated. Use clj-money.models/put instead"))
  #_(with-transacted-storage (env :db)
    (let [validated (validate transaction ::existing-transaction)]
      (if (v/has-error? validated)
        validated
        (let [existing (find-existing-transaction validated)
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
              recalc-base-date (dates/earliest (:transaction-date existing)
                                               (:transaction-date validated))
              recalc-account-ids (->> (:items validated)
                                      (map :account-id)
                                      (concat dereferenced-account-ids)
                                      (into #{}))]
          (update-full-transaction validated)
          (update-lots validated existing)
          (doseq [item dereferenced-items]
            (storage/delete item))
          (doseq [account-id recalc-account-ids]
            (recalculate-account account-id recalc-base-date))
          (reload validated))))))

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
  [{:keys [id transaction-date]}]
  (with-transacted-storage (env :db)
    (let [transaction (find id transaction-date)
          lot (lots/find-by {[:lot-transaction :transaction-id] id
                             [:lot-transaction :lot-action] "buy"})]
      (ensure-deletable transaction)
      (storage/delete transaction)
      (when lot
        (storage/delete lot))
      (doseq [account-id (extract-account-ids transaction)]
        (recalculate-account account-id (:transaction-date transaction))))))

(defn- find-last-item-before
  [account date]
  (find-item-by {:account-id (:id account)
                 :transaction-date [:between (:earliest-transaction-date account) (t/minus date (t/days 1))]}
                {:sort [[:transactions.transaction-date :desc] [:transaction_items.index :desc]]}))

(defn- find-last-item-on-or-before
  [account date]
  (find-item-by {:account-id (:id account)
                 :transaction-date [:between (:earliest-transaction-date account) date]}
                {:sort [[:transactions.transaction-date :desc]
                        [:transaction_items.index :desc]]}))

(defn balance-delta
  "Returns the change in balance during the specified period for the specified account"
  [account start end]
  (let [t1 (find-last-item-before account start)
        t2 (find-last-item-on-or-before account end)]
    (- (or (:balance t2) 0M)
       (or (:balance t1) 0M))))

(defn balance-as-of
  "Returns the balance for the specified account as of the specified date"
  [account as-of]
  (or (:balance
       (find-last-item-on-or-before account as-of))
      0M))

(defn find-items-by-ids
  [ids date-range]
  (search-items
   {:id ids
    :transaction-date (vec (concat [:between] date-range))}))

(defmacro with-delayed-balancing
  [entity-id progress-chan & body]
  `(do
     ; Make a note that balances should not be calculated
     ; for this entity
     (swap! ambient-settings
            assoc
            ~entity-id
            {})
     (let [result# (do ~@body)]
       ; Recalculate balances for affected accounts
       (let [accounts# (get-in @ambient-settings [~entity-id :accounts])
             progress# {:total (count accounts#)
                        :completed 0}]
         (a/>!! ~progress-chan progress#)
         (with-storage (env :db)
           (let [dates# (->> accounts#
                             (interleave (iterate inc 1))
                             (partition 2)
                             (map (fn [[index# [account-id# {earliest-date# :earliest-date}]]]
                                    (let [recalculated# (recalculate-account account-id# earliest-date#)]
                                      (a/>!! ~progress-chan (assoc progress# :completed index#))
                                      recalculated#)))
                             (reduce (fn [m# a#]
                                       (-> m#
                                           (update-in [:earliest-transaction-date]
                                                      dates/earliest
                                                      (:earliest-transaction-date a#))
                                           (update-in [:latest-transaction-date]
                                                      dates/latest
                                                      (:latest-transaction-date a#))))
                                     {}))]
             (-> (entities/find ~entity-id)
                 (update-in [:settings] merge dates#)
                 entities/update)))
         (a/close! ~progress-chan))

       ; clean up the ambient settings as if we were never here
       (swap! ambient-settings dissoc ~entity-id)
       result#)))

(defn- find-account []
  (let [accounts (atom {})]
    (fn [ref]
      (if (util/model-ref? ref)
        (if-let [account (@accounts ref)]
          account
          (if-let [account (models/find ref :account)]
            (do
              (swap! accounts assoc ref account)
              account)
            (throw (ex-info "Account not found" {:ref ref}))))
        ref))))

(defn- push-date-boundaries
  [model date early-ks late-ks]
  (-> model
      (update-in early-ks dates/earliest date)
      (update-in late-ks dates/latest date)))

(defn- propagate-item
  [{:transaction/keys [transaction-date]}]
  (fn [ctx {:as item :transaction-item/keys [account quantity value]}]
    ; TODO: polarize the quantity
    (-> ctx
        (update-in [:items]
                   conj
                   (assoc item
                          :transaction-item/index 0
                          :transaction-item/balance quantity))
        (update-in [:accounts]
                   conj
                   (-> account
                       (assoc :account/quantity quantity
                              :account/value value)
                       (push-date-boundaries transaction-date
                                             [:account/earliest-transaction-date]
                                             [:account/latest-transaction-date]))))))

(defn- propagate-items
  [{:transaction/keys [items] :as trx}]
  (let [find-act (find-account)]
    (->> items
         (map #(update-in % [:transaction-item/account] find-act))
         (reduce (propagate-item trx)
                 {:accounts #{}
                  :items []}))))

(defmethod models/propagate :transaction
  [{:as trx :transaction/keys [transaction-date]}]
  (let [{:keys [accounts items]} (propagate-items trx)
        entity (push-date-boundaries (models/find (:transaction/entity trx)
                                                  :entity)
                                     transaction-date
                                     [:entity/settings
                                      :settings/earliest-transaction-date]
                                     [:entity/settings
                                      :settings/latest-transaction-date])]
    (concat [(assoc trx :transaction/items items)
             entity]
            accounts)))
