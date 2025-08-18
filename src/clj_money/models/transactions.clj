(ns clj-money.models.transactions
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.core.async :as a]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [index-by
                                          update-in-if]]
            [dgknght.app-lib.validation :as v]
            [clj-money.db :as db]
            [clj-money.util :as util :refer [id=]]
            [clj-money.dates :as dates]
            [clj-money.transactions :as trxs]
            [clj-money.models :as models]
            [clj-money.models.propagation :as prop]
            [clj-money.accounts :as acts]))

(defn- no-reconciled-quantities-changed?
  [{:transaction/keys [items] :as trx}]
  (if (:id trx)
    (let [after (->> items
                     (map (juxt :id #(select-keys % [:id
                                                     :transaction-item/quantity
                                                     :transaction-item/action])))
                     (into {}))]
      (->> (models/select #:transaction-item{:transaction-date (:transaction/transaction-date trx)
                                             :transaction trx
                                             :reconciliation [:!= nil]})
           (map #(select-keys % [:id
                                 :transaction-item/quantity
                                 :transaction-item/action]))
           (remove #(= % (after (:id %))))
           empty?))
    true))

(v/reg-spec no-reconciled-quantities-changed? {:message "A reconciled quantity cannot be updated"
                                               :path [:transaction/items]})

(defn- not-a-trading-transaction?
  [{:keys [id] :transaction/keys [original-transaction-date]}]
  (zero? (models/count {:lot-item/transaction  {:id id}
                        :transaction/transaction-date original-transaction-date})))

(v/reg-spec not-a-trading-transaction? {:message "A trading transaction cannot be updated."
                                        :path []})

(defn- sum-of-credits-equals-sum-of-debits?
  [items]
  (if (= 1 (count items))
    (zero? (:transaction-item/value (first items))) ; a split transaction will have one item with a value of zero
    (let [{:keys [debit credit]}
        (->> items
             (group-by :transaction-item/action)
             (map #(update-in % [1] (fn [itms]
                                      (->> itms
                                           (map :transaction-item/value)
                                           (reduce + 0M)))))
             (into {}))]
    (= debit credit))))

(v/reg-msg sum-of-credits-equals-sum-of-debits? "Sum of debits must equal the sum of credits")

(defn- transaction-dates-match?
  [{:transaction/keys [transaction-date items]}]
  (->> items
       (map :transaction/transaction-date)
       (apply = transaction-date)))
(v/reg-msg transaction-dates-match? "All transaction items must have the same date as the transaction")

(def actions
  "Set of valid transaction action values, includes :debit and :credit"
  #{:debit :credit})

(s/def :transaction-item/account ::models/model-ref)
(s/def :transaction-item/action actions)
(s/def :transaction-item/quantity (s/and decimal? pos?))
; Balance is the running total of quantities for the account to which
; the item belongs
(s/def :transaction-item/balance decimal?)
; Value is the value of the line item expressed in the entity's
; default commodity. For transactions in the default currenty,
; this will be the same as the quantity. For transactions involving
; foreign currencies and commodity purchases (like stock trades)
; it will be different.
;
; A value can be zero for a transaction split. Otherwise, it must
; be positive.
(s/def :transaction-item/value (s/and decimal? (complement neg?)))
(s/def :transaction-item/memo (s/nilable string?))
(s/def :transaction-item/index integer?)

(s/def :transaction/description v/non-empty-string?)
(s/def :transaction/transaction-date t/local-date?)
(s/def :transaction/entity ::models/model-ref)
(s/def :lot-item/lot ::models/model-ref)
(s/def :lot-item/action #{:buy :sell})
(s/def :lot-item/shares decimal?)
(s/def ::models/lot-item (s/keys :req [:lot-item/lot
                                       :lot-item/shares
                                       :lot-item/action
                                       :lot-item/price]))
(s/def :transaction/lot-items (s/coll-of ::models/lot-item))

(s/def ::models/transaction-item (s/keys :req [:transaction-item/account
                                               :transaction-item/action
                                               :transaction-item/quantity]
                                         :opt [:transaction-item/balance
                                               :transaction-item/index
                                               :transaction-item/memo]))
; Most transactions need at least 2 items, but a commodity split
; will only have 1
(s/def :transaction/items (s/and (s/coll-of ::models/transaction-item :min-count 1)
                                 sum-of-credits-equals-sum-of-debits?))
(s/def ::models/transaction (s/and (s/keys :req [:transaction/description
                                                 :transaction/transaction-date
                                                 :transaction/items
                                                 :transaction/entity]
                                           :opt [:transaction/memo
                                                 :transaction/lot-items])
                                   no-reconciled-quantities-changed?))

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
  (-> trx
      trxs/expand
      (update-in [:transaction/items]
                 (fn [items]
                   (mapv (fn [{:as item :transaction-item/keys [quantity]}]
                           (-> item
                               (update-in [:transaction-item/value]
                                          (fnil identity quantity)) ; TODO need to calculate the correct value
                               (remove-empty-strings :transaction-item/memo)))
                         items)))))

(defmethod models/before-save :transaction
  [trx]
  (-> trx
      (dissoc :transaction/original-transaction-date)
      (assoc :transaction/value (trxs/value trx))))

(defn items-by-account
  "Returns the transaction items for the specified account"
  [account & {:as options}]
  (models/select (acts/->criteria account options)
                 {:sort [[:transaction/transaction-date :desc]
                         [:transaction-item/index :desc]]
                  :select-also [:transaction/transaction-date]}))

(defn- last-account-item-before
  [account date]
  (models/find-by
    (util/model-type
      {:transaction-item/account account
       :transaction/transaction-date [:< date]}
      :transaction-item)
    {:sort [[:transaction-item/index :desc]]}))

(defn- last-account-item-on-or-before
  [{:as account :account/keys [transaction-date-range]} date]
  (models/find-by (util/model-type
                    {:transaction-item/account account
                     :transaction/transaction-date [:between
                                                    (first transaction-date-range)
                                                    date]}
                    :transaction-item)
                  {:sort [[:transaction/transaction-date :desc]
                          [:transaction-item/index :desc]]}))

(defn balance-delta
  "Returns the change in balance during the specified period for the specified account"
  [account start end]
  (let [t1 (last-account-item-before account start)
        t2 (last-account-item-on-or-before account end)]
    (- (:transaction-item/balance t2 0M)
       (:transaction-item/balance t1 0M))))

(defn balance-as-of
  "Returns the balance for the specified account as of the specified date"
  [account as-of]
  (or (:transaction-item/balance
       (last-account-item-on-or-before account as-of))
      0M))

(defn- apply-prev
  "Given a transaction item and the previous transaction item,
  update the index and balance attributes of the item."
  [{:as item ::keys [polarized-quantity]}
   {prev-index :transaction-item/index
    prev-balance :transaction-item/balance
    :as prev-item}]
  {:pre [(::polarized-quantity item)
         (:transaction-item/balance prev-item)
         (:transaction-item/index prev-item)]}
  (assoc item
         :transaction-item/index (inc prev-index)
         :transaction-item/balance (+ polarized-quantity prev-balance)))

(def ^:private initial-basis
  #:transaction-item{:index -1
                     :balance 0M})

(defn- propagation-basis
  "Given and account and a date, return the last item before
  the date. If no such item exists, return a dummy item with
  starting index and balance values."
  [account date]
  (or (when (util/live-id? account)
        (last-account-item-before account date))
      initial-basis))

(defn- polarize
  [{:transaction-item/keys [account quantity action] :as item}]
  (assoc item
         ::polarized-quantity
         (acts/polarize-quantity quantity
                                 action
                                 account)))

(defn- default-commodity?
  [{:account/keys [commodity] {{:settings/keys [default-commodity]} :entity/settings} :account/entity}]
  (id= commodity default-commodity))

(defn- fetch-latest-price
  [{:commodity/keys [price-date-range] :as commodity}]
  (when price-date-range
    (:price/value
      (models/find-by
        {:price/commodity commodity
         :price/trade-date (apply vector :between price-date-range)}
        {:sort [[:price/trade-date :desc]]}))))

(defn- re-index
  "Given an account and a list of items, take the index and balance
  of the 1st item and calculate indices and balances forward, then
  apply the final to the account. Returns a sequence of all updated
  models, which may be fewer than the input.

  Additionally, an attempt is made to calculate the ending value of the account
  based on the most recent price of the tracked commodity. This price may be
  supplied in the account at :account/commodity-price, or it will be searched
  and will default to 1M if not found."
  ([account basis items]
   (re-index account basis {} items))
  ([{:as account :account/keys [commodity]} basis {:keys [force?] :or {force? false}} items]
   (let [updated-items (->> items
                            (reduce (fn [output item]
                                      (let [updated (apply-prev item (last output))]
                                        (if (and (= item updated)
                                                 (not force?))
                                          (reduced output)
                                          (conj output updated))))
                                    [basis])
                            (drop 1)
                            (map #(dissoc % ::polarized-quantity)))
         final-qty (or (:transaction-item/balance (last updated-items))
                       (:transaction-item/balance basis))
         price (or (when (default-commodity? account) 1M)
                   (:account/commodity-price account)
                   (fetch-latest-price commodity)
                   (throw (ex-info "No price found for commodity" {:commodity commodity})))]
     (if (= (count updated-items)
            (count items))
       (cons (-> account
                 (dates/push-model-boundary :account/transaction-date-range
                                            (:transaction/transaction-date (last items))
                                            (some :transaction/transaction-date
                                                  (cons basis
                                                        items)))
                 (assoc :account/quantity final-qty
                        :account/value (* final-qty price)))
             updated-items)
       updated-items))))

(defn- account-items-on-or-after
  [account as-of]
  (models/select (util/model-type
                   {:transaction-item/account account
                    :transaction/transaction-date [:>= as-of]}
                   :transaction-item)
                 {:sort [[:transaction-item/index :asc]]}))

(defn- propagate-account-items
  "Returns a function that takes a list of transaction items and returns the
  items along with any other items affected by the transaction, and the updated
  account, if the account is also updated"
  [& {:keys [as-of delete?]}]
  (fn [[_ [{:transaction-item/keys [account]} :as items]]]
    (let [ids (->> items
                   (map :id)
                   set)
          affected-items (if (util/temp-id? account)
                           []
                           (->> (account-items-on-or-after account as-of)
                                (remove #(ids (:id %)))
                                (map #(assoc % :transaction-item/account account))))
          account (update-in (:transaction-item/account (first items))
                             [:account/commodity]
                             models/resolve-ref
                             :commodity)]
      (re-index (if delete?
                  account
                  (dates/push-model-boundary
                    account
                    :account/transaction-date-range
                    as-of))
                (propagation-basis account as-of)
                (->> (cond->> affected-items
                       (not delete?) (concat items))
                     (sort-by :transaction/transaction-date t/before?)
                     (map polarize))))))

(defn- account-model-ref-ids
  "Extracts and returns the account ids from references from the transaction
  items, omitting any complete account models."
  [items]
  (->> items
       (filter #(util/model-ref? (:transaction-item/account %)))
       (map (comp :id
                  :transaction-item/account))
       set
       seq))

; TODO: Need to think some more about how to handle differences
; between the account in the item (when not a simple model-ref) and
; the account read from the database.
; It's passible the caller has made unsaved changes they want to keep.
; It's also possble the account has changed since the specified account
; was read from the database.
(defn- realize-accounts
  "Given a list of items, lookup the associated account and assoc
  it into the item, if the item has only a model reference."
  [entity items]
  (if-let [account-ids (account-model-ref-ids items)]
    (let [accounts (index-by :id
                             (models/select
                               (util/model-type
                                 {:id [:in account-ids]}
                                 :account)))]
      (->> items
           (map #(update-in % [:transaction-item/account] (fn [act]
                                                            (or (accounts (:id act))
                                                                act))))
           (map #(assoc-in % [:transaction-item/account :account/entity] entity))))
    items))

(def ^:private transaction-item?
  (util/model-type? :transaction-item))

(defn- belongs-to-trx?
  [{:keys [id] :as trx}]
  (fn [model]
    (if (transaction-item? model)
      (let [{:transaction-item/keys [transaction] :as item} model]
        (when (and id
                   (not (:id transaction)))
          (pprint {::trx trx
                   ::item item})
          (throw (ex-info "Unexpected transaction item without transaction id" {:transaction trx
                                                                                :item item})))
        (= id (:id transaction)))
      false)))

(defn- propagate-current-items
  "Given a transaction, return a list of accounts and transaction items
  that will also be affected by the operation."
  [[before {:transaction/keys [transaction-date] :keys [id] :as after}]]
  (let [entity (models/find (:transaction/entity after) :entity)]
    (->> (:transaction/items after)
         (map #(cond-> %
                 true (assoc :transaction/transaction-date transaction-date)
                 id   (assoc :transaction-item/transaction {:id id})))
         (realize-accounts entity)
         (group-by (comp util/->model-ref
                         :transaction-item/account))
         (mapcat (propagate-account-items
                   :as-of (dates/earliest
                            transaction-date
                            (:transaction/transaction-date before))
                   :delete? false)))))

(defn- propagate-dereferenced-account-items
  [[before {:transaction/keys [items] :as after}]]
  (let [act-ids (->> items
                     (map (comp :id
                                :transaction-item/account))
                     set)
        entity (models/find (:transaction/entity (or after before)) :entity)]
    (->> (:transaction/items before)
         (remove (comp act-ids
                       :id
                       :transaction-item/account))
         (realize-accounts entity)
         (group-by (comp util/->model-ref
                         :transaction-item/account))
         (mapcat (propagate-account-items
                   :as-of (:transaction/transaction-date before)
                   :delete? true)))))

(defn- propagate-items
  "Given a transaction, return a list of accounts and transaction items
  that will also be affected by the operation."
  [change]
  (concat (propagate-current-items change)
          (propagate-dereferenced-account-items change)))

(defn- propagate-scheduled-transaction
  [{:transaction/keys [transaction-date]
    {:scheduled-transaction/keys [last-occurrence]
     :as sched-trx} :transaction/scheduled-transaction}]
  (when (and last-occurrence
             (t/before? last-occurrence transaction-date))
    (assoc sched-trx :scheduled-transaction/last-occurrence transaction-date)))

(defn- propagate-transaction
  [[_ {:as trx :transaction/keys [transaction-date]} :as change]]
  (let [{transaction-items true
         others false} (group-by (belongs-to-trx? trx)
                                 (propagate-items change))
        entity (-> (:transaction/entity trx)
                   (models/find :entity)
                   (dates/push-model-boundary
                     :entity/transaction-date-range
                     transaction-date))
        updated-sched (propagate-scheduled-transaction trx)]
    (concat (filter identity
                    [entity
                     updated-sched])
            transaction-items
            others)))

(defmethod prop/propagate :transaction
  [[before after :as change]]
  (if after
    (propagate-transaction change)
    (concat (propagate-dereferenced-account-items change)
            (map (fn [i] [::db/delete i])
                 (:transaction/items before)))))

(defmethod models/before-delete :transaction
  [trx]
  (when (and (:id trx)
             (< 0  (models/count {:transaction-item/transaction trx
                                  :transaction/transaction-date (:transaction/transaction-date trx)
                                  :transaction-item/reconciliation [:!= nil]})))
    (throw (IllegalStateException. "Cannot delete transaction with reconciled items")))
  trx)

(defn append-items
  "Given a list of transactions, return the same with the items appended.

  If any of the transactions already has items, the sequence is returned as-is."
  [transactions]
  (let [trxs (seq transactions)]
    (when trxs
      (if (some (comp seq :transaction/items) trxs)
        trxs
        (let [items (group-by :transaction-item/transaction
                              (models/select #:transaction-item{:transaction [:in (map :id trxs)]}))]
          (map #(assoc % :transaction/items (items (:id %)))))))))

(defn- apply-commodities
  [[{:account/keys [entity]} :as accounts]]
  (let [commodities (index-by :id (models/select {:commodity/entity entity}))]
    (map #(update-in % [:account/commodity] (comp commodities
                                                  :id))
         accounts)))

(defn propagate-account-from-start
  [entity account]
  (let [items (->> (models/select {:transaction-item/account account}
                                  {:sort [:transaction/transaction-date
                                          :transaction-item/index]
                                   :select-also [:transaction/transaction-date]})
                   (map (comp polarize
                              #(assoc % :transaction-item/account account)))
                   seq)
        [{:account/keys [transaction-date-range]}
         :as updated] (if items
                        (->> items
                             (re-index (update-in account
                                                  [:account/entity]
                                                  models/resolve-ref
                                                  :entity)
                                       initial-basis
                                       {:force? true})
                             (map (comp #(dissoc % ::polarized-quantity)
                                        #(update-in-if %
                                                       [:transaction-item/account]
                                                       util/->model-ref))))
                        [(assoc account
                                :account/transaction-date-range nil
                                :account/quantity 0M
                                :account/value 0M)])
        [saved-entity] (-> entity
                           (update-in [:entity/transaction-date-range]
                                      #(apply dates/push-boundary
                                              %
                                              transaction-date-range))
                           (cons updated)
                           models/put-many)]
    saved-entity))

(defn propagate-all
  [entity {:keys [progress-chan]}]
  {:pre [entity (map? entity)]}
  (let [accounts (models/select {:account/entity entity})
        total (count accounts)]

    (log/debugf "[propagation] process transactions for %s. %s account(s)"
                (:entity/name entity)
                total)

    (when progress-chan
      (log/debugf "[propagation] report %s accounts" total)
      (a/go (a/>! progress-chan {:declaration/record-type :propagation
                                 :declaration/record-count total
                                 :import/record-type :declaration})))
    (->> accounts
         apply-commodities
         (interleave (map inc (range)))
         (partition-all 2)
         (map (fn [[index account]]
                (log/debugf "[propagation] starting account %s (%d of %d)"
                            (:account/name account)
                            index
                            total)
                account))
         (reduce (comp (fn [entity]
                         (when progress-chan
                           (a/go
                             (a/>! progress-chan
                                   {:import/record-type :propagation})))
                         entity)
                       propagate-account-from-start)
                 entity))))

(prop/add-full-propagation propagate-all :priority 5)

(defn migrate-account
  "Moves all transaction items from from-account to to-account and recalculates the accounts"
  [from-account to-account]
  {:pre [(id= (:account/entity from-account)
              (:account/entity to-account))]}
  (let [entity (models/find (:account/entity from-account) :entity)
        as-of (or (get-in from-account [:account/transaction-date-range 0])
                  (get-in entity [:entity/transaction-date-range 0]))]
    (assert as-of "Unable to find the earliest transaction date.")
    (models/update {:transaction-item/account (util/->model-ref to-account)
                    :transaction-item/index 0
                    :transaction-item/balance nil}
                   {:transaction-item/account (util/->model-ref from-account)
                    :transaction/transaction-date [:>= as-of]})
    (doseq [account [from-account to-account]]
      (propagate-account-from-start entity account))))

(defn propagate-accounts
  "Takes a map of account ids to dates and recalculates indices and balances for those
  accounts as of the associated dates."
  [{:keys [accounts entity-id] :as x}]
  (let [entity (models/find entity-id :entity)]
    (models/put-many
      (cons (update-in entity
                       [:entity/transaction-date-range]
                       #(apply dates/push-boundary % (:entity x)))
            (->> accounts
                 (map (comp (fn [[account date]]
                              {:account account
                               :date date
                               :basis (or (last-account-item-before account date)
                                          initial-basis)
                               :items (map (comp polarize
                                                 #(assoc % :transaction-item/account account))
                                           (models/select {:transaction-item/account account}))})
                            #(assoc-in % [0 :account/entity] entity)
                            #(update-in % [0 :account/commodity] models/resolve-ref :commodity)
                            #(update-in % [0] models/find :account)))
                 (mapcat (fn [{:keys [account items basis]}]
                           (re-index account basis items))))))))

(def extract-dates
  (comp (mapcat identity)
        (filter identity)
        (filter (util/model-type? :transaction))
        (mapcat (fn [{:transaction/keys [entity transaction-date items]}]
                  (map (fn [{:transaction-item/keys [account]}]
                         [entity account transaction-date])
                       items)))))

(defn accumulate-dates
  [m [entity account date]]
  (-> m
      (assoc :entity-id (:id entity))
      (update-in [:accounts (:id account)] dates/earliest date)
      (update-in [:entity 0] dates/earliest date)
      (update-in [:entity 1] dates/latest date)))

(defmacro with-delayed-propagation
  "Delays propagation for transactions that are put inside the body. Binds two
  positional vars:
    - the output channel to be passed as the option :out-chan to the put-many function
    - the control channel to be passed as the option :ctrl-chan to the put-many function"
  [bindings & body]
  `(let [f# (fn* [~(first bindings) ~(second bindings)] ~@body)
         out# (a/chan 10)
         ctrl# (a/chan)
         pending# (atom 0)
         ready# (atom false)
         _# (a/go-loop [msg# (a/<! ctrl#)]
                       (when msg#
                         (let [count# (swap! pending# (case msg# :start inc :finish dec))]
                           (when (and @ready#
                                      (= 0 count#))
                             (a/close! out#))
                           (recur (a/<! ctrl#)))))
         reduce# (a/transduce
                   extract-dates
                   (completing accumulate-dates)
                   {:entity []
                    :accounts {}}
                   out#)
         prim-result# (f# out# ctrl#)
         _# (reset! ready# true)
         sec-result# (a/<!! reduce#)]
     (concat prim-result#
             (propagate-accounts sec-result#))))
