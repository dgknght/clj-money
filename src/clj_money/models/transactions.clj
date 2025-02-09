(ns clj-money.models.transactions
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.core.async :as a]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [uuid
                                          index-by]]
            [dgknght.app-lib.validation :as v]
            [clj-money.db :as db]
            [clj-money.util :as util :refer [model=]]
            [clj-money.dates :as dates]
            [clj-money.transactions :as trxs]
            [clj-money.models :as models]
            [clj-money.accounts :as acts]))

(defn- simplify
  [m]
  (if (sequential? m)
    (map simplify m)
    (select-keys m [:transaction-item/index
                    :transaction-item/transaction-date
                    :transaction-item/quantity
                    :transaction-item/balance
                    :transaction/description
                    :account/name
                    :entity/name])))

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
                        :lot-item/transaction-date original-transaction-date})))

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
       (map :transaction-item/transaction-date)
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
(s/def :lot-item/lot-action #{:buy :sell})
(s/def :lot-item/shares decimal?)
(s/def ::models/lot-item (s/keys :req [:lot-item/lot
                                       :lot-item/shares
                                       :lot-item/lot-action
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

(defmethod models/before-save :transaction
  [trx]
  (-> trx
      (dissoc :transaction/original-transaction-date)
      (assoc :transaction/value (trxs/value trx))))

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
    (if-let [price (models/find-by #:price{:commodity commodity-id
                                           :trade-date [:between
                                                        earliest-transaction-date
                                                        latest-transaction-date]}
                                   {:sort [[:price/trade-date :desc]]})]
      (* (:price price) balance)
      0M)))

(defn migrate-account
  "Moves all transaction items from from-account to to-account and recalculates the accounts"
  [from-account to-account]
  {:pre [(= (:entity-id from-account)
            (:entity-id to-account))]}
  (throw (Exception. "Not yet implemented"))
  #_(let [entity (models/find (:entity from-account) :entity)
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

(defn items-by-account
  "Returns the transaction items for the specified account"
  [account & {:as options}]
  (models/select (acts/->criteria account options)
                 {:sort [[:transaction-item/transaction-date :desc]
                         [:transaction-item/index :desc]]}))

(defn- last-account-item-before
  [account date]
  (models/find-by
    (db/model-type
      {:transaction-item/account account
       :transaction/transaction-date [:< date]}
      :transaction-item)
    {:sort [[:transaction-item/index :desc]]}))

(defn- last-account-item-on-or-before
  [{:as account :account/keys [earliest-transaction-date]} date]
  (models/find-by (db/model-type
                    {:transaction-item/account account
                     :transaction/transaction-date [:between
                                                    earliest-transaction-date
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

(defn- push-date-boundaries
  [model date early-ks late-ks]
  (-> model
      (update-in early-ks dates/earliest date)
      (update-in late-ks dates/latest date)))

(defn- apply-prev
  "Given a transaction item and the previous transaction item,
  update the index and balance attributes of the item."
  [{:as item ::keys [polarized-quantity]}
   {prev-index :transaction-item/index
    prev-balance :transaction-item/balance}]
  (assoc item
         :transaction-item/index (+ 1 prev-index)
         :transaction-item/balance (+ polarized-quantity prev-balance)))

(defn- propagation-basis
  "Given and account and a date, return the last item before
  the date. If no such item exists, return a dummy item with
  starting index and balance values."
  [account date]
  (or (when (util/live-id? account)
        (last-account-item-before account date))
      #:transaction-item{:index -1
                         :balance 0M}))

(defn- polarize
  [{:transaction-item/keys [account quantity action] :as item}]
  (assoc item
         ::polarized-quantity
         (acts/polarize-quantity quantity
                                 action
                                 account)))

(defn- re-index
  "Given an account and a list of items, take the index and balance
  of the 1st item and calculate indices and balances forward, then
  apply the final to the account. Returns a sequence of all updated
  models, which may be fewer than the input.

  Additionally, an attempt is made to calculate the ending value of the account
  based on the most recent price of the tracked commodity. This price may be
  supplied in the account at :account/commodity-price, or it will be searched
  and will default to 1M if not found."
  [{:as account :account/keys [commodity]} items]
  (let [updated-items (->> items
                           rest
                           (reduce (fn [output item]
                                     (let [updated (apply-prev item (last output))]
                                       (if (= item updated)
                                         (reduced output)
                                         (conj output updated))))
                                   [(first items)]))
        ; the 1st is the basis and is not updated
        to-return (->> updated-items
                       (drop 1)
                       (map #(dissoc % ::polarized-quantity)))
        final-qty (or (:transaction-item/balance (last updated-items))
                       0M)
        price (or (:account/commodity-price account)
                  (:price/price
                    (models/find-by
                      {:price/commodity (:account/commodity account)
                       :price/trade-date [:between
                                          (:commodity/earliest-price commodity)
                                          (:commodity/latest-price commodity)]}
                      {:sort [[:price/trade-date :desc]]}))
                  1M)]
    (if (= (count to-return)
           (- (count items)
              1))  ; this means a short-circuit did not take place
      (cons (assoc account
                   :account/quantity final-qty
                   :account/value (* final-qty price))
            to-return)
      to-return)))

(defn- account-items-on-or-after
  [account as-of]
  (models/select (db/model-type
                   {:transaction-item/account account
                    :transaction/transaction-date [:>= as-of]}
                   :transaction-item)
                 {:sort [[:transaction-item/index :asc]]}))

(defn- integrate-from-bag
  [{:keys [account bag as-of]} items]
  (->> (when bag (get-in @bag [:transactions :new-items]))
       (filter #(model= account
                        (:transaction-item/account %)))
       (remove #(t/before? as-of
                           (:transaction-item/transaction-date %)))
       (concat items)))

(defn- propagate-account-items
  "Returns a function that takes a list of transaction items and returns the
  items along with any other items affected by the transaction, and the updated
  account, if the account is also updated"
  [{:keys [as-of delete?] :as opts}]
  (fn [[_ [{:transaction-item/keys [account]} :as items]]]
    (let [ids (->> items
                   (map :id)
                   set)
          affected-items (if (util/temp-id? account)
                           []
                           (->> (account-items-on-or-after account as-of)
                                (integrate-from-bag (assoc opts :account account))
                                (remove #(ids (:id %)))
                                (sort-by (juxt :transaction-item/transaction-date
                                               :transaction-item/index))
                                (map #(assoc % :transaction-item/account account))))]
      (re-index (if delete?
                  account
                  (-> account
                      (update-in [:account/commodity] #(if (util/model-ref? %)
                                                         (models/find % :commodity)
                                                         %))
                      (push-date-boundaries
                        as-of
                        [:account/earliest-transaction-date]
                        [:account/latest-transaction-date])))
                (cons (propagation-basis account as-of)
                      (->> (cond->> affected-items
                             (not delete?) (concat items))
                           (sort-by :transaction-item/transaction-date t/before?)
                           (map polarize)))))))

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
  [items]
  (if-let [account-ids (account-model-ref-ids items)]
    (let [accounts (index-by :id
                             (models/select
                               (db/model-type
                                 {:id [:in account-ids]}
                                 :account)))]
      (map #(update-in % [:transaction-item/account] (fn [act]
                                                       (or (accounts (:id act))
                                                           act)))
           items))
    items))

(defn- propagate-current-items
  "Given a transaction, return a list of accounts and transaction items
  that will also be affected by the operation."
  [{:transaction/keys [items transaction-date] :keys [id] :as trx} opts]
  (->> items
       (map #(cond-> %
               true (assoc :transaction-item/transaction-date transaction-date
                           ::current true)
               id   (assoc :transaction-item/transaction {:id id})))
       realize-accounts
       (group-by (comp util/->model-ref
                       :transaction-item/account))
       (mapcat (propagate-account-items
                 (assoc opts
                        :as-of (dates/earliest
                                 transaction-date
                                 (models/before trx :transaction/transaction-date)))))))

(defn- propagate-dereferenced-account-items
  [{:transaction/keys [items] :as trx} opts]
  (let [act-ids (->> items
                     (map (comp :id
                                :transaction-item/account))
                     set)]
    (->> (models/before trx :transaction/items)
         (remove (comp act-ids
                       :id
                       :transaction-item/account))
         realize-accounts
         (group-by (comp util/->model-ref
                         :transaction-item/account))
         (mapcat (propagate-account-items
                   (assoc opts
                          :as-of (models/before trx :transaction/transaction-date)
                          :delete? true))))))

(defn- propagate-items
  "Given a transaction, return a list of accounts and transaction items
  that will also be affected by the operation."
  [trx opts]
  (concat (propagate-current-items trx opts)
          (propagate-dereferenced-account-items trx opts)))

(def delayed (atom {}))

(defn- propagate-scheduled-transaction
  [{:transaction/keys [transaction-date]
    {:scheduled-transaction/keys [last-occurrence]
     :as sched-trx} :transaction/scheduled-transaction}]
  (when (and last-occurrence
             (t/before? last-occurrence transaction-date))
    (assoc sched-trx :scheduled-transaction/last-occurrence transaction-date)))

(defn- bag-new-items
  [bag items]
  (swap! bag
         update-in
         [:transactions :new-items]
         (fnil concat [])
         (remove :id items)))

(defn- propagate-transaction
  [{:as trx :transaction/keys [transaction-date]} {:as opts :keys [bag]}]
  (let [{current-items true
         others false} (group-by ::current
                                 (propagate-items trx opts))
        entity (-> (:transaction/entity trx)
                   (models/find :entity)
                   (push-date-boundaries transaction-date
                                         [:entity/settings
                                          :settings/earliest-transaction-date]
                                         [:entity/settings
                                          :settings/latest-transaction-date]))
        updated-sched (propagate-scheduled-transaction trx)
        transaction-items (map #(dissoc % ::current) current-items)]
    (bag-new-items bag transaction-items)
    (cons (cond-> trx
            (seq transaction-items)
            (assoc :transaction/items transaction-items))
          (concat (filter identity
                          [entity
                           updated-sched])
                  others))))

(defn- append-delay-details
  [m {:transaction/keys [items transaction-date]}]
  (-> m
      (update-in [:accounts] #(apply (fnil conj #{})
                                     %
                                     (map (comp util/->model-ref
                                                :transaction-item/account)
                                          items)))
      (update-in [:earliest-date] dates/earliest transaction-date)
      (update-in [:latest-date] dates/latest transaction-date)))

(defn- delay-propagation
  "Given a transaction, appends dummy index and balance values
  and saves account and date information for later use"
  [trx _opts]
  (swap! delayed
         update-in
         [(-> trx :transaction/entity :id)]
         append-delay-details
         trx)
  [(update-in trx
              [:transaction/items]
              #(map (fn [i]
                      (assoc i
                             :transaction-item/index 0
                             :transaction-item/balance 0M))
                    %))])

(defmethod models/propagate :transaction
  [{:as trx :transaction/keys [entity]} opts]
  (if (@delayed (:id entity))
    (delay-propagation trx opts)
    (propagate-transaction trx opts)))

(defmethod models/propagate-delete :transaction
  [{:as trx :transaction/keys [entity]} opts]
  (if (@delayed (:id entity))
    (delay-propagation trx opts)
    (cons trx (propagate-current-items trx (assoc opts :delete? true)))))

(defmethod models/before-delete :transaction
  [trx]
  (when (and (:id trx)
             (< 0  (models/count {:transaction-item/transaction trx
                                  :transaction-item/transaction-date (:transaction/transaction-date trx)
                                  :transaction-item/reconciliation [:!= nil]})))
    (throw (IllegalStateException. "Cannot delete transaction with reconciled items")))
  trx)

(defn process-delayed-balances*
  [entity-id {:keys [accounts earliest-date latest-date]} progress-chan]
  (-> (models/find entity-id :entity)
      (update-in [:entity/settings
                  :settings/earliest-transaction-date]
                 dates/earliest earliest-date)
      (update-in [:entity/settings
                  :settings/latest-transaction-date]
                 dates/latest latest-date)
      models/put)
  (a/>!! progress-chan
         (->> accounts
              (map (comp (fn [a]
                           (let [basis (propagation-basis a earliest-date)
                                 affected (map (comp polarize
                                                     #(assoc % :transaction-item/account a))
                                               (account-items-on-or-after a earliest-date))]
                             (models/put-many (re-index a (cons basis affected)))))
                         #(models/find % :account)))
              (reduce (fn [prg _put-result]
                        (a/>!! progress-chan prg)
                        (update-in prg [:completed] inc))
                      {:total (count accounts)
                       :completed 0})))
  (a/close! progress-chan))

(defmacro with-delayed-balancing
  "Any transactions saved in this body of this macro will not propagate
  changes to other models until the form is completed.

  The first binding argument is the ID of the entity for which balancing is delayed.
  The second binding argument is a channel to which updates will be sent."
  [bindings & body]
  `(let [f# (fn* [] ~@body)
         entity-id# ~(first bindings)
         prog-chan# ~(second bindings)
         _# (swap! delayed assoc entity-id# {})
         result# (f#)]
     (process-delayed-balances* entity-id# (@delayed entity-id#) prog-chan#)
     (swap! delayed dissoc entity-id#)
     result#))

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
