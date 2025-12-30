(ns clj-money.entities.transactions
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.core.async :as a]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clojure.walk :refer [postwalk]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [index-by
                                          update-in-if
                                          presence]]
            [dgknght.app-lib.validation :as v]
            [clj-money.db :as db]
            [clj-money.util :as util :refer [id=]]
            [clj-money.dates :as dates]
            [clj-money.transactions :as trxs]
            [clj-money.entities :as entities]
            [clj-money.entities.propagation :as prop]
            [clj-money.accounts :as acts]))

(defn- unbox
  "Takes a function and returns a function that takes a single argument
  and if that argument is a vector, extracts the second element and passes
  it to the given function, otherwise passes the argument as-is.

  This is because the transaction specs use 'or' specs, which can
  result in maps being wrapped in tuples with the branch of the matching
  'or' in the first position."
  [f]
  (fn [x]
    (f (if (vector? x)
         (second x)
         x))))

(defn- new-transaction-has-items?
  [input]
  (if (vector? input)
    (if (= :simple (first input))
      true
      (new-transaction-has-items? (second input)))
    (or (:id input)
        (seq (:transaction/items input)))))

(v/reg-spec new-transaction-has-items?
            {:message "A new transaction must have items"
             :path [:transaction/items]})

(defn- qty-comparable
  [item]
  (select-keys item [:id :account-item/quantity]))

(defn- no-reconciled-quantities-changed*
  [{:transaction/keys [items] :as trx}]
  (if (:id trx)
    (let [after (->> items
                     (map (juxt :id qty-comparable))
                     (into {}))]
      (->> (entities/select (util/entity-type
                              {:transaction/_self trx
                               :account-item/reconciliation [:!= nil]}
                              :account-item))
           (map qty-comparable)
           (remove #(= % (after (:id %))))
           empty?))
    true))

(def ^:private no-reconciled-quantities-changed?
  (unbox no-reconciled-quantities-changed*))

(v/reg-spec no-reconciled-quantities-changed? {:message "A reconciled quantity cannot be updated"
                                               :path [:transaction/items]})

(defn- not-a-trading-transaction?
  [{:keys [id] :transaction/keys [original-transaction-date]}]
  (zero? (entities/count {:lot-item/transaction  {:id id}
                        :transaction/transaction-date original-transaction-date})))

(v/reg-spec not-a-trading-transaction? {:message "A trading transaction cannot be updated."
                                        :path []})

(v/reg-msg trxs/sum-of-credits-equals-sum-of-debits? "Sum of debits must equal the sum of credits")

(def actions
  "Set of valid transaction action values, includes :debit and :credit"
  #{:debit :credit})

(s/def :lot-item/lot ::entities/entity-ref)
(s/def :lot-item/action #{:buy :sell})
(s/def :lot-item/shares decimal?)
(s/def ::entities/lot-item (s/keys :req [:lot-item/lot
                                         :lot-item/shares
                                         :lot-item/action
                                         :lot-item/price]))
(s/def :transaction/lot-items (s/coll-of ::entities/lot-item))

(s/def ::entities/transaction (s/and (s/merge ::trxs/transaction
                                              (s/keys :opt [:transaction/lot-items]))
                                     no-reconciled-quantities-changed?
                                     new-transaction-has-items?))

(defn- specified-entry?
  [ks]
  (every-pred map-entry?
              (comp ks key)))

(defn- empty-strings->nils
  [entity & keys]
  (let [specified? (specified-entry? (set keys))]
    (postwalk (fn [x]
                (if (specified? x)
                  (update-in x [1] presence)
                  x))
              entity)))

(defmethod entities/before-validation :transaction
  [trx]
  (empty-strings->nils trx))

(defmethod entities/before-save :transaction
  [transaction]
  (let [{:transaction/keys [items] :as trx}
        (-> transaction
            (dissoc :transaction/original-transaction-date)
            trxs/->bilateral)]
    (cond-> trx
      (seq items)
      (assoc :transaction/value (->> items
                                     (map :transaction-item/value)
                                     (reduce + 0M))))))

(defn items-by-account
  "Returns the transaction items for the specified account"
  [account & {:as options}]
  (entities/select (acts/->criteria account options)
                   {:sort [[:transaction/transaction-date :desc]
                           [:account-item/index :desc]]
                    :select-also [:transaction/transaction-date]}))

(defn- last-account-item-before
  [account date]
  (entities/find-by
    (util/entity-type
      {:account-item/account account
       :transaction/transaction-date [:< date]}
      :account-item)
    {:sort [[:account-item/index :desc]]}))

(defn- last-account-item-on-or-before
  [{:as account :account/keys [transaction-date-range]} date]
  {:pre [(:account/transaction-date-range account)]}
  (entities/find-by (util/entity-type
                      {:account-item/account account
                       :transaction/transaction-date [:between
                                                      (first transaction-date-range)
                                                      date]}
                      :account-item)
                    {:sort [[:transaction/transaction-date :desc]
                            [:account-item/index :desc]]}))

(defn balance-delta
  "Returns the change in balance during the specified period for the specified account"
  [account start end]
  (let [t1 (last-account-item-before account start)
        t2 (last-account-item-on-or-before account end)]
    (- (:account-item/balance t2 0M)
       (:account-item/balance t1 0M))))

(defn balance-as-of
  "Returns the balance for the specified account as of the specified date"
  ([account year month day]
   (balance-as-of account (t/local-date year month day)))
  ([account as-of]
   (or (:account-item/balance
         (last-account-item-on-or-before account as-of))
       0M)))

(defn- apply-prev
  "Given a transaction item and the previous transaction item,
  update the index and balance attributes of the item."
  [{:as item :account-item/keys [quantity]}
   {prev-index :account-item/index
    prev-balance :account-item/balance
    :as prev-item}]
  {:pre [(:account-item/balance prev-item)
         (:account-item/index prev-item)]}
  (assoc item
         :account-item/index (inc prev-index)
         :account-item/balance (+ quantity prev-balance)))

(def ^:private initial-basis
  #:account-item{:index -1
                 :balance 0M})

(defn- propagation-basis
  "Given and account and a date, return the last item before
  the date. If no such item exists, return a dummy item with
  starting index and balance values."
  [account date]
  (or (when (util/live-id? account)
        (last-account-item-before account date))
      initial-basis))

(defn- default-commodity?
  [{:account/keys [commodity] {{:settings/keys [default-commodity]} :entity/settings} :account/entity}]
  (id= commodity default-commodity))

(defn- fetch-latest-price
  [{:commodity/keys [price-date-range] :as commodity}]
  (when price-date-range
    (:price/value
      (entities/find-by
        {:price/commodity commodity
         :price/trade-date (apply vector :between price-date-range)}
        {:sort [[:price/trade-date :desc]]}))))

(defn- re-index
  "Given an account and a list of account items, take the index and balance
  of the 1st item and calculate indices and balances forward, then
  apply the final to the account. Returns a sequence of all updated
  entities, which may be fewer than the input.

  Additionally, an attempt is made to calculate the ending value of the account
  based on the most recent price of the tracked commodity. This price may be
  supplied in the account at :account/commodity-price, or it will be searched
  and will default to 1M if not found."
  ([account basis items]
   (re-index account basis {} items))
  ([{:as account :account/keys [commodity]}
    basis
    {:keys [force?]}
    items]
   {:pre [account (every? :transaction/transaction-date items)]}
   (if (empty? items)
     [(cond-> (assoc account
                     :account/quantity (:transaction-item/balance basis 0M)
                     :account/value (:transaction-item/value basis 0M))
        (= -1 (:account-item/index basis))
        (assoc :account/transaction-date-range nil))]
     (let [updated-items (->> items
                              (reduce (fn [output item]
                                        (let [updated (apply-prev item (last output))]
                                          (if (and (= item updated)
                                                   (not force?))
                                            (reduced output)
                                            (conj output updated))))
                                      [basis])
                              (drop 1))
           final-qty (or (:account-item/balance (last updated-items))
                         (:account-item/balance basis))
           price (or (when (default-commodity? account) 1M)
                     (:account/commodity-price account)
                     (fetch-latest-price commodity)
                     (throw (ex-info "No price found for commodity" {:commodity commodity})))]
       (if (= (count updated-items)
              (count items))
         (cons (-> account
                   (dates/push-entity-boundary :account/transaction-date-range
                                               (:transaction/transaction-date (last items))
                                               (some :transaction/transaction-date
                                                     (cons basis
                                                           items)))
                   (assoc :account/quantity final-qty
                          :account/value (* final-qty price)))
               updated-items)
         updated-items)))))

(defn- account-items-on-or-after
  [account as-of]
  (entities/select (util/entity-type
                     {:account-item/account account
                      :transaction/transaction-date [:>= as-of]}
                     :account-item)
                   {:sort [[:account-item/index :asc]]
                    :select-also [:transaction/transaction-date]}))

(defn- propagate-account-items
  "Returns a function that takes a list of transaction items and returns the
  items along with any other items affected by the transaction, and the updated
  account, if the account is also updated"
  [& {:keys [as-of delete?]}]
  (fn [[_ [{:account-item/keys [account]} :as items]]]
    {:pre [account]}
    (let [ids (->> items
                   (map :id)
                   set)
          affected-items (if (util/temp-id? account)
                           []
                           (->> (account-items-on-or-after account as-of)
                                (remove (comp ids :id))
                                (map #(assoc % :account-item/account account))))]
      (re-index (if delete?
                  account
                  (dates/push-entity-boundary
                    account
                    :account/transaction-date-range
                    as-of))
                (propagation-basis account as-of)
                {:force? delete?}
                (sort-by :transaction/transaction-date t/before?
                         (cond->> affected-items
                           (not delete?) (concat items)))))))

(defn- realize-commodities
  "Given a list of accounts, looks up the commodities"
  [accounts]
  (when (seq accounts)
    (let [commodities (->> accounts
                           (map (comp :id :account/commodity))
                           set
                           seq
                           entities/find-many
                           (index-by :id))]
      (map #(update-in %
                       [:account/commodity]
                       (comp commodities :id))
           accounts))))

(defn- account-ref-ids
  "Extracts and returns the account ids from references from the account
  items, omitting any complete account entities."
  [items]
  (->> items
       (map :account-item/account)
       (filter util/entity-ref?)
       (map :id)
       set
       seq))

; TODO: Need to think some more about how to handle differences
; between the account in the item (when not a simple entity-ref) and
; the account read from the database.
; It's possible the caller has made unsaved changes they want to keep.
; It's also possble the account has changed since the specified account
; was read from the database.
(defn- realize-accounts
  "Given a list of account items, replace any account references with full
  account maps."
  [entity items]
  (if-let [account-ids (account-ref-ids items)]
    (let [accounts (->> (entities/find-many account-ids)
                        (map #(assoc % :account/entity entity))
                        realize-commodities
                        (index-by :id))]
      (map #(update-in %
                       [:account-item/account]
                       (comp accounts :id))
           items))
    items))

(def ^:private transaction-item?
  (util/entity-type? :transaction-item))

(defn- belongs-to-trx?
  [{:transaction/keys [items]}]
  (fn [entity]
    (and (transaction-item? entity)
         (contains? (set (map :id items)) (:id entity)))))

(defn- propagate-current-items
  "Given a transaction, return a list of accounts and transaction items
  that will also be affected by the operation."
  [[before {:transaction/keys [transaction-date] :as after}]]
  (let [entity (entities/find (:transaction/entity after))]
    (->> (:transaction/items after)
         (mapcat (juxt :transaction-item/debit-item
                       :transaction-item/credit-item))
         (realize-accounts entity)
         (map #(assoc % :transaction/transaction-date transaction-date))
         (group-by (comp :id
                         :account-item/account))
         (mapcat (propagate-account-items
                   :as-of (dates/earliest
                            transaction-date
                            (:transaction/transaction-date before))
                   :delete? false)))))

(defn- propagate-dereferenced-account-items
  [[before {:transaction/keys [items] :as after}]]
  (let [after-ids (->> items
                       (map :id)
                       set)
        entity (-> (or after before)
                   :transaction/entity
                   entities/find)]
    (->> (:transaction/items before)
         (remove (comp after-ids :id))
         (mapcat (juxt :transaction-item/debit-item
                       :transaction-item/credit-item))
         (realize-accounts entity)
         (group-by (comp :id
                         :account-item/account))
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
                   entities/find
                   (dates/push-entity-boundary
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

(defmethod entities/before-delete :transaction
  [{:keys [id] :as trx}]
  (when-let [{:transaction/keys [items]}
             (when id (entities/find-by (util/entity-type {:id id}
                                                       :transaction)
                                      {:include-items? true}))]
    (when (some :transaction-item/reconciliation
                items)
      (throw (IllegalStateException. "Cannot delete transaction with reconciled items"))))
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
                              (entities/select #:transaction-item{:transaction [:in (map :id trxs)]}))]
          (map #(assoc % :transaction/items (items (:id %)))))))))

(defn- fetch-account-items
  [account]
  (map #(assoc % :account-item/account account)
       (entities/select {:account-item/account account}
                        {:sort [:transaction/transaction-date
                                :account-item/index]
                         :select-also [:transaction/transaction-date]})))

(defn- process-account-items
  [account entity items]
  (if items
    (->> items
         (re-index (assoc account :account/entity entity)
                   initial-basis
                   {:force? true})
         (map (comp #(dissoc % :transaction/transaction-date)
                    #(update-in-if %
                                   [:transaction-item/account]
                                   util/->entity-ref))))
    [(assoc account
            :account/transaction-date-range nil
            :account/quantity 0M
            :account/value 0M)]))

(defn propagate-account-from-start
  [entity account]
  (try
    (let [items (fetch-account-items account)
          [{:account/keys [transaction-date-range]}
           :as updates] (process-account-items account entity items)
          updated (-> entity
                      (update-in [:entity/transaction-date-range]
                                 #(apply dates/push-boundary
                                         %
                                         transaction-date-range))
                      entities/put)]
      (->> updates
           (partition-all 10)
           (mapcat entities/put-many)
           doall)
      updated)
    (catch Exception e
      (log/errorf e
                  "[propagation] Unable to propagate account %s (%s) "
                  (:account/name account)
                  (:id account))
      entity)))

(defn propagate-all
  [entity {:keys [progress-chan]}]
  {:pre [entity (map? entity)]}
  (let [accounts (entities/select {:account/entity entity})
        total (count accounts)
        notify-progress (if progress-chan
                          (fn [entity]
                            (a/go
                              (a/>! progress-chan
                                    {:import/record-type :propagation}))
                            entity)
                          identity)]

    (log/debugf "[propagation] process transactions for %s. %s account(s)"
                (:entity/name entity)
                total)

    (when progress-chan
      (a/go (a/>! progress-chan {:declaration/record-type :propagation
                                 :declaration/record-count total
                                 :import/record-type :declaration})))
    (->> accounts
         realize-commodities
         (interleave (map inc (range)))
         (partition-all 2)
         (map (fn [[index account]]
                (log/debugf "[propagation] starting account %s (%d of %d)"
                            (:account/name account)
                            index
                            total)
                account))
         (reduce (comp notify-progress
                       propagate-account-from-start)
                 entity))))

(prop/add-full-propagation propagate-all :priority 5)

(defn migrate-account
  "Moves all transaction items from from-account to to-account and recalculates the accounts"
  [from-account to-account]
  {:pre [(id= (:account/entity from-account)
              (:account/entity to-account))]}
  (let [entity (-> from-account :account/entity entities/find)
        as-of (or (get-in from-account [:account/transaction-date-range 0])
                  (get-in entity [:entity/transaction-date-range 0]))]
    (assert as-of "Unable to find the earliest transaction date.")
    (entities/update {:transaction-item/account (util/->entity-ref to-account)
                    :transaction-item/index 0
                    :transaction-item/balance nil}
                   {:transaction-item/account (util/->entity-ref from-account)
                    :transaction/transaction-date [:>= as-of]})
    (doseq [account [from-account to-account]]
      (propagate-account-from-start entity account))))

(defn propagate-accounts
  "Takes a map of account ids to dates and recalculates indices and balances for those
  accounts as of the associated dates."
  [{:keys [accounts entity-id] :as x}]
  (let [entity (entities/find entity-id)]
    (entities/put-many
      (cons (update-in entity
                       [:entity/transaction-date-range]
                       #(apply dates/push-boundary % (:entity x)))
            (->> accounts
                 (map (comp (fn [[account date]]
                              {:account account
                               :date date
                               :basis (or (last-account-item-before account date)
                                          initial-basis)
                               :items (map #(assoc % :account-item/account account)
                                           (entities/select {:account-item/account account}
                                                            {:select-also [:transaction/transaction-date]}))})
                            #(assoc-in % [0 :account/entity] entity)
                            #(update-in % [0 :account/commodity] entities/resolve-ref)
                            #(update-in % [0] entities/find)))
                 (mapcat (fn [{:keys [account items basis]}]
                           (re-index account basis items))))))))

(def extract-dates
  (comp (mapcat identity)
        (filter identity)
        (filter (util/entity-type? :transaction))
        (mapcat (fn [{:transaction/keys [entity transaction-date items]}]
                  (map (fn [{:account-item/keys [account]}]
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
