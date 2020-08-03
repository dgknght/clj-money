(ns clj-money.import
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.java.io :as io]
            [clojure.core.async :refer [<!! >!! chan go] :as async]
            [stowaway.core :refer [with-storage
                                   with-transacted-storage]]
            [clj-money.validation :as validation]
            [clj-money.trading :as trading]
            [clj-money.accounts :refer [->criteria]]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.reconciliations :as recs]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.images :as images]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]))

(defn- ignore?
  [record]
  (-> record meta :ignore?))

(defmulti read-source
  (fn [source-type & _]
    source-type))

(defn- import-price
  [{:keys [storage entity] :as context} price]
  (let [commodity (->> {:exchange (name (:exchange price))
                        :symbol (:symbol price)
                        :entity-id (:id entity)}
                       (commodities/search storage)
                       first)]
    (prices/create storage (-> price
                               (assoc :commodity-id (:id commodity))
                               (dissoc :exchange :symbol))))
  context)

(defn- find-commodity
  [{:keys [commodities-by-symbol
           commodities-by-exchange-and-symbol]}
   {:keys [exchange symbol]}]
  (or (get commodities-by-exchange-and-symbol [exchange symbol])
      (get commodities-by-symbol symbol)))

(defn- import-account
  [context account]
  (let [original-id (:id account)
        original-parent-id (:parent-id account)
        parent-id (get-in context [:accounts original-parent-id])
        commodity (find-commodity context (:commodity account))
        to-create (-> account
                      (assoc :entity-id (-> context :entity :id)
                             :commodity-id (:id commodity))
                      (dissoc :id :commodity)
                      (assoc :parent-id parent-id))
        result (accounts/create (:storage context) to-create)]
    (when-not (:id result)
      (throw (ex-info (str
                        "Unable to create the account "
                        (validation/error-messages result))
                      {:result result})))
    (log/info (format "imported account \"%s\"" (:name result)))
    (cond-> (update-in context [:accounts] assoc original-id (:id result))
      parent-id (update-in [:account-children]
                           (fnil update-in {})
                           [parent-id]
                           (fnil conj [])
                           (:id result))
      parent-id (update-in [:account-parents]
                           (fnil assoc {})
                           (:id result)
                           parent-id))))

(defn import-reconciliation
  [{:keys [accounts storage] :as context} reconciliation]
  (let [created (as-> reconciliation r
                  (update-in r [:account-id] accounts)
                  (dissoc r :id)
                  (assoc r :balance 0M)
                  (recs/create storage r))]
    (update-in context [:account-recons]
               (fnil assoc {})
               (:account-id created) (:id created))))

(defn- prepare-budget-item
  [item {:keys [accounts]}]
  (if-let [account-id (accounts (:account-id item))]
    (assoc item :account-id account-id)
    (do
      (log/errorf "Unable to resolve account id %s" (:account-id item))
      item)))

(defn- prepare-budget
  [budget {:keys [entity] :as context}]
  (-> budget
      (update-in [:items]
                 (fn [items]
                   (->> items
                        (map #(prepare-budget-item % context))
                        (filter :account-id)
                        (remove #(= 0M (reduce + (:periods %)))))))
      (assoc :entity-id (:id entity))))

(defn- import-budget
  [{:keys [storage ] :as context} budget]
  (log/debugf "importing budget %s" (prn-str budget))
  (let [to-create (prepare-budget budget context)]
    (try
      (log/infof "imported budget %s"
                 (:name (budgets/create storage
                                        to-create)))
      (catch Exception e
        (log/errorf "error importing budget %s - %s: %s"
                    (.getClass e)
                    (.getMessage e)
                    (prn-str to-create)))))
  context)

(defn- import-commodity
  [{:keys [entity storage] :as context} commodity]
  (let [to-create (assoc commodity :entity-id (:id entity))
        {:keys [exchange symbol] :as created} (commodities/create storage to-create)]
    (if (validation/has-error? created)
      (throw (ex-info (format "Unable to create commodity %s (%s): %s"
                              (:name created)
                              (:symbol created)
                              (validation/error-messages created))
                      created))
    (log/infof "imported commodity %s (%s)" (:name created)  (:symbol created)))
    (-> context
        (update-in [:commodities] #((fnil conj []) % created))
        (update-in [:commodities-by-symbol] #((fnil assoc {}) % symbol created))
        (update-in [:commodities-by-exchange-and-symbol] #((fnil assoc {}) % [exchange symbol] created)))))

(defn- resolve-account-references
  [context items]
  (map (fn [item]
         (update-in item [:account-id] #(-> context :accounts (get %))))
       items))

(defn- prepare-transaction
  [transaction context]
  (-> transaction
      (update-in [:items] #(resolve-account-references context %))
      (assoc :entity-id (-> context :entity :id))))

(defmulti ^:private import-transaction
  (fn [_ transaction]
    (:action transaction)))

(defn- log-transaction
  [transaction transaction-type]
  (if (validation/has-error? transaction)
    (log/errorf "error importing %s transaction %s"
                transaction-type
                transaction)
    (log/infof "imported %s transaction on %s: %s"
               transaction-type
               (:transaction-date transaction)
               (:description transaction))))

(defmethod ^:private import-transaction :default
  [context transaction]
  (let [to-create (prepare-transaction transaction context)
        created (transactions/create (:storage context)
                                    to-create)]
    (log-transaction created "standard"))
  context)

(defn- inv-transaction-fee-info
  [{:keys [accounts storage]} transaction trans-type]
  (when-not (= 2 (count (:items transaction)))
    (let [non-commodity-items (filter #(= (if (= :buy trans-type)
                                            :credit
                                            :debit)
                                          (:action %))
                                      (:items transaction))
          account-ids (map #(get-in accounts [(:account-id %)])
                           non-commodity-items)
          accounts-map (->> (accounts/search storage {:id account-ids})
                        (map (juxt :id identity))
                        (into {}))
          fee-items (filter #(= :expense (get-in accounts-map [(accounts (:account-id %)) :type]))
                            non-commodity-items)]
      (when (seq fee-items)
        [(transduce (map :quantity) + fee-items)
         (accounts (:account-id (first fee-items)))]))))

(defmethod ^:private import-transaction :buy
  [{:keys [accounts storage] :as context} transaction]
  (log/debug "import buy transaction " (prn-str transaction))
  (let [[fee fee-account-id] (inv-transaction-fee-info context transaction :sell)
        purchase {:commodity-id (->> context
                                     :commodities
                                     (filter #(and (= (:symbol %) (:symbol transaction))
                                                   (= (:exchange %) (:exchange transaction))))
                                     first
                                     :id)
                  :commodity-account-id (accounts (:commodity-account-id transaction))
                  :account-id (accounts (:account-id transaction))
                  :fee fee
                  :fee-account-id fee-account-id
                  :trade-date (:transaction-date transaction)
                  :shares (:shares transaction)
                  :value (:value (->> (:items transaction)
                                      (filter #(= :debit (:action %)))
                                      first))}
        {result :transaction} (trading/buy storage purchase)]
    (log-transaction result "commodity purchase"))
  context)

(defmethod ^:private import-transaction :sell
  [{:keys [accounts storage] :as context} transaction]
  (log/debug "import sell transaction " (prn-str transaction))
  (let [[fee fee-account-id] (inv-transaction-fee-info context transaction :sell)
        sale {:commodity-id (->> context
                                 :commodities
                                 (filter #(and (= (:symbol %) (:symbol transaction))
                                               (= (:exchange %) (:exchange transaction))))
                                 first
                                 :id)
              :fee fee
              :fee-account-id fee-account-id
              :commodity-account-id (accounts (:commodity-account-id transaction))
              :account-id (accounts (:account-id transaction))
              :trade-date (:transaction-date transaction)
              :shares (:shares transaction)
              :value (:value (->> (:items transaction)
                                  (filter #(= :credit (:action %)))
                                  first))}
        {result :transaction} (trading/sell storage sale)]
    (log-transaction result "commodity sale"))
  context)

(defmethod ^:private import-transaction :transfer
  [{:keys [storage accounts] :as context}
   {:keys [from-account-id to-account-id] :as transaction}]
  (log/debug "import transfer transaction " (prn-str transaction))
  (let [from-commodity-account (accounts/find-by-id storage (accounts from-account-id))
        from-account (accounts/find-by-id storage (:parent-id from-commodity-account))
        to-commodity-account (accounts/find-by-id storage (accounts to-account-id))
        {result :transaction} (trading/transfer (:storage context)
                                                (assoc transaction
                                                       :transfer-date (:transaction-date transaction)
                                                       :commodity-id (:commodity-id from-commodity-account)
                                                       :from-account-id (:id from-account)
                                                       :to-account-id (:parent-id to-commodity-account)))]
    (log-transaction result "commodity transfer"))
  context)

; This is maybe too specific to GnuCash. It would be better if the
; gnucash namespace did these lookups
(defn- ensure-split-ids
  [{:keys [commodity-account-id commodity-id account-id] :as transaction}
   {:keys [accounts storage]}]
  (if (and account-id commodity-id)
    transaction
    (let  [commodity-account (accounts/find-by-id
                               storage
                               (accounts commodity-account-id))]
      (assoc transaction :commodity-id (:commodity-id commodity-account)
                         :account-id (:parent-id commodity-account)))))

(defmethod ^:private import-transaction :split
  [context transaction]
  (let  [split (-> transaction
                   (ensure-split-ids context)
                   (dissoc :items))
         {result :transaction} (trading/split (:storage context) split)]
    (log-transaction result "commodity split"))
  context)

(defn- get-source-type
  [{content-type :content-type}]
  (->> content-type
       (re-matches #"^application\/(.*)")
       second
       keyword))

(defn- prepare-input
  "Returns the input data and source type based
  on the specified image"
  [storage image-ids]
  (let [images (map #(images/find-by storage {:id %} {:include-body? true})
                    image-ids)
        source-type (-> images first get-source-type)]
    [(map #(io/input-stream (byte-array (:body %)))
          images)
     source-type]))

(def ^:private reportable-record-types
  #{:commodity :price :account :transaction :budget})

(defn- report-progress?
  [record]
  (-> record
      meta
      :record-type
      reportable-record-types))

(defn- inc-progress
  [xf]
  (fn
    ([context] (xf context))
    ([context record]
     (if (report-progress? record)
       (xf
         (update-in context [:progress
                             (-> record meta :record-type)
                             :imported]
                    (fnil inc 0))
         record)
       (xf context record)))))

(defmulti import-record*
  (fn [_ record]
    (-> record meta :record-type)))

(defmethod import-record* :declaration
  [context {:keys [record-type record-count]}]
  (assoc-in context
            [:progress record-type :total]
            record-count))

(defmethod import-record* :account
  [context account]
  (import-account context account))

(defmethod import-record* :reconciliation
  [context reconciliation]
  (import-reconciliation context reconciliation))

(defn- find-reconciliation-id
  [old-account-id {:keys [accounts account-recons account-parents]}]
  (loop [id (accounts old-account-id)]
    (when id
      (if-let [recon-id (account-recons id)]
        recon-id
        (recur (account-parents id))))))

(defn- refine-recon-info
  [{:keys [account-recons] :as ctx} items]
  (if account-recons
    (map #(-> %
              (assoc :reconciliation-id
                     (if (:reconciled %)
                       (find-reconciliation-id (:account-id %) ctx)
                       nil))
              (dissoc :reconciled))
         items)
    items))

(defmethod import-record* :transaction
  [context transaction]
  (import-transaction context
                      (update-in transaction
                                 [:items]
                                 #(refine-recon-info context %))))

(defmethod import-record* :budget
  [context budget]
  (import-budget context budget))

(defmethod import-record* :price
  [context price]
  (import-price context price))

(defmethod import-record* :commodity
  [context commodity]
  (import-commodity context commodity))

(defn- import-record
  [xf]
  (fn
    ([] (xf))
    ([context] (xf context))
    ([context record]
     (if (ignore? record)
       (xf context record)
       (xf (try (import-record* context record)
                (catch clojure.lang.ExceptionInfo e
                  (assoc-in context [:progress :error] {:message (.getMessage e)
                                                        :data (ex-data e)})))
           record)))))

(defn- notify-progress
  [progress-chan]
  (let [last-progress (atom nil)]
    (fn
      ([context]
       context)
      ([{:keys [progress] :as context} _]
       (if (= progress @last-progress)
         context
         (do
           (>!! progress-chan progress)
           (reset! last-progress progress)
           (if (:error progress)
             (reduced context)
             context)))))))

(defn- append-child-ids
  [account-id account-children]
  (concat [account-id]
          (mapcat #(append-child-ids % account-children)
                  (account-children account-id))))

(defn- process-reconciliation
  [{:keys [account-id id] :as recon} {:keys [storage account-children]}]
  (let [accounts (accounts/search storage
                                  {:id (if account-children
                                         (append-child-ids account-id account-children)
                                         account-id)})]
    (if (every? :earliest-transaction-date accounts)
      (let [criteria (assoc (->criteria accounts)
                            :reconciliation-id id)
            balance (->> (transactions/search-items storage criteria)
                         (map :polarized-quantity)
                         (reduce + 0M))
            result (recs/update storage (assoc recon
                                               :balance balance
                                               :status :completed))]
        (when (validation/has-error? result)
          (log/errorf "Unable to finalize reconciliation: %s" (prn-str (select-keys result [:id :account-id :balance :end-of-period ::validation/errors])))))
      (log/errorf "Unable to finalized reconciliation because date range is unspecified: %s %s"
                  (prn-str (select-keys recon [:id :account-id :balance :end-of-period]))
                  (prn-str (map #(select-keys % [:id
                                                 :name
                                                 :parent-id
                                                 :earliest-transaction-date
                                                 :latest-trasaction-date])
                                accounts))))))

(defn- process-reconciliations
  [{:keys [entity storage] :as ctx}]
  (doseq [recon (recs/search storage {[:account :entity-id] (:id entity)})]
    (process-reconciliation recon ctx)))

(defn- import-data*
  [s import-spec progress-chan]
  (let [user (users/find-by-id s (:user-id import-spec))
        [inputs source-type] (prepare-input s (:image-ids import-spec))
        entity (entities/find-or-create s
                                        user
                                        (:entity-name import-spec))
        wait-promise (promise)
        out-chan (chan)
        result-chan (async/transduce (comp import-record
                                           inc-progress)
                                     (notify-progress progress-chan)
                                     {:storage s
                                      :import import-spec
                                      :progress {}
                                      :accounts {}
                                      :entity entity}
                                     out-chan)]
    (go
      (try
        (let [result (transactions/with-delayed-balancing s (:id entity)
                       (read-source source-type inputs out-chan)
                       (<!! result-chan))]
          (process-reconciliations result)
          (>!! progress-chan (assoc (:progress result) :finished true)))
        (finally
          (deliver wait-promise true))))
    {:entity entity
     :wait wait-promise}))

(defn import-data
  "Reads the contents from the specified input and saves
  the information using the specified storage. If an entity
  with the specified name is found, it is used, otherwise it
  is created"
  ([storage-spec import-spec progress-chan]
   (import-data storage-spec import-spec progress-chan {}))
  ([storage-spec import-spec progress-chan options]
   (if (:atomic? options)
     (with-transacted-storage [s storage-spec]
       (let [result (import-data* s import-spec progress-chan)]
         (-> result :wait deref)
         result))
     (with-storage [s storage-spec]
       (import-data* s import-spec progress-chan)))))
