(ns clj-money.import
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.java.io :as io]
            [clojure.core.async :refer [<!! >!! chan go] :as async]
            [environ.core :refer [env]]
            [stowaway.implicit :refer [with-storage
                                       with-transacted-storage]]
            [clj-money.validation :as validation]
            [clj-money.trading :as trading]
            [clj-money.accounts :refer [->criteria]]
            [clj-money.models.settings :as settings]
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
  [{:keys [entity] :as context} price]
  (let [commodity (commodities/find-by {:exchange (name (:exchange price))
                                        :symbol (:symbol price)
                                        :entity-id (:id entity)})]
    (prices/create (-> price
                       (assoc :commodity-id (:id commodity))
                       (dissoc :exchange :symbol))))
  context)

(defn- find-commodity
  [{:keys [commodities-by-symbol
           commodities-by-exchange-and-symbol]}
   {:keys [exchange symbol]}]
  (or (get commodities-by-exchange-and-symbol [exchange symbol])
      (get commodities-by-symbol symbol)))

(defn- update-account-relationships
  [context {:keys [parent-id id]}]
  (if parent-id
    (-> context
        (update-in [:account-children parent-id] (fnil conj #{}) id)
        (assoc-in [:account-parents id] parent-id))
    context))

(defn- build-path
  [account]
  (loop [a account
         path (:name account)]
    (if-let [parent (when (:parent-id a)
                      (accounts/find (:parent-id a)))]
      (recur parent (str (:name parent) "/" path))
      path)))

(defn- update-entity-settings
  [context account]
  (let [path (build-path account)]
    (if-let [setting (->> (get-in context [:import :options])
                          (filter #(= path (second %)))
                          ffirst)]
      (update-in context [:entity] #(entities/update
                                     (assoc-in %
                                               [:settings setting]
                                               (:id account))))
      context)))

(defn import-reconciliation
  [{:keys [accounts] :as context} reconciliation]
  (let [created (-> reconciliation
                    (update-in [:account-id] accounts)
                    (dissoc :id)
                    (assoc :balance 0M)
                    recs/create)]
    (update-in context [:account-recons]
               (fnil assoc {})
               (:account-id created) (:id created))))

(defn- prepare-budget-item
  [item {:keys [accounts]}]
  (if-let [account-id (accounts (:account-id item))]
    (assoc item :account-id account-id)
    (log/errorf "Unable to resolve account id s for budget item %s"
                item)))

(defn- prepare-budget
  [budget {:keys [entity] :as context}]
  (-> budget
      (update-in [:items]
                 (fn [items]
                   (->> items
                        (map #(prepare-budget-item % context))
                        (filter identity)
                        (remove #(= 0M (reduce + (:periods %)))))))
      (assoc :entity-id (:id entity))))

(defn- import-budget
  [context budget]
  (log/debugf "importing budget %s" (prn-str budget))
  (let [to-create (prepare-budget budget context)]
    (try
      (log/infof "imported budget %s"
                 (:name (budgets/create to-create)))
      (catch Exception e
        (log/errorf "error importing budget %s - %s: %s"
                    (.getClass e)
                    (.getMessage e)
                    (prn-str to-create)))))
  context)

(defn- import-commodity
  [{:keys [entity] :as context} commodity]
  (let [to-create (assoc commodity :entity-id (:id entity))
        {:keys [exchange symbol] :as created} (commodities/create to-create)]
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
        created (transactions/create to-create)]
    (log-transaction created "standard"))
  context)

(defn- inv-transaction-fee-info
  [{:keys [accounts]} transaction trans-type]
  (when-not (= 2 (count (:items transaction)))
    (let [non-commodity-items (filter #(= (if (= :buy trans-type)
                                            :credit
                                            :debit)
                                          (:action %))
                                      (:items transaction))
          account-ids (map #(get-in accounts [(:account-id %)])
                           non-commodity-items)
          accounts-map (->> (accounts/search {:id account-ids})
                            (map (juxt :id identity))
                            (into {}))
          fee-items (filter #(= :expense (get-in accounts-map [(accounts (:account-id %)) :type]))
                            non-commodity-items)]
      (when (seq fee-items)
        [(transduce (map :quantity) + fee-items)
         (accounts (:account-id (first fee-items)))]))))

(defmethod ^:private import-transaction :buy
  [{:keys [accounts] :as context} transaction]
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
        {result :transaction
         errors ::validation/errors} (trading/buy purchase)]
    (when (seq errors)
      (log/errorf "Unable to import purchase transaction %s: %s"
                  (prn-str purchase)
                  (prn-str errors)))
    (log-transaction result "commodity purchase"))
  context)

(defmethod ^:private import-transaction :sell
  [{:keys [accounts] :as context} transaction]
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
        {result :transaction} (trading/sell sale)]
    (log-transaction result "commodity sale"))
  context)

(defmethod ^:private import-transaction :transfer
  [{:keys [accounts] :as context}
   {:keys [from-account-id to-account-id] :as transaction}]
  (log/debug "import transfer transaction " (prn-str transaction))
  (let [from-commodity-account (accounts/find (accounts from-account-id))
        from-account (accounts/find (:parent-id from-commodity-account))
        to-commodity-account (accounts/find (accounts to-account-id))
        {result :transaction} (trading/transfer (assoc transaction
                                                       :transfer-date (:transaction-date transaction)
                                                       :commodity-id (:commodity-id from-commodity-account)
                                                       :from-account from-account
                                                       :to-account-id (:parent-id to-commodity-account)))]
    (log-transaction result "commodity transfer"))
  context)

; This is maybe too specific to GnuCash. It would be better if the
; gnucash namespace did these lookups
(defn- ensure-split-ids
  [{:keys [commodity-account-id commodity-id account-id] :as transaction}
   {:keys [accounts]}]
  (if (and account-id commodity-id)
    transaction
    (let  [commodity-account (accounts/find (accounts commodity-account-id))]
      (assoc transaction :commodity-id (:commodity-id commodity-account)
             :account-id (:parent-id commodity-account)))))

(defmethod ^:private import-transaction :split
  [context transaction]
  (let  [split (-> transaction
                   (ensure-split-ids context)
                   (dissoc :items))
         {result :transaction} (trading/split split)]
    (log-transaction result "commodity split"))
  context)

(defn- get-source-type
  [{:keys [content-type]}]
  (->> content-type
       (re-matches #"^application\/(.*)")
       second
       keyword))

(defn- prepare-input
  "Returns the input data and source type based
  on the specified image"
  [image-ids]
  (let [images (map #(images/find-by {:id %} {:include-body? true})
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
  [{:keys [accounts] :as context}
   {:keys [parent-id commodity] :as account}]
  (let [result (accounts/create (-> account
                                    (assoc :entity-id (-> context :entity :id)
                                           :commodity-id (:id (find-commodity
                                                               context
                                                               commodity))
                                           :parent-id (accounts parent-id))
                                    (dissoc :id :commodity)))]
    (when-not (:id result)
      (throw (ex-info (str
                       "Unable to create the account "
                       (validation/error-messages result))
                      {:result result})))
    (log/info (format "imported account \"%s\"" (:name result)))
    (-> context
        (assoc-in [:accounts (:id account)] (:id result))
        (update-account-relationships result)
        (update-entity-settings result))))

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
                (catch Exception e
                  (log/errorf e "unable to import record %s" record)
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
  (cons account-id
        (mapcat #(append-child-ids % account-children)
                (account-children account-id))))

(defn- fetch-reconciled-items
  [{:keys [account-id id]} {:keys [account-children earliest-date latest-date]}]
  (let [accounts (accounts/search {:id (if account-children
                                         (append-child-ids
                                          account-id
                                          account-children)
                                         account-id)})]
    (transactions/search-items (assoc (->criteria accounts
                                                  {:earliest-date earliest-date
                                                   :latest-date latest-date})
                                      :reconciliation-id id))))

(defn- process-reconciliation
  [recon ctx]
  (let [result (recs/update (assoc recon
                                   :balance (->> (fetch-reconciled-items recon ctx)
                                                 (map :polarized-quantity)
                                                 (reduce + 0M))
                                   :status :completed))]
    (when (validation/has-error? result)
      (log/errorf "Unable to finalize reconciliation: %s"
                  (prn-str (select-keys result
                                        [:id
                                         :account-id
                                         :balance
                                         :end-of-period
                                         ::validation/errors]))))))

(defn- process-reconciliations
  [{:keys [entity] :as ctx}]
  (doseq [recon (recs/search {[:account :entity-id] (:id entity)})]
    (process-reconciliation recon ctx)))

(defn- import-data*
  [import-spec progress-chan]
  (let [user (users/find (:user-id import-spec))
        [inputs source-type] (prepare-input (:image-ids import-spec))
        entity (entities/find-or-create user
                                        (:entity-name import-spec))
        wait-promise (promise)
        out-chan (chan)
        result-chan (async/transduce (comp import-record
                                           inc-progress)
                                     (notify-progress progress-chan)
                                     {:import import-spec
                                      :progress {}
                                      :accounts {}
                                      :entity entity}
                                     out-chan)]
    (go
      (try
        (let [result (transactions/with-delayed-balancing (:id entity)
                       (read-source source-type inputs out-chan)
                       (<!! result-chan))]
          (process-reconciliations (assoc result
                                          :earliest-date (settings/get :earliest-partition-date)
                                          :latest-date (settings/get :latest-partition-date)))
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
  ([import-spec progress-chan]
   (import-data import-spec progress-chan {}))
  ([import-spec progress-chan options]
   (if (:atomic? options)
     (with-transacted-storage (env :db)
       (let [result (import-data* import-spec progress-chan)]
         (-> result :wait deref)
         result))
     (with-storage (env :db)
       (import-data* import-spec progress-chan)))))
