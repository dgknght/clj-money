(ns clj-money.import
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clojure.java.io :as io]
            [clojure.core.async :refer [<! >! chan go pipe sliding-buffer] :as async]
            [java-time.api :as t]
            [dgknght.app-lib.validation :as v]
            [clj-money.db :as db]
            [clj-money.util :as util]
            [clj-money.models :as models]
            [clj-money.dates :as dates]
            [clj-money.trading :as trading]
            [clj-money.accounts :refer [->criteria]]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.settings :as settings]))

(defn- ignore?
  [record]
  (-> record meta :ignore?))

(defmulti read-source
  (fn [source-type & _]
    source-type))

(defn- import-price
  [{:keys [entity] :as context} price]
  (let [commodity (models/find-by #:commodity{:exchange (name (:exchange price))
                                              :symbol (:symbol price)
                                              :entity entity})]
    (models/put (-> price
                    (assoc :price/commodity commodity)
                    (dissoc :exchange :symbol))))
  context)

(defn- find-commodity
  [{:keys [commodities-by-symbol
           commodities-by-exchange-and-symbol]}
   {:keys [exchange symbol]}]
  (or (get commodities-by-exchange-and-symbol [exchange symbol])
      (get commodities-by-symbol symbol)))

(defn- update-account-relationships
  [context {:keys [id] :account/keys [parent]}]
  (if parent
    (-> context
        (update-in [:account-children (:id parent)] (fnil conj #{}) id)
        (assoc-in [:account-parents id] (:id parent)))
    context))

(defn- build-path
  [account]
  (loop [a account
         path (:name account)]
    (if-let [parent (when (:parent-id a)
                      (models/find (:account/parent a) :account))]
      (recur parent (str (:name parent) "/" path))
      path)))

(defn- update-entity-settings
  [context account]
  (let [path (build-path account)]
    (if-let [setting (->> (get-in context [:import :options])
                          (filter #(= path (second %)))
                          ffirst)]
      (update-in context [:entity] #(models/put
                                     (assoc-in %
                                               [:entity/settings setting]
                                               (util/->model-ref account))))
      context)))

(defn- prepare-budget-item
  [item {:keys [accounts]}]
  (if-let [account-id (accounts (:account-id item))]
    (assoc item :account-id account-id)
    (throw
      (ex-info
        (format "Unable to resolve account id %s for the budget item."
                (:account-id item))
        {:item item}))))

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
  (let [to-create (prepare-budget budget context)]
    (try
      (log/infof "imported budget %s"
                 (:budget/name (models/put to-create)))
      (catch Exception e
        (log/errorf "error importing budget %s - %s: %s"
                    (.getClass e)
                    (.getMessage e)
                    (prn-str to-create)))))
  context)

(defn- import-commodity
  [{:keys [entity] :as context} commodity]
  (let [{:commodity/keys [exchange symbol]
         :as created} (-> commodity
                          (update-in [:commodity/exchange] (fnil identity :nyse))
                          (assoc :commodity/entity entity
                                 :commodity/price-config {:enabled true}) ; TODO: read this from import source
                          models/put)]
    (log/infof "imported commodity %s (%s)"
               (:commodity/name created)
               symbol)
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
  (if (v/has-error? transaction)
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
        created (models/put to-create)]
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
          accounts-map (->> (models/select
                              (db/model-type
                                {:id [:in account-ids]}
                                :account))
                            (map (juxt :id identity))
                            (into {}))
          fee-items (filter #(= :expense (get-in accounts-map [(accounts (:account-id %)) :type]))
                            non-commodity-items)]
      (when (seq fee-items)
        [(transduce (map :quantity) + fee-items)
         (accounts (:account-id (first fee-items)))]))))

(defmethod ^:private import-transaction :buy
  [{:keys [accounts] :as context} transaction]
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
         errors ::v/errors} (trading/buy purchase)]
    (when (seq errors)
      (log/errorf "Unable to import purchase transaction %s: %s"
                  (prn-str purchase)
                  (prn-str errors)))
    (log-transaction result "commodity purchase"))
  context)

(defmethod ^:private import-transaction :sell
  [{:keys [accounts] :as context} transaction]
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
  (let [find-account (models/find :account)
        from-commodity-account (find-account (accounts from-account-id))
        from-account (find-account (:parent-id from-commodity-account))
        to-commodity-account (find-account (accounts to-account-id))
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
    (let  [commodity-account (models/find (accounts commodity-account-id)
                                          :account)]
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
  ; TODO: Make sure the image body is included here, but not included when not wanted elsewhere
  (let [images (map #(models/find-by (db/model-type {:id %} :image))
                    image-ids)
        source-type (-> images first get-source-type)]
    [(map #(io/input-stream (byte-array (:body %)))
          images)
     source-type]))

(defmulti import-record*
  (fn [_ record]
    (-> record meta :record-type)))

(defmethod import-record* :declaration
  [context _]
  context)

(defmethod import-record* :account
  [{:keys [accounts] :as context}
   {:import/keys [parent-id commodity] :as account}]
  (let [result (models/put (-> account
                               (assoc :account/entity (:entity context)
                                      :account/commodity (find-commodity
                                                           context
                                                           commodity)
                                      :account/parent (accounts parent-id))
                               (dissoc :import/id
                                       :import/commodity
                                       :import/parent-id)))]
    (log/info (format "imported account \"%s\"" (:account/name result)))
    (-> context
        (assoc-in [:accounts (:id account)] (:id result))
        (update-account-relationships result)
        (update-entity-settings result))))

(defmethod import-record* :reconciliation
  [{:keys [accounts] :as context} reconciliation]
  (let [created (-> reconciliation
                    (update-in [:reconciliation/account] (comp accounts :id))
                    (dissoc :id)
                    (assoc :reconciliation/balance 0M)
                    models/put)]
    (update-in context [:account-recons]
               (fnil assoc {})
               (:account-id created) (:id created))))

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
              (assoc :transaction-item/reconciliation
                     (if (:transaction-item/reconciled %)
                       (find-reconciliation-id (:transaction-item/account-id %) ctx)
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

(def ^:private day-keys
  [:sunday
   :monday
   :tuesday
   :wednesday
   :thursday
   :friday
   :saturday])

(defn- infer-date-spec-day
  [date]
  (if (dates/last-day-of-the-month? date)
    :last
    (t/day-of-month date)))

(defn- infer-date-spec
  [{:keys [start-date last-occurrence interval-type]}]
  (let [date (or last-occurrence start-date)]
    (case interval-type
      :year {:day (infer-date-spec-day date)
             :month (dates/month date)}
      :month {:day (infer-date-spec-day date)}
      :week {:days [(nth day-keys (dates/day-of-week date))]})))

(defmethod import-record* :scheduled-transaction
  [{:keys [entity accounts] :as context} sched-tran]
  (let [created (models/put
                  (-> sched-tran
                      (assoc :scheduled-transaction/entity entity
                             :scheduled-transaction/date-spec (infer-date-spec sched-tran))
                      (update-in [:scheduled-transaction/items]
                                 (fn [items]
                                   (map (fn [{:import/keys [account-id] :as i}]
                                          (-> i
                                              (assoc :scheduled-transaction-item/account
                                                     (accounts account-id))
                                              (dissoc :import/account-id)))
                                        items)))))]
    (log/infof "Imported scheduled transaction %s"
               (:scheduled-transaction/description created)))
  context)

(defmethod import-record* :budget
  [context budget]
  (import-budget context budget))

(defmethod import-record* :price
  [context price]
  (import-price context price))

(defmethod import-record* :commodity
  [context commodity]
  (import-commodity context commodity))

(defn- report-error
  [ctx msg data]
  (update-in ctx
             [:progress :errors]
             (fnil conj [])
             {:message msg
              :data data}))

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
                  (report-error context (.getMessage e) (ex-data e))))
           record)))))

(defn- append-child-ids
  [account-id account-children]
  (cons account-id
        (mapcat #(append-child-ids % account-children)
                (account-children account-id))))

(defn- fetch-reconciled-items
  [{:keys [account-id id]} {:keys [account-children earliest-date latest-date]}]
  (let [accounts (models/select
                   (db/model-type
                     {:id (if account-children
                            [:in (append-child-ids
                                   account-id
                                   account-children)]
                            account-id)}
                     :account))]
    (models/select (assoc (->criteria accounts
                                      {:earliest-date earliest-date
                                       :latest-date latest-date})
                          :transaction-tiem/reconciliationd (util/->model-ref id)))))

(defn- process-reconciliation
  [recon ctx]
  (models/put
    (assoc recon
           :reconciliation/balance (->> (fetch-reconciled-items recon ctx)
                                        (map :transaction-item/polarized-quantity)
                                        (reduce + 0M))
           :reconciliation/status :completed)))

(defn- process-reconciliations
  [{:keys [entity] :as ctx} out-chan]
  (let [reconciliations (models/select
                          (db/model-type
                            {:account/entity entity}
                            :reconciliation))
        progress {:total (count reconciliations)}
        done (promise)]
    (go
      (>! out-chan progress)
      (->> reconciliations
           (map #(process-reconciliation % ctx))
           (map-indexed (fn [i _]
                          (go
                            (>! out-chan (assoc progress :completed (+ 1 i))))))
           doall)
      (deliver done true))
    done))

(defn- forward
  "Returns a transducer that places received values on the specified channel"
  [c]
  (fn [xf]
    (fn
      ([acc] (xf acc))
      ([acc v]
       (go (>! c v))
       (xf acc v)))))

(defn- classify-progress
  [progress-type]
  (fn [xf]
    (fn
      ([acc] (xf acc))
      ([acc record]
       (xf acc (assoc record :type progress-type))))))

(defmulti update-progress-state
  (fn [_state record]
    (if (= ::finished record)
      :finish
      (let [record-type (-> record meta :record-type)]
        (case record-type
          nil :direct ; this comes from models/transactions or here and is basically ready to go
          :declaration :init ; this comes from a declaration in the import source indicating the number or records to expect
          :imported))))) ; this is an imported record

(defmethod update-progress-state :finish
  [state _record]
  (assoc state :finished true))

(defmethod update-progress-state :direct
  [state record]
  (assoc state (:type record) (dissoc record :type)))

(defmethod update-progress-state :init
  [state record]
  (assoc state
         (:record-type record)
         (-> record
             (rename-keys {:record-count :total})
             (dissoc :record-type)
             (assoc :completed 0))))

(defmethod update-progress-state :imported
  [state record]
  (update-in state
             [(-> record meta :record-type)
              :completed]
             (fnil inc 0)))

(defn- ->progress
  "Returns a transducer that takes an imported record and converts it
  into a progress record"
  []
  (let [state (atom {})]
    (fn
      [xf]
      (fn
        ([acc] (xf acc))
        ([acc record]
         (swap! state update-progress-state record)
         (xf acc @state))))))

(defmulti filter-behavior
  (fn [record _state]
    (-> record meta :record-type)))

(defmethod filter-behavior :default
  [_record _state]
  :import)

(defmethod filter-behavior :price
  [{:keys [trade-date] :as price} state]
  (let [cache-key (select-keys price [:symbol :exchange])
        last-trade-date (get-in @state [cache-key] (t/local-date 1000 1 1))]
    (or
      (when (t/before? last-trade-date
                       (t/minus trade-date
                                (t/months 1)))
        (swap! state assoc cache-key trade-date)
        :import)
      :ignore)))

(defn- filter-import
  "Returns a transducing fn that drops records that can be omitted"
  []
  (let [state (atom {})]
    (fn [xf]
      (completing
        (fn [acc record]
          (case (filter-behavior record state)
            :import (xf acc record)
            :ignore (xf acc (vary-meta record assoc :ignore? true))
            :drop   (xf acc)))))))

; Import steps
; read input -> stream of records
; rebalance accounts -> stream of accounts (just counts?)
; process reconciliations -> stream of (just counts?)
;
; source => import record =>
;                            \
; imported-chan                => filter-for-progress => ->progress-record =>
;
; rebalance-chan                                  => ->progress-record =>  - =>  inc-progress => update-import
;                                                                         /
; reconcilations                                  => ->progress-record =>

; The source path does these things
; 1. import-record accepts the import record, writes the database record, passes on the database record
; 2. filt

(defn- import-data*
  [import-spec progress-chan]
  (let [user (models/find (:user-id import-spec) :user)
        [inputs source-type] (prepare-input (:image-ids import-spec))
        entity ((some-fn models/find-by models/put)
                {:entity/user user
                 :entity/name (:entity-name import-spec)})
        wait-promise (promise)
        source-chan (chan)
        rebalance-chan (chan 1 (classify-progress :account-balance))
        reconciliations-chan (chan 1 (classify-progress :process-reconciliation))
        prep-chan (chan (sliding-buffer 1) (->progress))
        read-source-result-chan (async/transduce
                                  (comp (filter-import)
                                        import-record
                                        (forward prep-chan))
                                  (fn
                                    ([ctx] ctx)
                                    ([ctx record]
                                     (if (db/model-type? record :account)
                                       (assoc-in ctx [:accounts (:id record)] record)
                                       ctx)))
                                  {:import import-spec
                                   :accounts {}
                                   :entity entity}
                                  source-chan)]
    (pipe rebalance-chan prep-chan false)
    (pipe reconciliations-chan prep-chan false)
    (pipe prep-chan progress-chan false)
    (go
      (try
        (let [result  (transactions/with-delayed-balancing (:id entity) rebalance-chan
                        (read-source source-type inputs source-chan)
                        (<! read-source-result-chan))]
          (deref
            (process-reconciliations (assoc result
                                            :earliest-date (settings/get :earliest-partition-date)
                                            :latest-date (settings/get :latest-partition-date))
                                     reconciliations-chan))
          (>! prep-chan ::finished))
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
     (throw (UnsupportedOperationException. "Atomic imports are not supported"))
     (import-data* import-spec progress-chan))))
