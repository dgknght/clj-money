(ns clj-money.import
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clojure.core.async :refer [<!! >! chan go pipe sliding-buffer] :as async]
            [clojure.spec.alpha :as s]
            [java-time.api :as t]
            [dgknght.app-lib.validation :as v]
            [clj-money.db :as db]
            [clj-money.util :as util]
            [clj-money.models :as models]
            [clj-money.dates :as dates]
            [clj-money.trading :as trading]
            [clj-money.accounts :refer [->>criteria]]
            [clj-money.transactions :refer [polarize-item-quantity]]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.settings :as settings]))

(defmulti read-source
  (fn [source-type & _]
    source-type))

; TODO: change this to something makes its way to the output
(defn- validate
  [m spec]
  (when-let [errors (s/explain-data spec m)]
    (pprint errors))
  m)

(defn- remove-keys-by-ns
  [m ns]
  (with-meta
    (->> m
         (remove #(= ns (namespace (first %))))
         (into {}))
    (meta m)))

(defn- purge-import-keys
  [m]
  (remove-keys-by-ns m "import"))

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
   {:commodity/keys [exchange symbol]}]
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
         path (:account/name account)]
    (if-let [parent (when-let [p (:account/parent a)]
                      (models/find p :account))]
      (recur parent (str (:account/name parent) "/" path))
      path)))

(defn- update-entity-investing-account
  [entity account context]
  (let [path (build-path account)]
    (if-let [setting (->> (get-in context [:import :import/options])
                          (filter #(= path (second %)))
                          ffirst)]
      (assoc-in entity
                [:entity/settings setting]
                (util/->model-ref account))
      entity)))

(defn- update-entity-default-commodity
  [entity {:keys [commodities]}]
  (if (get-in entity [:entity/settings :settings/default-commodity])
    entity
    (if-let [commodity (->> commodities
                            (filter #(= :currency (:commodity/type %)))
                            first)]
      (assoc-in entity
                [:entity/settings
                 :settings/default-commodity]
                (util/->model-ref commodity))
      entity)))

(defn- update-entity-settings
  [context account]
  (update-in
    context
    [:entity]
    (fn [entity]
      (let [updated (-> entity
                        (update-entity-investing-account account context)
                        (update-entity-default-commodity context))]
        (if (= updated entity)
          entity
          (models/put updated))))))

(defn- resolve-account-references
  [{:keys [account-ids]} items]
  (map (fn [{:import/keys [account-id] :as item}]
         (-> item
             (assoc :transaction-item/account {:id (account-ids account-id)})
             purge-import-keys))
       items))

(defn- prepare-transaction
  [transaction {:as context :keys [entity]}]
  (-> transaction
      (update-in [:transaction/items] #(resolve-account-references context %))
      (assoc :transaction/entity entity)
      purge-import-keys))

(defmulti ^:private import-transaction
  (fn [_ transaction]
    (:trade/action transaction)))

(defn- log-transaction
  [transaction transaction-type]
  (log/infof "imported %s transaction on %s: \"%s\""
             transaction-type
             (:transaction/transaction-date transaction)
             (:transaction/description transaction)))

(defmethod ^:private import-transaction :default
  [context transaction]
  (-> transaction
      (prepare-transaction context)
      (validate ::models/transaction)
      models/put
      (log-transaction "standard"))
  context)

(defn- inv-transaction-fee-info
  [{:keys [account-ids]} transaction trans-type]
  (when-not (= 2 (count (:transaction/items transaction)))
    (let [non-commodity-items (filter #(= (if (= :buy trans-type)
                                            :credit
                                            :debit)
                                          (:transaction-tiem/action %))
                                      (:transaction/items transaction))
          item-account-ids (map #(get-in account-ids [(:import/account-id %)])
                           non-commodity-items)
          accounts-map (->> (models/select
                              (db/model-type
                                {:id [:in item-account-ids]}
                                :account))
                            (map (juxt :id identity))
                            (into {}))
          fee-items (filter #(= :expense
                                (get-in accounts-map
                                        [(account-ids (:import/account-id %))
                                         :account/type]))
                            non-commodity-items)]
      (when (seq fee-items)
        [(transduce (map :item/quantity) + fee-items)
         (account-ids (:import/account-id (first fee-items)))]))))

(defmethod ^:private import-transaction :buy
  [{:keys [account-ids] :as context}
   {:trade/keys [shares value]
    :transaction/keys [transaction-date]
    :import/keys [commodity-account-id account-id]
    :as transaction}]
  (let [[fee fee-account-id] (inv-transaction-fee-info context transaction :buy)
        commodity-id (->> context
                          :commodities
                          (filter #(and (= (:commodity/symbol %)
                                           (:commodity/symbol transaction))
                                        (= (:commodity/exchange %)
                                           (:commodity/exchange transaction))))
                          first
                          :id)
        purchase (cond-> #:trade{:date transaction-date
                                 :shares shares
                                 :value value}
                   commodity-id         (assoc :trade/commodity {:id commodity-id})
                   commodity-account-id (assoc :trade/commodity-account {:id (account-ids commodity-account-id)})
                   account-id           (assoc :trade/account {:id (account-ids account-id)})
                   fee                  (assoc :trade/fee fee
                                               :trade/fee-account {:id fee-account-id}))
        {result :transaction} (trading/buy purchase)]
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
  [{:image/keys [content-type]}]
  (->> content-type
       (re-matches #"^application\/(.*)")
       second
       keyword))

(defn- prepare-input
  "Returns the input data and source type based
  on the specified image"
  [image-refs]
  ; TODO: Make sure the image body is included here, but not included when not wanted elsewhere
  (let [images (map #(models/find % :image)
                    image-refs)
        source-type (-> images first get-source-type)]
    [(map #(io/input-stream (byte-array (:image/body %)))
          images)
     source-type]))

(defmulti import-record*
  (fn [_ record]
    (:import/record-type record)))

(defmethod import-record* :declaration
  [context _]
  context)

(defmethod import-record* :account
  [{:keys [account-ids] :as context}
   {:import/keys [parent-id commodity id] :as account}]
  (let [result (models/put (-> account
                               (assoc :account/entity (:entity context)
                                      :account/commodity (find-commodity
                                                           context
                                                           commodity)
                                      :account/parent (when-let [parent-id (account-ids parent-id)]
                                                        {:id parent-id}))
                               purge-import-keys
                               (validate ::models/account)))]
    (log/infof "imported account \"%s\"" (:account/name result))
    (-> context
        (assoc-in [:account-ids id] (:id result))
        (update-account-relationships result)
        (update-entity-settings result))))

(defmethod import-record* :reconciliation
  [{:keys [account-ids] :as context} {:import/keys [account-id] :as reconciliation}]
  (let [created (-> reconciliation
                    (assoc :reconciliation/balance 0M
                           :reconciliation/status :new
                           :reconciliation/account {:id (account-ids account-id)})
                    purge-import-keys
                    (validate ::models/reconciliation)
                    models/put)]
    ; We'll use this map later to assocate reconciled transactions
    ; for this account with this reconciliation
    (update-in context [:account-recons]
               (fnil assoc {})
               (-> created :reconciliation/account :id)
               (:id created))))

(defn- find-reconciliation-id
  [old-account-id {:keys [account-ids account-recons account-parents]}]
  (loop [id (account-ids old-account-id)]
    (when id
      (if-let [recon-id (account-recons id)]
        recon-id
        (recur (account-parents id))))))

; This is probably pretty specific to the gnucash format
(defn- refine-recon-info
  "Given an imported transaction item, if the import/reconciled? attribute is true,
  add a transaction-item/reconciliation attribute."
  [{:keys [account-recons] :as ctx} items]
  (if account-recons
    (mapv (fn [{:as item :import/keys [reconciled? account-id]}]
           (cond-> item
             reconciled? (assoc :transaction-item/reconciliation
                                (util/->model-ref (find-reconciliation-id account-id ctx)))))
         items)
    items))

(defmethod import-record* :transaction
  [context transaction]
  (import-transaction context
                      (update-in transaction
                                 [:transaction/items]
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

(defn- prepare-budget-item
  [item {:keys [account-ids]}]
  (if-let [account-id (account-ids (:import/account-id item))]
    (-> item
        (assoc :budget-item/account {:id account-id})
        purge-import-keys)
    (throw
      (ex-info
        (format "Unable to resolve account id %s for the budget item."
                (:import/account-id item))
        {:item item}))))

(defn- prepare-budget
  [budget {:keys [entity] :as context}]
  (-> budget
      (update-in [:budget/items]
                 (fn [items]
                   (->> items
                        (map #(prepare-budget-item % context))
                        (filter identity)
                        (remove #(= 0M (reduce + (:budget-item/periods %)))))))
      purge-import-keys
      (assoc :budget/entity entity)))

(defmethod import-record* :budget
  [context budget]
  (let [to-create (prepare-budget budget context)]
    (try
      (let [imported (models/put to-create)]
        (log/infof "imported budget %s" (:budget/name imported)))
      (catch Exception e
        (log/errorf "error importing budget %s - %s: %s"
                    (.getClass e)
                    (.getMessage e)
                    (prn-str to-create))))))

(defmethod import-record* :price
  [context price]
  (import-price context price))

(defmethod import-record* :commodity
  [{:keys [entity] :as context} commodity]
  (let [{:commodity/keys [exchange symbol]
         :as created} (-> commodity
                          (assoc :commodity/entity entity
                                 :commodity/price-config {:price-config/enabled true}) ; TODO: read this from import source
                          purge-import-keys
                          (validate ::models/commodity)
                          models/put)]
    (log/infof "imported commodity %s (%s)"
               (:commodity/name created)
               symbol)
    (-> context
        (update-in [:commodities] (fnil conj []) created)
        (update-in [:commodities-by-symbol] (fnil assoc {}) symbol created)
        (update-in [:commodities-by-exchange-and-symbol] (fnil assoc {}) exchange symbol))))

(defn- assoc-error
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
     (if (:import/ignore? record)
       (xf context record)
       (xf (try (import-record* context record)
                (catch Exception e
                  (log/errorf e "unable to import record %s" record)
                  (assoc-error context (.getMessage e) (ex-data e))))
           record)))))

(defn- append-child-ids
  [account-id account-children]
  (cons account-id
        (mapcat #(append-child-ids % account-children)
                (account-children account-id))))

(defn- fetch-reconciled-items
  [{:reconciliation/keys [account]
    :keys [id]}
   {:keys [account-children
           earliest-date
           latest-date]}]
  (let [accounts (models/select
                   (db/model-type
                     {:id (if account-children
                            [:in (append-child-ids
                                   (:id account)
                                   account-children)]
                            (:id account))}
                     :account))]
    (models/select (assoc (->>criteria {:earliest-date earliest-date
                                        :latest-date latest-date}
                                       accounts)
                          :transaction-item/reconciliation (util/->model-ref id)))))

(defn- process-reconciliation
  [{:reconciliation/keys [account] :as recon} ctx]
  (let [account (models/find account :account)
        updated (assoc recon
                       :reconciliation/balance (->> (fetch-reconciled-items recon ctx)
                                                    (map (comp :transaction-item/polarized-quantity
                                                               polarize-item-quantity
                                                               #(assoc % :transaction-item/account account)))
                                                    (reduce + 0M))
                       :reconciliation/status :completed)]
    (models/put updated)))

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
    (completing
      (fn
        [acc v]
        (go (>! c v))
        (xf acc v)))))

(defmulti update-progress-state
  (fn [_state record]
    (if (= ::finished record)
      :finish
      (case (:import/record-type record)
        (:account-balance :process-reconciliation) :direct ; this comes from models/transactions or here and is basically ready to go
        :declaration :init ; this comes from a declaration in the import source indicating the number or records to expect
        :imported)))) ; this is an imported record

(defmethod update-progress-state :finish
  [state _record]
  (assoc state :finished true))

(defmethod update-progress-state :direct
  [state record]
  (assoc state (:import/record-type record) (dissoc record :import/record-type)))

(defmethod update-progress-state :init
  [state {:declaration/keys [record-count record-type]}]
  (assoc state
         record-type
         {:total record-count
          :completed 0}))

(defmethod update-progress-state :imported
  [state record]
  (update-in state
             [(:import/record-type record)
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
            :ignore (xf acc (assoc record :import/ignore? true))
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
  (let [user (models/find (:import/user import-spec) :user)
        [inputs source-type] (prepare-input (:import/images import-spec))
        entity ((some-fn models/find-by models/put)
                {:entity/user user
                 :entity/name (:import/entity-name import-spec)})
        wait-promise (promise)
        source-chan (chan)
        rebalance-chan (chan 1 (map #(assoc % :import/record-type :account-balance)))
        reconciliations-chan (chan 1 (map #(assoc % :import/record-type :process-reconciliation)))
        prep-chan (chan (sliding-buffer 1) (->progress))
        read-source-result-chan (async/transduce
                                  (comp (filter-import)
                                        import-record
                                        (forward prep-chan))
                                  (completing (fn [acc _] acc))
                                  {:import import-spec
                                   :account-ids {}
                                   :entity entity}
                                  source-chan)]
    (pipe rebalance-chan prep-chan false)
    (pipe reconciliations-chan prep-chan false)
    (pipe prep-chan progress-chan false)
    (go
      (try
        (let [result (transactions/with-delayed-balancing [(:id entity) rebalance-chan]
                       (read-source source-type inputs source-chan)
                       (<!! read-source-result-chan))]
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
