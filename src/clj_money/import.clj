(ns clj-money.import
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clojure.core.async :as a]
            [clojure.spec.alpha :as s]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [uuid]]
            [clj-money.util :as util]
            [clj-money.dates :as dates]
            [clj-money.progress :as prog]
            [clj-money.images :as images]
            [clj-money.models :as models]
            [clj-money.trading :as trading]
            [clj-money.accounts :refer [->>criteria
                                        expense?]]
            [clj-money.transactions :refer [polarize-item-quantity]]))

(defmacro with-fatal-exceptions
  [& body]
  `(try
    ~@body
    (catch Exception e#
      (throw (ex-info (ex-message e#)
                      (assoc (ex-data e#) ::fatal? true)
                      e#)))))

(defmulti read-source
  (fn [source-type & _]
    source-type))

(defn- assoc-warning
  [ctx msg data]
  (log/warnf "[import] %s" msg)
  (update-in ctx
             [:progress :notifications]
             conj
             {:import/record-type :notification
              :notification/id (uuid)
              :notification/severity :warning
              :notification/message msg
              :notification/data data}))

(defn- validate
  [m spec]
  (when-let [errors (s/explain-data spec m)]
    (throw (ex-info (format "Invalid model %s" (util/simplify m))
                    {:model m
                     :explain errors})))
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

(defn- update-account-relationships
  [context {:keys [id] :account/keys [parent]}]
  (if parent
    (-> context
        (update-in [:account-children (:id parent)] (fnil conj #{}) id)
        (assoc-in [:account-parents id] (:id parent)))
    context))

(defn- build-path
  [account accounts]
  (loop [a account
         path (:account/name account)]
    (if-let [parent (when-let [p (:account/parent a)]
                      (accounts (:id p)))]
      (recur parent (str (:account/name parent) "/" path))
      path)))

(defn- update-entity-investing-account
  [entity account {:as context :keys [accounts]}]
  (let [path (build-path account accounts)]
    (if-let [setting (->> (get-in context [:import :import/options])
                          (filter #(= path (second %)))
                          ffirst)]
      (assoc-in entity
                [:entity/settings (keyword "settings" (name setting))]
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

(defn- resolve-account-reference
  ([ctx] #(resolve-account-reference % ctx))
  ([{:as item :import/keys [account-id]} {:keys [account-ids accounts]}]
   (assoc item
          :transaction-item/account
          (-> account-id account-ids accounts))))

(defn- prepare-transaction
  [transaction {:keys [entity]}]
  (-> transaction
      (assoc :transaction/entity entity)
      purge-import-keys))

(defmulti ^:private import-transaction
  (fn [_ transaction]
    (log/debugf "[import] import-transaction %s" (:trade/action transaction :default))
    (:trade/action transaction)))

(defn- log-transaction
  [transaction transaction-type]
  (log/infof "[import] imported %s transaction on %s: \"%s\""
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
  [{:keys [account-ids accounts]} {:transaction/keys [items]}]
  ; For a purchase
  ; - credit the cash account for the amount paid
  ; - debit the commodity account for the quantity of shares and the value of product of the shares and the price
  ; - debit the expense account the amount of the expense
  ; For a sale
  ; - credit the commodity account for the quantity of shares and the value of the product of the shares and the price
  ; - debit the cash account for the amount received
  ; - debit the expense account for the amount of the expense
  (when-let [exp-items (->> items
                            (filter (comp expense?
                                          accounts
                                          account-ids
                                          :import/account-id))
                            seq)]
    [(->> exp-items
          (map :transaction-item/quantity)
          (reduce + 0M))
     (account-ids (:import/account-id (first exp-items)))]))

(defmethod ^:private import-transaction :buy
  [{:keys [account-ids] :as context}
   {:trade/keys [shares value]
    :transaction/keys [transaction-date]
    :import/keys [commodity-account-id account-id]
    :as transaction}]
  (let [[fee fee-account-id] (inv-transaction-fee-info context transaction)
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
        {result :trade/transaction} (trading/buy purchase)]
    (log-transaction result "commodity purchase"))
  context)

(defn- find-commodity
  [{:keys [commodities-by-symbol
           commodities-by-exchange-and-symbol]}
   {:commodity/keys [exchange symbol] :as commodity}]
  (or (get commodities-by-exchange-and-symbol [exchange symbol])
      (get commodities-by-symbol symbol)
      (throw (ex-info (format "Unable to find the commodity \"%s\"" symbol)
                      commodity))))

(defmethod ^:private import-transaction :sell
  [{:keys [account-ids] :as context}
   {:as transaction
    :import/keys [commodity-account-id]
    :transaction/keys [transaction-date]
    :trade/keys [shares value]}]
  (let [[fee fee-account-id] (inv-transaction-fee-info context transaction)
        sale (cond-> #:trade{:commodity-account {:id (account-ids commodity-account-id)}
                             :date transaction-date
                             :shares shares
                             :value value}
               fee (assoc :trade/fee fee
                          :trade/fee-account {:id fee-account-id}))
        {:trade/keys [transaction]} (trading/sell sale)]
    (log-transaction transaction "commodity sale"))
  context)

(defmethod ^:private import-transaction :transfer
  [{:keys [account-ids accounts account-parents] :as context}
   {:import/keys [from-account-id to-account-id]
    :transaction/keys [transaction-date]
    :transfer/keys [shares]}]
  (let [from-commodity-account-id (account-ids from-account-id)
        from-account {:id (account-parents from-commodity-account-id)}
        to-commodity-account-id (account-ids to-account-id)
        to-account {:id (account-parents to-commodity-account-id)}
        commodity (:account/commodity (accounts from-commodity-account-id))
        xfr #:transfer{:date transaction-date
                       :from-account from-account
                       :to-account to-account
                       :commodity commodity
                       :shares shares}
        _ (log/debugf "[import] transfer %s" xfr)
        {result :transfer/transaction} (trading/transfer xfr)]
    (log-transaction result "commodity transfer"))
  context)

(defmethod ^:private import-transaction :split
  [{:as context
    :keys [account-parents
           account-ids
           accounts]}
   {:as transaction
    :import/keys [commodity-account-id]}]
  ; this logic to adjust accounts may be specific to gnucash
  (let  [{result :split/transaction} (-> transaction
                                         (select-keys [:split/date :split/shares-gained])
                                         (assoc :split/account {:id (-> commodity-account-id
                                                                        account-ids
                                                                        account-parents)}
                                                :split/commodity (-> commodity-account-id
                                                                     account-ids
                                                                     accounts
                                                                     :account/commodity))
                                         trading/split)]
    (log-transaction result "commodity split"))
  context)

(defn- get-source-type
  [{:image/keys [content-type]}]
  (->> content-type
       (re-matches #"^application\/(.*)")
       second
       keyword))

(defn- image-content
  [{:image/keys [uuid]}]
  (-> (images/get uuid)
      byte-array
      io/input-stream))

(defmulti import-record*
  (fn [_ record]
    (:import/record-type record)))

(defmethod import-record* :declaration
  [context _]
  context)

(defn- account-parent
  "Returns a model reference for the parent if the parent ID is found.

  The ID may not be found because we ignore the parents for the basic account types."
  [{:import/keys [parent-id]}
   {:keys [account-ids]}]
  (when-let [id (account-ids parent-id)]
    {:id id}))

(defmethod import-record* :account
  [{:keys [entity] :as context}
   {:import/keys [commodity id] :as account}]
  (with-fatal-exceptions
    (let [result (-> account
                     (assoc :account/entity entity
                            :account/commodity (find-commodity context commodity)
                            :account/parent (account-parent account context))
                     purge-import-keys
                     (validate ::models/account)
                     models/put)]
      (log/infof "[import] imported account \"%s\": %s -> %s"
                 (:account/name result)
                 id
                 (:id result))
      (-> context
          (assoc-in [:account-ids id] (:id result))
          (assoc-in [:accounts (:id result)] result)
          (update-account-relationships result)
          (update-entity-settings result)))))

(defmethod import-record* :reconciliation
  [{:keys [account-ids] :as context} {:import/keys [account-id] :as recon}]
  (if-let [new-id (account-ids account-id)]
    (let [created (-> recon
                      (assoc :reconciliation/balance 0M
                             :reconciliation/status :new
                             :reconciliation/account {:id new-id})
                      purge-import-keys
                      (validate ::models/reconciliation)
                      models/put)]
      ; We'll use this map later to assocate reconciled transactions
      ; for this account with this reconciliation
      (update-in context [:account-recons]
                 (fnil assoc {})
                 (-> created :reconciliation/account :id)
                 (:id created)))
    (assoc-warning
      context
      (format "Unable to resolve account %s for reconciliation on %s"
              account-id
              (:reconciliation/end-of-period recon))
      {:reconciliation recon})))

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
  ([ctx]
   (if (:account-recons ctx)
     #(refine-recon-info % ctx)
     identity))
  ([{:as item :import/keys [reconciled? account-id]} ctx]
   (cond-> item
     reconciled? (assoc :transaction-item/reconciliation
                        (util/->model-ref (find-reconciliation-id account-id ctx))))))

(defn- remove-zero-quantity-items
  [items]
  (remove #(zero? (:transaction-item/quantity %)) items))

(defn- propagate-item
  [context]
  (fn
    [{:transaction-item/keys [polarized-quantity account] :as item}]
    {:pre [(:transaction-item/polarized-quantity item)
           (:transaction-item/account item)]}
    (let [{:transaction-item/keys [balance index]}
          (get-in context
                  [:last-trxs (:id account)]
                  #:transaction-item{:balance 0M
                                     :index -1})]
      (assoc item
             :transaction-item/balance (+ balance polarized-quantity)
             :transaction-item/index (inc index)))))

(defn- update-last-trxs
  [context {:transaction/keys [items]}]
  (update-in context
             [:last-trxs]
             #(reduce (fn [c {:as item {:keys [id]} :transaction-item/account}]
                        (assoc c id item))
                      %
                      items)))

(defmethod import-record* :transaction
  [context transaction]
  (with-fatal-exceptions
    (let [trx (update-in transaction
                         [:transaction/items]
                         (comp #(map (comp purge-import-keys
                                           (propagate-item context)
                                           polarize-item-quantity
                                           (resolve-account-reference context)
                                           (refine-recon-info context))
                                     %)
                               remove-zero-quantity-items))]
      (if (empty? (:transaction/items trx))
        (do
          (log/warnf "[import] Transaction with no items: %s" trx)
          (assoc-warning context "Transaction with no items" trx))
        (-> context
            (import-transaction trx)
            (update-last-trxs trx)
            (update-in [:entity]
                       dates/push-model-boundary
                       :entity/transaction-date-range 
                       (:transaction/transaction-date trx)))))))

(defmethod import-record* :scheduled-transaction
  [{:keys [entity account-ids]
    :as context}
   sched-tran]
  (let [created (-> sched-tran
                    (assoc :scheduled-transaction/entity entity)
                    (update-in [:scheduled-transaction/items]
                               (fn [items]
                                 (map (fn [{:import/keys [account-id] :as i}]
                                        (-> i
                                            (assoc :scheduled-transaction-item/account
                                                   {:id (account-ids account-id)})
                                            purge-import-keys))
                                      items)))
                    purge-import-keys
                    models/put)]
    (log/infof "[import] imported scheduled transaction %s"
               (:scheduled-transaction/description created)))
  context)

(defn- prepare-budget-item
  [item {:keys [account-ids]}]
  (if-let [account-id (account-ids (:import/account-id item))]
    (-> item
        (assoc :budget-item/account {:id account-id})
        purge-import-keys)
    (log/warnf "Unable to resolve account id for budget item %s" item)))

(defn- prepare-budget
  [budget {:keys [entity]}]
  (-> budget
      purge-import-keys
      (assoc :budget/entity entity)))

(defn- prepare-budget-items
  [budget context items]
  (->> items
       (map (comp #(prepare-budget-item % context)
                  #(assoc % :budget-item/budget budget)))
       (filter identity)
       (remove #(= 0M (reduce + (:budget-item/periods %))))
       (map #(update-in % [:budget-item/periods] vec))))

(defmethod import-record* :budget
  [context {:as budget :budget/keys [items]}]
  (let [imported (-> budget
                     (prepare-budget context)
                     (dissoc :budget/items)
                     models/put)]
    (->> items
         (prepare-budget-items imported context)
         models/put-many)
    (log/infof "[import] imported budget %s" (:budget/name imported))
    context))

(defmethod import-record* :price
  [ctx price]
  (-> price
      (select-keys [:price/trade-date
                    :price/value])
      (assoc :price/commodity (find-commodity ctx price))
      (validate ::models/price)
      models/put)
  ctx)

(defmethod import-record* :commodity
  [{:keys [entity] :as context} commodity]
  (with-fatal-exceptions "commodity"
    (let [{:commodity/keys [exchange symbol]
           :as created} (-> commodity
                            (assoc :commodity/entity entity
                                   :commodity/price-config {:price-config/enabled true}) ; TODO: read this from import source
                            purge-import-keys
                            (validate ::models/commodity)
                            models/put)]
      (log/infof "[import] imported commodity %s (%s)"
                 (:commodity/name created)
                 symbol)
      (-> context
          (update-in [:commodities] (fnil conj []) created)
          (update-in [:commodities-by-symbol] (fnil assoc {}) symbol created)
          (update-in [:commodities-by-exchange-and-symbol] (fnil assoc {}) [exchange symbol] created)))))

(defn- ignore?
  [{:import/keys [ignore?]}]
  ignore?)

(defn- handle-ex
  [e record context]
  (let [data (ex-data e)
        msg {:import/record-type :notification
             :notification/id (uuid)
             :notification/severity (if (::fatal? data) :fatal :error)
             :notification/message (format "An error occurred while trying to save record of type \"%s\": %s"
                                           (name (:import/record-type record))
                                           (ex-message e))
             :notification/data {:record record
                                 :ex-data data}}]
    (log/errorf e "[import] errors saving record %s: %s"
                (pr-str record)
                (pr-str data))
    [(update-in context
                [:notifications]
                conj
                msg)
     msg]))

(defn- import-record
  [xf]
  (fn
    ([] (xf))
    ([context] (xf context))
    ([context record]
     (if (ignore? record)
       (xf context record)
       (try
         (xf (import-record* context record)
             record)
         (catch Exception e
           (apply xf (handle-ex e record context))))))))

(defn- fetch-reconciled-items
  [{:reconciliation/keys [account]
    :keys [id]}
   {:keys [entity]}]
  (let [accounts (models/select
                   (util/model-type
                     (select-keys account [:id])
                     :account)
                   {:include-children? true})]
    (models/select
      (assoc
        (->>criteria
          {:earliest-date (get-in entity [:entity/transaction-date-range 0])
           :latest-date (get-in entity [:entity/transaction-date-range 1])}
          accounts)
        :transaction-item/reconciliation {:id id}))))

(defn- process-reconciliation
  [recon {:as ctx :keys [accounts]}]
  (let [balance (->> (fetch-reconciled-items recon ctx)
                     (map (comp :transaction-item/polarized-quantity
                                polarize-item-quantity
                                #(update-in % [:transaction-item/account] (comp accounts :id))))
                     (reduce + 0M))]
    (-> recon
        (assoc :reconciliation/balance balance
               :reconciliation/status :completed)
        models/put)))

(defn- notify-reconciliation-finalization
  [out-chan]
  (if out-chan
    (fn [recon]
      (a/go
        (a/>! out-chan {:import/record-type :finalize-reconciliation}))
      recon)
    identity))

(defn- process-reconciliations
  [{:keys [entity] :as ctx} out-chan]
  (let [reconciliations (models/select
                          (util/model-type
                            {:account/entity entity}
                            :reconciliation))
        ch (a/promise-chan)]
    (a/go
      (when out-chan
        (a/>! out-chan {:declaration/record-type :finalize-reconciliation
                        :declaration/record-count (count reconciliations)
                        :import/record-type :declaration}))
      (mapv (comp (notify-reconciliation-finalization out-chan)
                  #(process-reconciliation % ctx))
            reconciliations)
      (a/close! ch))
    ch))

(defn- forward
  "Returns a transducer that places received values on the specified channel"
  [c]
  (fn [xf]
    (completing
      (if c
        (fn
          [acc v]
          (a/go (a/>! c v))
          (xf acc v))
        (fn [acc v]
          (xf acc v))))))

(defn progress-xf
  [tracker-or-import-or-id]
  (let [tracker (cond
                  (satisfies? prog/Tracker tracker-or-import-or-id)
                  tracker-or-import-or-id

                  (map? tracker-or-import-or-id)
                  (prog/tracker (:id tracker-or-import-or-id))

                  :else
                  (prog/tracker tracker-or-import-or-id))]
    (comp (remove (fn [{:import/keys [record-type]}]
                    (= :reconciliation record-type)))
          (map (fn [{:as r :import/keys [record-type]}]
                 (case record-type
                   :declaration
                   (prog/expect tracker
                                (:declaration/record-type r)
                                (:declaration/record-count r))

                   :notification
                   (if (= :fatal (:notification/severity r))
                     (prog/fail tracker (:notification/message r))
                     (prog/warn tracker (:notification/message r)))

                   :termination-signal
                   (prog/finish tracker)

                   (prog/increment tracker record-type))
                 r)))))

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
            :ignore (xf acc (assoc record :import/ignore? true))))))))

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
  [import-spec {:keys [out-chan]}]
  (let [user (models/find (:import/user import-spec) :user)
        images (map (models/find :image)
                    (:import/images import-spec))
        source-type (get-source-type (first images))
        entity ((some-fn models/find-by models/put)
                {:entity/user user
                 :entity/name (:import/entity-name import-spec)})
        wait-chan (a/promise-chan)]
    (log/infof "[import] Import process starting for \"%s\"."
               (:import/entity-name import-spec))
    (a/go
      (try
        (let [result (->> images
                          (map image-content)
                          (read-source source-type)
                          (a/transduce
                            (comp (filter-import)
                                  import-record
                                  (forward out-chan))
                            (completing
                              (fn [acc {:import/keys [record-type]
                                        :notification/keys [severity]}]
                                (if (and (= :notification record-type)
                                         (= :fatal severity))
                                  (reduced (assoc acc ::abend? true))
                                  acc)))
                            {:import import-spec
                             :account-ids {}
                             :account-children {}
                             :account-parents {}
                             :notifications []
                             :entity entity})
                          a/<!!)]
          (models/put (:entity result)) ; transaction-date-range has been updated
          (when-not (::abend? result)
            (log/debugf "[import] data imported, start reconciliations for %s"
                        (:import/entity-name import-spec))
            (a/alts!! [(process-reconciliations result
                                                out-chan)
                       (a/timeout 5000)]))
          (when out-chan
            (a/go
              (a/>! out-chan {:import/record-type :termination-signal})))
          (a/>! wait-chan (select-keys result [:notifications :entity])))
        (finally
          (log/infof "[import] Import process has ended for \"%s\"."
                     (:import/entity-name import-spec))
          (a/close! wait-chan))))
    {:entity entity
     :wait-chan wait-chan}))

(defn import-data
  "Reads the contents from the specified input and saves
  the information using the specified storage. If an entity
  with the specified name is found, it is used, otherwise it
  is created"
  [import-spec & {:as opts :keys [atomic?]}]
  (if atomic?
    (throw (UnsupportedOperationException. "Atomic imports are not supported"))
    (import-data* import-spec opts)))
