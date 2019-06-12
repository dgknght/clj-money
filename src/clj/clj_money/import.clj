(ns clj-money.import
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clojure.java.io :as io]
            [clojure.core.async :refer [<!! >!! chan go] :as async]
            [clj-money.util :refer [pprint-and-return
                                    pprint-and-return-l]]
            [clj-money.validation :as validation]
            [clj-money.trading :as trading]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.images :as images]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]
            [clj-money.models.imports :as imports]
            [clj-money.models.helpers :refer [with-storage
                                              with-transacted-storage]]))

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
  [context {:keys [exchange symbol]}]
  (->> context
       :commodities
       (filter #(and (= (:symbol %) symbol)
                     (or (= :iso4217 exchange)
                         (= (:exchange %) exchange))))
       first))

(defn- import-account
  [context account]
  (let [original-id (:id account)
        original-parent-id (:parent-id account)
        parent-id (get-in context [:accounts original-parent-id])
        commodity (find-commodity context (:commodity account))
        to-create (cond-> account
                    true
                    (assoc :entity-id (-> context :entity :id)
                           :commodity-id (:id commodity))

                    true
                    (dissoc :id :commodity)

                    parent-id
                    (assoc :parent-id parent-id))
        result (accounts/create (:storage context) to-create)]
    (when-not (:id result)
      (throw (ex-info (str
                        "Unable to create the account "
                        (validation/error-messages result))
                      {:result result})))
    (log/info (format "imported account \"%s\"" (:name result)))
    (update-in context [:accounts] #(assoc % original-id (:id result)))))

(defn- import-budget
  [{:keys [storage accounts] :as context} budget]
  (let [result (budgets/create
                 storage
                 (-> budget
                     (dissoc :items)
                     (assoc :entity-id (-> context :entity :id))))]
    (doseq [item (:items budget)]
      (budgets/create-item storage
                           (-> item
                               (assoc :budget-id (:id result))
                               (update-in [:periods] #(->> %
                                                           (sort-by :index)
                                                           (map :amount)))
                               (update-in [:account-id] #(get accounts %)))))
    (log/info (format "imported budget \"%s\"" (:name result))))
  context)

(defn- import-commodity
  [{:keys [entity storage] :as context} commodity]
  (let [to-create (assoc commodity :entity-id (:id entity))
        created (commodities/create storage to-create)]
    (update-in context [:commodities] #((fnil conj []) % created))))

(defn- resolve-account-references
  [context items]
  (map (fn [item]
         (update-in item [:account-id] #(-> context :accounts (get %))))
       items))

(defn- prepare-transaction
  [context transaction]
  (-> transaction
      (update-in [:items] #(resolve-account-references context %))
      (assoc :entity-id (-> context :entity :id))))

(defmulti ^:private import-transaction
  (fn [_ transaction]
    (:action transaction)))

(defmethod ^:private import-transaction :default
  [context transaction]
  (let [result (->> transaction
                    (prepare-transaction context)
                    (transactions/create (:storage context)))]
    (log/info (format "imported transaction on %s at %s for %s"
                      (:transaction-date result)
                      (:description result)
                      (reduce + (->> (:items result)
                                     (filter #(= :debit (:action %)))
                                     (map :quantity))))))
  ; Update anything in the context?
  ; don't want to include all transactions,
  ; as that can be many
  context)

(defmethod ^:private import-transaction :buy
  [{:keys [accounts storage] :as context} transaction]
  (let [purchase {:commodity-id (->> context
                                     :commodities
                                     (filter #(and (= (:symbol %) (:symbol transaction))
                                                   (= (:exchange %) (:exchange transaction))))
                                     first
                                     :id)
                  :commodity-account-id (accounts (:commodity-account-id transaction))
                  :account-id (accounts (:account-id transaction))
                  :trade-date (:transaction-date transaction)
                  :shares (:shares transaction)
                  :value (or (:value transaction)
                             (:quantity (first (:items transaction))))}]
    (trading/buy storage purchase))
  context)

(defmethod ^:private import-transaction :sell
  [{:keys [accounts storage] :as context} transaction]
  (let [sale {:commodity-id (->> context
                                 :commodities
                                 (filter #(and (= (:symbol %) (:symbol transaction))
                                               (= (:exchange %) (:exchange transaction))))
                                 first
                                 :id)
              :commodity-account-id (accounts (:commodity-account-id transaction))
              :account-id (accounts (:account-id transaction))
              :trade-date (:transaction-date transaction)
              :shares (:shares transaction)
              :value (or (:value transaction)
                         (:quantity (first (:items transaction))))}]
    (trading/sell storage sale))
  context)

(defmethod ^:private import-transaction :transfer
  [{:keys [storage accounts] :as context}
   {:keys [from-account-id to-account-id] :as transaction}]
  (let [from-commodity-account (accounts/find-by-id storage (accounts from-account-id))
        from-account (accounts/find-by-id storage (:parent-id from-commodity-account))
        to-commodity-account (accounts/find-by-id storage (accounts to-account-id))]
    (trading/transfer (:storage context)
                      (assoc transaction
                             :transfer-date (:transaction-date transaction)
                             :commodity-id (:commodity-id from-commodity-account)
                             :from-account-id (:id from-account)
                             :to-account-id (:parent-id to-commodity-account))))
  context)

; This is maybe too specific to GnuCash. It would be better if the
; gnucash namespace did these lookups
(defn- ensure-split-ids
  [{:keys [commodity-account-id commodity-id account-id] :as transaction}
   {:keys [accounts storage] :as context}]
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
                   (dissoc :items))]
    (trading/split (:storage context) split))
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
  (let [images (map #(images/find-by-id storage %) image-ids)
        source-type (-> images first get-source-type)]
    [(map #(io/input-stream (byte-array (:body %))) images)
     source-type]))

(def ^:private reportable-record-types
  #{:commodity :price :account :transaction :budget})

(defn- report-progress?
  [record]
  (reportable-record-types (-> record meta :record-type)))

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

(defmethod import-record* :transaction
  [context transaction]
  (import-transaction context transaction))

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
       (xf (import-record* context record) record)))))

(defn- notify-progress
  ([progress-chan context] context)
  ([progress-chan context _]
   (>!! progress-chan (:progress context))
   context))

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
                                     (partial notify-progress progress-chan)
                                     {:storage s
                                      :import import-spec
                                      :progress {}
                                      :accounts {}
                                      :entity entity}
                                     out-chan)]
    (go
      (transactions/with-delayed-balancing s (:id entity)
        (read-source source-type inputs out-chan)
        (>!! progress-chan (-> (<!! result-chan)
                               :progress
                               (assoc :finished true))))
      (deliver wait-promise true))
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
