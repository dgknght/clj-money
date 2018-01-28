(ns clj-money.import
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clojure.java.io :as io]
            [clojure.core.async :refer [go >! >!!]]
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
            [clj-money.models.helpers :refer [with-transacted-storage]]))

(defmulti read-source
  (fn [source-type _]
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
                                     (map :amount))))))
  ; Update anything in the context?
  ; don't want to include all transactions,
  ; as that can be many
  context)

(defmethod ^:private import-transaction :buy
  [context transaction]
  (let [purchase {:commodity-id (->> context
                                     :commodities
                                     (filter #(and (= (:symbol %) (:symbol transaction))
                                                   (= (:exchange %) (:exchange transaction))))
                                     first
                                     :id)
                  :account-id ((:accounts context) (:account-id transaction))
                  :trade-date (:transaction-date transaction)
                  :shares (:shares transaction)
                  :value (:amount (first (:items transaction)))}]
    (trading/buy (:storage context) purchase))
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

(defn- notify-progress
  [{:keys [progress-chan progress] :as context}]
  ; this will block until the item is taken off the channel
  ; in the future we may want more flexibility than that, but
  ; for now this is ensuring that updates are received in the
  ; correct order
  (>!! progress-chan progress)
  context)

(defn- inc-and-notify-progress
  [context record-type]
  (-> context
      (update-in [:progress record-type :imported]
                 (fnil inc 0))
      notify-progress))

(defmulti import-record
  (fn [_ record]
    (-> record meta :record-type)))

(defmethod import-record :declaration
  [context {:keys [record-type record-count]}]
  (-> context
      (assoc-in [:progress record-type :total] record-count)
      notify-progress))

(defmethod import-record :account
  [context account]
  (import-account context account))

(defmethod import-record :transaction
  [context transaction]
  (import-transaction context transaction))

(defmethod import-record :budget
  [context budget]
  (import-budget context budget))

(defmethod import-record :price
  [context price]
  (import-price context price))

(defmethod import-record :commodity
  [context commodity]
  (import-commodity context commodity))

(def ^:private reportable-record-types
  #{:commodity :price :account :transaction :budget})

(defn- report-progress?
  [record-type]
  (reportable-record-types record-type))

(defn process-record
  "Top-level callback processing

  This function calls the multimethod import-record
  to dispatch to the correct import logic for the
  record-type.

  If the record is nil, processing is skipped but
  the progress is updated."
  [context record]
  (let [{:keys [record-type ignore?]} (meta record)]
    (cond-> context
      (not ignore?)
      (import-record record)

      (report-progress? record-type)
      (inc-and-notify-progress record-type))))

(defn import-data
  "Reads the contents from the specified input and saves
  the information using the specified storage. If an entity
  with the specified name is found, it is used, otherwise it
  is created"
  [storage-spec import-spec progress-chan]
  (with-transacted-storage [s storage-spec]
    (let [user (users/find-by-id s (:user-id import-spec))
          [inputs source-type] (prepare-input s (:image-ids import-spec))
          entity (entities/find-or-create s
                                          user
                                          (:entity-name import-spec))
          result (transactions/with-delayed-balancing s (:id entity)
                   (->> inputs
                        (mapcat #(read-source source-type %))
                        (reduce process-record
                                {:storage s
                                 :import import-spec
                                 :progress-chan progress-chan
                                 :progress {}
                                 :accounts {}
                                 :entity entity})))]
      entity)))
