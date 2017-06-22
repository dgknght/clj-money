(ns clj-money.import
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clojure.java.io :as io]
            [clojure.core.async :refer [go >!]]
            [clj-money.util :refer [pprint-and-return
                                    pprint-and-return-l]]
            [clj-money.validation :as validation]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.images :as images]
            [clj-money.models.imports :as imports]
            [clj-money.models.helpers :refer [with-transacted-storage]]))

(defmulti read-source
  (fn [source-type _ _]
    source-type))

(defn- import-account
  [context account]
  (let [original-id (:id account)
        original-parent-id (:parent-id account)
        parent-id (get-in context [:accounts original-parent-id])
        to-create (cond-> account
                    true
                    (assoc :entity-id (-> context :entity :id))

                    true
                    (dissoc :id)

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
  context)

(defn- prepare-input
  "Returns the input data and source type based
  on the specified image"
  [storage image-id]
  (let [image (images/find-by-id storage image-id)
        extension (re-find #"(?<=\.).*$" (:original-filename image))]
    [(io/input-stream (byte-array (:body image))) (keyword extension)]))

(defn- update-progress
  [{:keys [callback progress] :as context}]
  (if (fn? callback)
    (callback progress)
    (go (>! callback progress)))
  context)

(defn- inc-and-update-progress
  [context record-type]
  (-> context
      (update-in [:progress record-type :imported]
                 (fnil inc 0))
      update-progress))

(defmulti process-record
  (fn [_ _ record-type]
    record-type))

(defmethod process-record :declaration
  [context {:keys [record-type record-count]} _]
  (-> context
      (assoc-in [:progress record-type :total] record-count)
      update-progress))

(defmethod process-record :account
  [context account _]
  (-> context
      (import-account account)
      (inc-and-update-progress :account)))

(defmethod process-record :transaction
  [context transaction _]
  (-> context
      (import-transaction transaction)
      (inc-and-update-progress :transaction)))

(defmethod process-record :budget
  [context budget _]
  (-> context
      (import-budget budget)
      (inc-and-update-progress :budget)))

(defmethod process-record :price
  [context commodity _]
  context)

(defmethod process-record :commodity
  [context commodity _]
  context)

(defn process-callback
  "Top-level callback processing

  This function calls the multimethod process-record
  to dispatch to the correct import logic for the
  record-type.

  If the record is nil, processing is skipped but
  the progress is updated."
  [context record record-type]
  (swap! context #(if record
                    (process-record % record record-type)
                    (inc-and-update-progress % record-type))))

(defn import-data
  "Reads the contents from the specified input and saves
  the information using the specified storage. If an entity
  with the specified name is found, it is used, otherwise it
  is created"
  [storage-spec impt progress-callback]
  (with-transacted-storage [s storage-spec]
    (let [user (users/find-by-id s (:user-id impt))
          context  (atom {:storage s
                          :import impt
                          :callback progress-callback
                          :progress {}
                          :accounts {}
                          :entity (entities/find-or-create s
                                                           user
                                                           (:entity-name impt))})
          [input source-type] (prepare-input s (:image-id impt))]
      (transactions/with-delayed-balancing s (-> @context :entity :id)
        (read-source source-type input (partial process-callback context)))
      (:entity @context))))
