(ns clj-money.import
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.util :refer [pprint-and-return
                                    pprint-and-return-l]]
            [clj-money.validation :as validation]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.helpers :refer [with-transacted-storage]]))

(defmulti read-source
  (fn [source-type _ _]
    source-type))

(deftype Callback [account budget transaction])

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
    (update-in context [:accounts] #(assoc % original-id (:id result)))))

(defn- import-budget
  [context budget]
  (budgets/create (:storage context)
                  (-> budget
                      (dissoc :items)
                      (assoc :entity-id (-> context :entity :id))))
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

(defn- import-transaction
  [context transaction]
  (->> transaction
       (prepare-transaction context)
       (transactions/create (:storage context)))
  ; Update anything in the context?
  ; don't want to include all transactions,
  ; as that can be many
  context)

(defn import-data
  "Reads the contents from the specified input and saves
  the information using the specified storage. If an entity
  with the specified name is found, it is used, otherwise it
  is created"
  [storage-spec user entity-name input source-type]
  (with-transacted-storage [s storage-spec]
    (let [context (atom {:storage s
                         :accounts {}
                         :entity (entities/find-or-create s user entity-name)})
          callback (->Callback (fn [account]
                                 (swap! context #(import-account % account)))
                               (fn [budget]
                                 (swap! context #(import-budget % budget)))
                               (fn [transaction]
                                 (swap! context #(import-transaction % transaction))))]
      (read-source source-type input callback))))
