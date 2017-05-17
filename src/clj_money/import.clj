(ns clj-money.import
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.util :refer [pprint-and-return-l]]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.helpers :refer [with-transacted-storage]]))

(defmulti read-source
  (fn [source-type _ _]
    source-type))

(deftype Callback [account transaction])

(defn- import-account
  [context account]
  (let [original-id (:id account)
        original-parent-id (:parent-id account)
        parent-id (get-in context [:accounts original-parent-id])
        to-create (cond-> account
                    true
                    (assoc :entity-id (:entity-id context))

                    true
                    (dissoc :id)

                    parent-id
                    (assoc :parent-id parent-id))
        result (accounts/create (:storage context) to-create)]
    (update-in context [:accounts] #(assoc % original-id (:id result)))))

(defn- import-transaction
  [context transaction]
  (let [to-create (-> transaction
                      (update-in [:items] (fn [items]
                                            (map (fn [item]
                                                   (update-in item [:account-id] #(-> context :accounts (get %))))
                                                 items)))
                      (assoc :entity-id (:entity-id context)))]
    (transactions/create (:storage context) to-create)
    context)) ; Update anything in the context? don't want to include all transactions, as that can be many

(defn import-data
  "Reads the contents from the specified input and saves
  the information using the specified storage. If an entity
  with the specified name is found, it is used, otherwise it
  is created"
  [storage-spec user entity-name input source-type]
  (with-transacted-storage [s storage-spec]
    (let [entity (entities/find-or-create s user  entity-name)
          context (atom {:storage s
                         :accounts {}
                         :entity-id (:id entity)})]
      (read-source source-type input (->Callback (fn [account] (swap! context #(import-account % account)))
                                                 (fn [transaction] (swap! context #(import-transaction % transaction))))))))
