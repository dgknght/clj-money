(ns clj-money.api.transactions
  (:refer-clojure :exclude [update get count])
  (:require [cljs.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.comparatives :as comparatives]
            [clj-money.state :refer [current-entity]]
            [clj-money.dates :refer [serialize-local-date local-date?]]
            [clj-money.entities.schema :as schema]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn- serialize-date
  [x]
  (cond
    (vector? x) (mapv serialize-date x)
    (local-date? x) (serialize-local-date x)
    :else x))

(defn- ->query
  [criteria]
  (-> criteria
      (update-in-if [:transaction/transaction-date] serialize-date)
      (update-in-if [:transaction/created-at] serialize-date)
      (update-in-if [:transaction-item/account] :id)
      (update-in-if [:transaction/review-status] name)
      (rename-keys {:transaction/transaction-date :transaction-date
                    :transaction/created-at :created-at
                    :transaction/description :description
                    :transaction/review-status :review-status
                    :transaction-item/quantity :quantity
                    :transaction-item/account :account-id})
      comparatives/nominalize))

(defn select
  [criteria & {:as opts}]
  (api/get
    (api/path :entities
              @current-entity
              :transactions)
    (->query criteria)
    (add-error-handler opts "Unable to retrieve the transactions: %s")))

(defn count
  "Retrieves the number of transactions matching the criteria, as {:count n}"
  [criteria & {:as opts}]
  (api/get
    (api/path :entities
              @current-entity
              :transactions
              :count)
    (->query criteria)
    (add-error-handler opts "Unable to retrieve the transaction count: %s")))

(defn get
  [{:keys [id]} & {:as opts}]
  (api/get (api/path :transactions id)
           {}
           (add-error-handler opts "Unable to retrieve the transaction: %s")))

(defn create
  [transaction opts]
  (api/post (api/path :entities
                      @current-entity
                      :transactions)
            (dissoc transaction :transaction/entity)
            (add-error-handler opts "Unable to create the transaction: %s")))

(defn update
  [transaction opts]
  (api/patch (api/path :transactions (:id transaction))
             (dissoc transaction :transaction/entity)
             (add-error-handler opts "Unable to update the transaction: %s")))

(defn save
  [transaction & {:as opts}]
  (let [f (if (:id transaction) update create)]
    (-> transaction
        (schema/prune :transaction)
        (f opts))))

(defn delete
  [{:keys [id]} & {:as opts}]
  (api/delete (api/path :transactions id)
              (add-error-handler opts "Unable to delete the transaction: %s")))

(defn get-by-transaction-item
  [{:keys [id]} & {:as opts}]
  (api/get (api/path :transactions)
           {:transaction-item-id id}
           (add-error-handler opts "Unable to retrieve the transaction: %s")))
