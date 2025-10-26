(ns clj-money.api.transactions
  (:refer-clojure :exclude [update get])
  (:require [cljs.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clj-money.comparatives :as comparatives]
            [clj-money.state :refer [current-entity]]
            [clj-money.dates :refer [serialize-local-date local-date?]]
            [clj-money.entities.schema :as schema]
            [clj-money.api :as api :refer [add-error-handler]]))

(def ^:private working-date
  (some-fn :transaction/original-transaction-date
           :transaction/transaction-date))

(defn- transaction-path
  [{:keys [id] :as transaction}]
  (api/path :transactions
            (serialize-local-date (working-date transaction))
            id))

(defn- serialize-date
  [x]
  (cond
    (vector? x) (mapv serialize-date x)
    (local-date? x) (serialize-local-date x)
    :else x))

(defn select
  [criteria & {:as opts}]
  {:pre [(:transaction/transaction-date criteria)]}
  (api/get
    (api/path :entities
              @current-entity
              :transactions)
    (-> criteria
        (update-in [:transaction/transaction-date]
                   serialize-date)
        (rename-keys {:transaction/transaction-date :transaction-date})
        comparatives/nominalize)
    (add-error-handler opts "Unable to retrieve the transactions: %s")))

(defn create
  [transaction opts]
  (api/post (api/path :entities
                      @current-entity
                      :transactions)
            (dissoc transaction :transaction/entity)
            (add-error-handler opts "Unable to create the transaction: %s")))

(defn update
  [transaction opts]
  (api/patch (transaction-path transaction)
             (dissoc transaction :transaction/entity)
             (add-error-handler opts "Unable to update the transaction: %s")))

(defn save
  [transaction & {:as opts}]
  (let [f (if (:id transaction) update create)]
    (-> transaction
        (schema/prune :transaction)
        (f opts))))

(defn get
  [tkey & {:as opts}]
  (api/get (transaction-path tkey)
           {}
           (add-error-handler opts "Unable to retrieve the transaction: %s")))

(defn delete
  [transaction & {:as opts}]
  (api/delete (transaction-path transaction)
              (add-error-handler opts "Unable to remove the transaction: %s")))
