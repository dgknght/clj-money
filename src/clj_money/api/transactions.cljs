(ns clj-money.api.transactions
  (:refer-clojure :exclude [update get])
  (:require [cljs.pprint :refer [pprint]]
            [cljs-time.core :as t]
            [clj-money.state :refer [current-entity]]
            [clj-money.dates :refer [serialize-local-date]]
            [clj-money.models :refer [prune]]
            [clj-money.api :as api :refer [add-error-handler]]))


(def ^:private working-date
  (some-fn :transaction/original-transaction-date
           :transaction/transaction-date))

(defn- transaction-path
  [{:keys [id] :as transaction}]
  (api/path :transactions
            (serialize-local-date (working-date transaction))
            id))

(defn search
  [criteria & {:as opts}]
  (let [end-date (get-in criteria [:end-date] (t/today))
        start-date (get-in criteria [:start-date] (t/minus end-date (t/months 6)))]
    (api/get
      (api/path :entities
                @current-entity
                (serialize-local-date start-date)
                (serialize-local-date end-date)
                :transactions)
      (dissoc criteria :start-date :end-date)
      (add-error-handler opts "Unable to retrieve the transactions: %s"))))

(defn create
  [transaction opts]
  (api/post (api/path :entities
                      (:transaction/entity transaction)
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
        (prune :transaction)
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
