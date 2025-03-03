(ns clj-money.api.transactions
  (:refer-clojure :exclude [update get])
  (:require [cljs-time.core :as t]
            [dgknght.app-lib.web :refer [serialize-date]]
            [dgknght.app-lib.api-async :as api]
            [clj-money.state :refer [current-entity]]
            [clj-money.api :refer [add-error-handler]]))

(def ^:private working-date
  (some-fn :original-transaction-date
           :transaction-date))

(defn- transaction-path
  [{:keys [id] :as transaction}]
  (api/path :transactions
            (serialize-date (working-date transaction))
            id))

(defn search
  [criteria & {:as opts}]
  (let [end-date (get-in criteria [:end-date] (t/today))
        start-date (get-in criteria [:start-date] (t/minus end-date (t/months 6)))]
    (api/get
      (api/path :entities
                (:id @current-entity)
                (serialize-date start-date)
                (serialize-date end-date)
                :transactions)
      (dissoc criteria :start-date :end-date)
      (add-error-handler opts "Unable to retrieve the transactions: %s"))))

(defn create
  [transaction opts]
  (api/post (api/path :entities
                      (:entity-id transaction)
                      :transactions)
            transaction
            (add-error-handler opts "Unable to create the transaction: %s")))

(defn update
  [transaction opts]
  (api/patch (transaction-path transaction)
             transaction
             (add-error-handler opts "Unable to update the transaction: %s")))

(defn save
  [transaction & {:as opts}]
  (if (:id transaction)
    (update transaction opts)
    (create transaction opts)))

(defn get
  [tkey & {:as opts}]
  (api/get (transaction-path tkey)
           {}
           (add-error-handler opts "Unable to retrieve the transaction: %s")))

(defn delete
  [transaction & {:as opts}]
  (api/delete (transaction-path transaction)
              (add-error-handler opts "Unable to remove the transaction: %s")))
