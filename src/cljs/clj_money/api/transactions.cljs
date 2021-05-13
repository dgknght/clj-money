(ns clj-money.api.transactions
  (:refer-clojure :exclude [update get])
  (:require [cljs-time.core :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.web :refer [serialize-date
                                         unserialize-date
                                         unserialize-date-time]]
            [clj-money.state :refer [current-entity]]
            [dgknght.app-lib.api :as api]
            [clj-money.api.transaction-items :as items]))

(defn after-read
  [{:keys [transaction-date] :as transaction}]
  (let [transaction-date (unserialize-date transaction-date)]
    (-> transaction
        (assoc :original-transaction-date transaction-date
               :transaction-date transaction-date)
        (update-in [:created-at] unserialize-date-time)
        (update-in [:items] #(map items/after-read %)))))

(def ^:private working-date
  (some-fn :original-transaction-date
           :transaction-date))

(defn- transaction-path
  [{:keys [id] :as transaction}]
  (api/path :transactions
            (serialize-date (working-date transaction))
            id))

(defn search
  [criteria success-fn error-fn]
  (let [end-date (get-in criteria [:end-date] (t/today))
        start-date (get-in criteria [:start-date] (t/minus end-date (t/months 6)))]
    (api/get
      (api/path :entities
                (:id @current-entity)
                (serialize-date start-date)
                (serialize-date end-date)
                :transactions)
      (dissoc criteria :start-date :end-date)
      #(success-fn (map after-read %))
      error-fn)))

(defn- serialize
  [transaction]
  (-> transaction
      (update-in-if [:original-transaction-date] serialize-date)
      (update-in [:transaction-date] serialize-date)))

(defn create
  [transaction success-fn error-fn]
  (api/post (api/path :entities
                      (:entity-id transaction)
                      :transactions)
            (serialize transaction)
            (comp success-fn after-read)
            error-fn))

(defn update
  [transaction success-fn error-fn]
  (api/patch (transaction-path transaction)
             (serialize transaction)
             (comp success-fn after-read)
             error-fn))

(defn save
  [transaction success-fn error-fn]
  (if (:id transaction)
    (update transaction success-fn error-fn)
    (create transaction success-fn error-fn)))

(defn get
  [tkey success-fn error-fn]
  (api/get (transaction-path tkey)
           (comp success-fn after-read)
           error-fn))

(defn delete
  [transaction success-fn error-fn]
  (api/delete (transaction-path transaction)
              success-fn
              error-fn))
