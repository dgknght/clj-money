(ns clj-money.api.transactions
  (:refer-clojure :exclude [update])
  (:require [cljs-time.core :as t]
            [cljs-time.coerce :as tc]
            [clj-money.util :refer [update-in-if
                                    serialize-date
                                    unserialize-date
                                    unserialize-date-time]]
            [clj-money.state :refer [current-entity]]
            [clj-money.api :as api]
            [clj-money.api.transaction-items :as items]))

(defn- after-read
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
  (api/get-resources
    (api/path :entities
              (:id @current-entity)
              (serialize-date (-> 6 t/months t/ago tc/to-local-date))
              (serialize-date (-> 1 t/months t/from-now tc/to-local-date))
              :transactions)
    criteria
    #(success-fn (map after-read %))
    error-fn))

(defn- serialize
  [transaction]
  (-> transaction
      (update-in-if [:original-transaction-date] serialize-date)
      (update-in [:transaction-date] serialize-date)))

(defn create
  [transaction success-fn error-fn]
  (api/create-resource (api/path :entities
                                 (:entity-id transaction)
                                 :transactions)
                       (serialize transaction)
                       (comp success-fn after-read)
                       error-fn))

(defn update
  [transaction success-fn error-fn]
  (api/update-resource (transaction-path transaction)
                       (serialize transaction)
                       (comp success-fn after-read)
                       error-fn))

(defn save
  [transaction success-fn error-fn]
  (if (:id transaction)
    (update transaction success-fn error-fn)
    (create transaction success-fn error-fn)))

(defn get-one
  [tkey success-fn error-fn]
  (api/get-resources (transaction-path tkey)
                     (comp success-fn after-read)
                     error-fn))

(defn delete
  [transaction success-fn error-fn]
  (api/delete-resource (transaction-path transaction)
                       success-fn
                       error-fn))
