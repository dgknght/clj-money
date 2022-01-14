(ns clj-money.api.transactions
  (:refer-clojure :exclude [update get])
  (:require [cljs-time.core :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.web :refer [serialize-date
                                         unserialize-date
                                         unserialize-date-time]]
            [dgknght.app-lib.api-async :as api]
            [clj-money.state :refer [current-entity]]
            [clj-money.api.transaction-items :as items]
            [clj-money.api :refer [handle-ex]]))

(defn after-read
  [transaction]
  (-> transaction
      (update-in [:transaction-date] unserialize-date)
      (update-in [:original-transaction-date] unserialize-date)
      (update-in [:created-at] unserialize-date-time)
      (update-in [:items] #(map items/after-read %))))

(defn- transform
  [xf]
  (comp (api/apply-fn after-read)
        xf))

(def ^:private working-date
  (some-fn :original-transaction-date
           :transaction-date))

(defn- transaction-path
  [{:keys [id] :as transaction}]
  (api/path :transactions
            (serialize-date (working-date transaction))
            id))

(defn search
  [criteria xf]
  (let [end-date (get-in criteria [:end-date] (t/today))
        start-date (get-in criteria [:start-date] (t/minus end-date (t/months 6)))]
    (api/get
      (api/path :entities
                (:id @current-entity)
                (serialize-date start-date)
                (serialize-date end-date)
                :transactions)
      (dissoc criteria :start-date :end-date)
      {:transform (transform xf)
       :handle-ex (handle-ex "Unable to retrieve the transactions: %s")})))

(defn- serialize
  [transaction]
  (-> transaction
      (update-in-if [:original-transaction-date] serialize-date)
      (update-in [:transaction-date] serialize-date)))

(defn create
  [transaction xf]
  (api/post (api/path :entities
                      (:entity-id transaction)
                      :transactions)
            (serialize transaction)
            {:transform (transform xf)
             :handle-ex (handle-ex "Unable to create the transaction: %s")}))

(defn update
  [transaction xf]
  (api/patch (transaction-path transaction)
             (serialize transaction)
             {:transform (transform xf)
              :handle-ex (handle-ex "Unable to update the transaction: %s")}))

(defn save
  [transaction xf]
  (if (:id transaction)
    (update transaction xf)
    (create transaction xf)))

(defn get
  [tkey xf]
  (api/get (transaction-path tkey)
           {}
           {:transform (transform xf)
            :handle-ex (handle-ex "Unable to retrieve the transaction: %s")}))

(defn delete
  [transaction xf]
  (api/delete (transaction-path transaction)
              {:transform xf
               :handle-ex (handle-ex "Unable to remove the transaction: %s")}))
