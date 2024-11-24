(ns clj-money.db.sql.scheduled-transactions
  (:require [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.util :as util]
            [clj-money.db :as db]
            [clj-money.db.sql :as sql]
            [clj-money.db.sql.types :refer [temp-id]]))

(defmethod sql/before-save :scheduled-transaction
  [trx]
  (-> trx
      (update-in [:scheduled-transaction/interval-type] name)
      (update-in [:scheduled-transaction/date-spec] sql/->json)))

(defmethod sql/deconstruct :scheduled-transaction
  [{:scheduled-transaction/keys [items] :as trx}]
  (let [trx-id (or (:id trx)
                   (temp-id))]
    (cons (-> trx
              (dissoc :scheduled-transaction/items)
              (assoc :id trx-id))
        (map #(assoc % :scheduled-transaction-item/scheduled-transaction-id trx-id)
             items))))

(defmethod sql/after-read :scheduled-transaction
  [trx]
  (-> trx
      (update-in [:scheduled-transaction/interval-type] keyword)
      (update-in [:scheduled-transaction/date-spec] sql/json->map)
      (update-in-if [:scheduled-transaction/start-date] t/local-date)
      (update-in-if [:scheduled-transaction/end-date] t/local-date)
      (update-in-if [:scheduled-transaction/date-spec :day] #(if (string? %)
                                                               (keyword %)
                                                               %))))

(defmethod sql/reconstruct :scheduled-transaction
  [models]
  ; TODO: should we remove :scheduled-transaction-item/scheduled-transaction to be consistent with Datomic?
  (util/reconstruct {:parent? :scheduled-transaction/description
                     :child? :scheduled-transaction-item/action
                     :children-key :scheduled-transaction/items}
                    models))

(defmethod sql/post-select :scheduled-transaction
  [storage trxs]
  (map #(assoc %
               :scheduled-transaction/items
               (vec (db/select storage {:scheduled-transaction-item/scheduled-transaction %} {})))
       trxs))
