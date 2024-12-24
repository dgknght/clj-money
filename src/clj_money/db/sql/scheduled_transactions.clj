(ns clj-money.db.sql.scheduled-transactions
  (:require [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db :as db]
            [clj-money.db.sql :as sql]
            [clj-money.db.sql.types :refer [->json json->map]]))

(defmethod sql/before-save :scheduled-transaction
  [trx]
  (-> trx
      (update-in [:scheduled-transaction/interval-type] name)
      (update-in [:scheduled-transaction/date-spec] ->json)))

(defmethod sql/deconstruct :scheduled-transaction
  [{:scheduled-transaction/keys [items] :keys [id] :as trx}]
  (cons (dissoc trx :scheduled-transaction/items)
        (map #(assoc % :scheduled-transaction-item/scheduled-transaction-id id)
             items)))

(defmethod sql/after-read :scheduled-transaction
  [trx]
  (-> trx
      (update-in [:scheduled-transaction/interval-type] keyword)
      (update-in [:scheduled-transaction/date-spec] json->map)
      (update-in-if [:scheduled-transaction/start-date] t/local-date)
      (update-in-if [:scheduled-transaction/end-date] t/local-date)
      (update-in-if [:scheduled-transaction/date-spec :day] #(if (string? %)
                                                               (keyword %)
                                                               %))))

(defmethod sql/post-select :scheduled-transaction
  [storage trxs]
  (map #(assoc %
               :scheduled-transaction/items
               (vec (db/select storage {:scheduled-transaction-item/scheduled-transaction %} {})))
       trxs))
