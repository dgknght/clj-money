(ns clj-money.db.sql.scheduled-transactions
  (:require [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.util :as util]
            [clj-money.db :as db]
            [clj-money.db.sql :as sql]))

(defmethod sql/deconstruct :scheduled-transaction
  [{:scheduled-transaction/keys [items] :keys [id] :as trx}]
  (cons (dissoc trx :scheduled-transaction/items)
        (map #(assoc % :scheduled-transaction-item/scheduled-transaction-id id)
             items)))

(defn- ->keyword
  [x]
  (if (string? x)
    (keyword x)
    x))

(defmethod sql/after-read :scheduled-transaction
  [trx]
  (-> trx
      (update-in [:scheduled-transaction/interval-type] keyword)
      (update-in-if [:scheduled-transaction/start-date] t/local-date)
      (update-in-if [:scheduled-transaction/end-date] t/local-date)
      (update-in-if [:scheduled-transaction/date-spec :day] ->keyword)
      (update-in-if [:scheduled-transaction/date-spec :days] #(mapv ->keyword %))))

(defmethod sql/post-select :scheduled-transaction
  [storage trxs]
  (map #(assoc %
               :scheduled-transaction/items
               (vec (db/select storage
                               {:scheduled-transaction-item/scheduled-transaction (util/->model-ref %)}
                               {})))
       trxs))
