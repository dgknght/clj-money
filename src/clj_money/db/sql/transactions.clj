(ns clj-money.db.sql.transactions
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-money.db :as db]
            [clj-money.db.sql :as sql]))

(defmethod sql/deconstruct :transaction
  [{:transaction/keys [items lot-items transaction-date] :keys [id] :as trx}]
  (cons (dissoc trx
                :transaction/items
                :transaction/lot-items)
        (concat (map #(assoc %
                             :transaction-item/transaction-id id
                             :transaction-item/transaction-date transaction-date)
                     items)
                (map #(assoc %
                             :lot-item/transaction-id id
                             :lot-item/transaction-date transaction-date)
                     lot-items))))

(defmethod sql/after-read :transaction
  [trx]
  (update-in trx [:transaction/transaction-date] t/local-date))

(defn- ->item-criteria
  [trxs]
  {:transaction-item/transaction-id
   (if (= 1 (count trxs))
     (:id (first trxs))
     [:in (mapv :id trxs)])})

(defmethod sql/post-select :transaction
  [{:keys [storage]} trxs]
  (let [items (->> (db/select storage (->item-criteria trxs) {})
                   (group-by (comp :id :transaction-item/transaction)))]
    (map #(assoc % :transaction/items (items (:id %)))
         trxs)))
