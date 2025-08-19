(ns clj-money.db.sql.transactions
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-money.util :as util]
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
  [[trx :as trxs]]
  (if (= 1 (count trxs))
    {:transaction-item/transaction-date (:transaction/transaction-date trx)
     :transaction-item/transaction-id (:id trx)}
    (let [[start end] (->> trxs
                           (map :transaction/transaction-date)
                           (util/->>range {:compare t/before?}))]
      {:transaction-item/transaction-date [:between start end]
       :transaction-item/transaction-id [:in (mapv :id trxs)]})))

(defmethod sql/post-select :transaction
  [{:keys [storage]} trxs]
  (let [items (->> (db/select storage (->item-criteria trxs) {})
                   (group-by (comp :id :transaction-item/transaction)))]
    (map #(assoc % :transaction/items (items (:id %)))
         trxs)))
