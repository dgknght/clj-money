(ns clj-money.db.sql.transactions
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-money.db :as db]
            [clj-money.db.sql :as sql]))

(defn- deleted-items
  [trx]
  (when-let [before (-> trx
                        meta
                        :clj-money.entities/original
                        :transaction/items
                        seq)]
    (let [current? (comp (set
                           (map :id
                                (:transaction/items trx)))
                         :id)]
      (->> before
           (remove current?)
           (map #(vector ::db/delete %))))))

(defmethod sql/deconstruct :transaction
  [{:transaction/keys [lot-items items] :keys [id] :as trx}]
  (cons (dissoc trx
                :transaction/items
                :transaction/lot-items)
        (concat (map #(assoc % :transaction-item/transaction-id id)
                     items)
                (map #(assoc % :lot-item/transaction-id id)
                     lot-items)
                (deleted-items trx))))

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
  (let [items (group-by (comp :id :transaction-item/transaction)
                        (db/select storage (->item-criteria trxs) {}))]
    (map #(assoc % :transaction/items (items (:id %)))
         trxs)))
