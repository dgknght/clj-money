(ns clj-money.db.sql.transactions
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [index-by]]
            [clj-money.util :as util]
            [clj-money.transactions :as trxs]
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
           (mapcat (juxt identity
                         :transaction-item/debit-item
                         :transaction-item/credit-item))
           (map #(vector ::db/delete %))))))

(defn- ensure-id
  [entity]
  (update-in entity [:id] #(or % (random-uuid))))

(defn- deconstruct-item
  [trx-id]
  (fn [{:as item
        :keys [id]
        :transaction-item/keys [credit-item debit-item]}]
    [(assoc credit-item :account-item/transaction-item-id id)
     (assoc debit-item :account-item/transaction-item-id id)
     (-> item
         (assoc :transaction-item/transaction-id trx-id
                :transaction-item/credit-item-id (:id credit-item)
                :transaction-item/debit-item-id (:id debit-item))
         (dissoc :transaction-item/credit-item
                 :transaction-item/debit-item))]))

(defmethod sql/deconstruct :transaction
  [{:transaction/keys [lot-items items] :keys [id] :as trx}]
  (cons (dissoc trx
                :transaction/items
                :transaction/lot-items)
        (concat (->> items
                     (map #(-> %
                               ensure-id
                               (update-in [:transaction-item/credit-item]
                                          ensure-id)
                               (update-in [:transaction-item/debit-item]
                                          ensure-id)))
                     (mapcat (deconstruct-item id)))
                (map #(assoc % :lot-item/transaction-id id)
                     lot-items)
                (deleted-items trx))))

(defmethod sql/deconstruct-for-delete :transaction
  [{:transaction/keys [items] :as trx}]
  (cons trx
        (mapcat trxs/account-items items)))

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
  (let [trx-items (db/select storage (->item-criteria trxs) {})
        act-items (->> (db/select storage
                                  (util/entity-type
                                    {:id [:in (->> trx-items
                                                   (mapcat trxs/account-items)
                                                   (mapv :id))]}
                                    :account-item)
                                  {})
                       (map #(update-in % [:account-item/action] keyword))
                       (index-by :id))
        items-with-acts (->> trx-items
                             (map #(-> %
                                       (update-in
                                         [:transaction-item/credit-item]
                                         (comp act-items :id))
                                       (update-in
                                         [:transaction-item/debit-item]
                                         (comp act-items :id))))
                             (group-by (comp :id :transaction-item/transaction)))]
    (map (fn [trx]
           (-> trx
               (assoc :transaction/items (items-with-acts (:id trx)))))
         trxs)))
