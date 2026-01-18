(ns clj-money.db.datomic.transactions
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.db.datomic :as datomic]
            [clj-money.entities :as entities]
            [clj-money.util :as util]))

(defn- prepare-item
  [{:keys [id] :as item}]
  (let [id (or id (util/temp-id))]
    (-> item
        (assoc :id id)
        (update-in [:transaction-item/debit-item]
                   #(assoc %
                           :account-item/transaction-item
                           id))
        (update-in [:transaction-item/credit-item]
                   #(assoc %
                           :account-item/transaction-item
                           id)))))

(defmethod datomic/before-save :transaction
  [trx]
  (update-in trx
             [:transaction/items]
             #(map prepare-item %)))

(defn- removed
  [before after]
  (let [after-ids (->> after
                       (map :id)
                       set)]
    (remove (comp after-ids :id)
            before)))

(defn- item-redactions
  [trx before]
  (mapcat (fn [item]
            [[:db/retractEntity (:id item)]
             [:db/retractEntity (-> item
                                    :transaction-item/debit-item
                                    :id)]
             [:db/retractEntity (-> item
                                    :transaction-item/credit-item
                                    :id)]])
          (removed (:transaction/items before)
                   (:transaction/items trx))))

(defmethod datomic/deconstruct :transaction
  [trx]
  (if-let [before (::entities/before (meta trx))]
    (cons trx
          (item-redactions trx before))
    [trx]))
