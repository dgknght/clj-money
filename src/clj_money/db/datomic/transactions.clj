(ns clj-money.db.datomic.transactions
  (:require [clj-money.db.datomic :as datomic]
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
