(ns clj-money.db.datomic.transactions
  (:require [clj-money.db.datomic :as datomic]
            [clj-money.util :as util]))

(defn- prepare-item
  [{:keys [id] :as item}]
  (let [id (or id (util/temp-id))]
    (-> item
        (assoc :id id)
        (update-in [:transaction-item/account-items]
                   (fn [items]
                     (map #(assoc %
                                  :account-item/transaction-item
                                  id)
                          items))))))

(defmethod datomic/before-save :transaction
  [trx]
  (update-in trx
             [:transaction/items]
             #(map prepare-item %)))
