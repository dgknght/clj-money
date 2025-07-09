(ns clj-money.db.datomic.reconciliations
  (:require [clj-money.db.datomic :as datomic]))

(defmethod datomic/before-save :reconciliation
  [{:as recon :reconciliation/keys [item-refs]}]
  (-> recon
      (dissoc :reconciliation/item-refs)
      (assoc :reconciliation/items
             (mapv first item-refs))))
