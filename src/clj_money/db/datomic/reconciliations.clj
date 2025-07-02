(ns clj-money.db.datomic.reconciliations
  (:require [clj-money.dates :refer [->local-date]]
            [clj-money.db.datomic :as datomic]))

(defmethod datomic/before-save :reconciliation
  [{:as recon :reconciliation/keys [item-refs]}]
  (-> recon
      (dissoc :reconciliation/item-refs)
      (assoc :reconciliation/items
             (mapv first item-refs))))

(defmethod datomic/after-read :reconciliation
  [recon]
  (update-in recon [:reconciliation/end-of-period] ->local-date))

