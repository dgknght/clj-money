(ns clj-money.db.datomic.reconciliations
  (:require [clj-money.util :as util]
            [clj-money.db.datomic :as datomic]))

(defmethod datomic/deconstruct :reconciliation
  [{:as recon :reconciliation/keys [items]}]
  (let [r (-> recon
              util/+id
              (dissoc :reconciliation/items))]
    (cons r
          (map #(assoc % :transaction-item/reconciliation r)
               items))))
