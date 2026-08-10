(ns clj-money.db.datomic.reconciliations
  (:require [clj-money.util :as util]
            [clj-money.entities.reconciliations :as recs]
            [clj-money.db.datomic :as datomic]))

(defmethod datomic/deconstruct :reconciliation
  [{:as recon :reconciliation/keys [items]}]
  (let [r (-> recon
              util/+id
              (dissoc :reconciliation/items))]
    (concat [r]
            (map #(assoc % :transaction-item/reconciliation r)
                 items)
            (map #(assoc % :transaction-item/reconciliation nil)
                 (::recs/removed-items (meta recon))))))
