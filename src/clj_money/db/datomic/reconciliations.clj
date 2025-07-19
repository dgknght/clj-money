(ns clj-money.db.datomic.reconciliations
  (:require [clj-money.util :as util]
            [clj-money.db.datomic :as datomic]))

(defmethod datomic/deconstruct :reconciliation
  [{:reconciliation/keys [items] :as recon}]
  (let [with-id (-> recon
                    util/+id
                    (dissoc :reconciliation/items))
        id-only (select-keys with-id [:id])]
    (cons with-id
          (map #(assoc % :transaction-item/reconciliation id-only)
               items))))
