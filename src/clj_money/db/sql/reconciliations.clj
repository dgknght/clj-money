(ns clj-money.db.sql.reconciliations
  (:require [clj-money.db :as db]
            [clj-money.db.sql :as sql]))

(defmethod sql/post-select :reconciliation
  [{:keys [storage]} reconciliations]
  (map #(assoc %
               :reconciliation/items
               (db/select storage
                          {:transaction-item/reconciliation %}
                          {:select-also [:transaction/transaction-date]}))
       reconciliations))

(defmethod sql/deconstruct :reconciliation
  [{:as recon :keys [id] :reconciliation/keys [items]}]
  (cons (dissoc recon :reconciliation/items)
        (mapv #(assoc % :transaction-item/reconciliation {:id id})
              items)))
