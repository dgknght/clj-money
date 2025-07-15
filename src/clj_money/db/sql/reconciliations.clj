(ns clj-money.db.sql.reconciliations
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db :as db]
            [clj-money.db.sql :as sql]))

(defmethod sql/before-save :reconciliation
  [recon]
  (update-in-if recon [:reconciliation/status] name))

(defmethod sql/after-read :reconciliation
  [recon]
  (-> recon
      (update-in [:reconciliation/status] keyword)
      (update-in [:reconciliation/end-of-period] t/local-date)))

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
  (concat [(dissoc recon :reconciliation/items)]
          (map #(assoc % :transaction-item/reconciliation {:id id})
               items)))
