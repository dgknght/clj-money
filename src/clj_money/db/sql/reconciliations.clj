(ns clj-money.db.sql.reconciliations
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [stowaway.criteria :as criteria]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db :as db]
            [clj-money.db.sql :as sql]))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :reconciliation/account)

(declare ->model-refs)
(sql/def->model-refs ->model-refs :reconciliation/account)

(defn- coerce
  [m]
  (-> m
      ->sql-refs
      (update-in-if [:reconciliation/status] name)))

(defmethod sql/before-save :reconciliation
  [recon]
  (-> recon
      coerce
      (dissoc :reconciliation/item-refs)))

(defmethod sql/after-read :reconciliation
  [recon]
  (-> recon
      ->model-refs
      (update-in [:reconciliation/status] keyword)
      (update-in [:reconciliation/end-of-period] t/local-date)))

(defmethod sql/prepare-criteria :reconciliation
  [criteria]
  (criteria/apply-to criteria coerce))

(defmethod sql/post-select :reconciliation
  [storage reconciliations]
  (map #(assoc %
               :reconciliation/item-refs
               (mapv (juxt :id :transaction-item/transaction-date)
                     (db/select storage {:transaction-item/reconciliation %} {})))
       reconciliations))
