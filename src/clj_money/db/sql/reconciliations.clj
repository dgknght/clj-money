(ns clj-money.db.sql.reconciliations
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.util :as util]
            [clj-money.models :as models]
            [clj-money.db :as db]
            [clj-money.db.sql :as sql]))

(defn- coerce
  [m]
  (update-in-if m [:reconciliation/status] name))

(defmethod sql/before-save :reconciliation
  [recon]
  (coerce recon))

(defmethod sql/after-read :reconciliation
  [recon]
  (-> recon
      (update-in [:reconciliation/status] keyword)
      (update-in [:reconciliation/end-of-period] t/local-date)))

(defmethod sql/post-select :reconciliation
  [storage reconciliations]
  (map #(assoc %
               :reconciliation/item-refs
               (mapv (juxt :id :transaction-item/transaction-date)
                     (db/select storage {:transaction-item/reconciliation %} {})))
       reconciliations))

(defn- ->range
  [vs & {:keys [compare] :or {compare <}}]
  ((juxt first last) (sort compare vs)))

(defn- item-refs->query
  [item-refs]
  (db/model-type
    {:transaction/transaction-date (apply vector
                                          :between
                                          (->range (mapv second item-refs)
                                                   :compare t/before?))
     :id [:in (mapv first item-refs)]}
    :transaction-item))

(defmethod sql/deconstruct :reconciliation
  [{:as recon :keys [id] :reconciliation/keys [item-refs]}]
  (let [without-refs (dissoc recon :reconciliation/item-refs)]
    (if (seq item-refs)
      (cons without-refs
            (->> (models/select (item-refs->query item-refs))
                 (mapv #(assoc % :transaction-item/reconciliation {:id id}))))
      [without-refs])))

(defmethod sql/reconstruct :reconciliation
  [models]
  (->> models
       (util/reconstruct {:parent? :reconciliation/end-of-period
                          :child? :transaction-item/reconciliation
                          :children-key :reconciliation/item-refs})
       (map (fn [m]
              (if (db/model-type? m :reconciliation)
                (update-in m
                           [:reconciliation/item-refs]
                           (partial mapv (juxt :id :transaction-item/transaction-date)))
                m)))))
