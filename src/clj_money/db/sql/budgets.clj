(ns clj-money.db.sql.budgets
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [stowaway.criteria :as criteria]
            [clj-money.db :as db]
            [clj-money.db.sql :as sql]
            [clj-money.db.sql.types :refer [temp-id]]))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :budget/entity)

(declare ->model-refs)
(sql/def->model-refs ->model-refs :budget/entity)

(defmethod sql/prepare-criteria :budget
  [criteria]
  (criteria/apply-to criteria ->sql-refs))

(defmethod sql/before-save :budget
  [budget]
  (-> budget
      (update-in [:budget/period] name)
      ->sql-refs))

(defmethod sql/deconstruct :budget
  [{:budget/keys [items] :as budget}]
  (let [budget-id (or (:id budget)
                      (temp-id))]
    (cons (-> budget
              (assoc :id budget-id)
              (dissoc :budget/items))
          (map #(assoc % :budget-item/budget-id budget-id)
               items))))

(defmethod sql/resolve-temp-ids :budget-item
  [budget-item id-map]
  (update-in budget-item [:budget-item/budget-id] id-map))

(defmethod sql/after-read :budget
  [budget]
  (-> budget
      (update-in [:budget/start-date] t/local-date)
      (update-in [:budget/end-date] t/local-date)
      (update-in [:budget/period] keyword)
      ->model-refs))

(defmethod sql/post-select :budget
  [storage budgets]
  (map #(assoc %
               :budget/items
               (vec (db/select
                      storage {:budget-item/budget %} {})))
       budgets))

(defmethod sql/reconstruct :budget
  [models]
  ; This logic assume the order established in deconstruct is maintained
  (let [{:keys [current budgets]}
        (reduce (fn [{:keys [current] :as res} mod]
                  (if (:budget/name mod)
                    (cond-> (assoc res :current mod)
                      current (update-in [:budgets] conj current))
                    (update-in res [:current :budget/items] (fnil conj []) mod)))
                {:current nil
                 :budgets []}
                models)]
    (conj budgets current)))
