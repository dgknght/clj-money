(ns clj-money.db.sql.budgets
  (:require [clojure.pprint :refer [pprint]]
            [stowaway.criteria :as criteria]
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
      (update-in [:budget/period] keyword)
      ->model-refs))
