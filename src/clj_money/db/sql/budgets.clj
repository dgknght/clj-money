(ns clj-money.db.sql.budgets
  (:require [clojure.pprint :refer [pprint]]
            [stowaway.criteria :as criteria]
            [clj-money.db.sql :as sql]))

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

(defmethod sql/after-read :budget
  [budget]
  (-> budget
      (update-in [:budget/period] keyword)
      ->model-refs))
