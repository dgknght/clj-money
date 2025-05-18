(ns clj-money.db.sql.budgets
  (:require [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [parse-int]]
            [clj-money.db :as db]
            [clj-money.db.sql :as sql]))

(defmethod sql/before-save :budget
  [budget]
  (update-in budget [:budget/period 1] name))

(defmethod sql/deconstruct :budget
  [{:budget/keys [items] :keys [id] :as budget}]
  (cons (dissoc budget :budget/items)
        (map #(assoc % :budget-item/budget-id id)
             items)))

(defmethod sql/after-read :budget
  [budget]
  (-> budget
      (update-in [:budget/period 0] parse-int)
      (update-in [:budget/period 1] keyword)))

(defmethod sql/post-select :budget
  [storage budgets]
  (map #(assoc %
               :budget/items
               (vec (db/select
                      storage {:budget-item/budget %} {})))
       budgets))
