(ns clj-money.db.sql.budgets
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-money.util :as util]
            [clj-money.db :as db]
            [clj-money.db.sql :as sql]))

(defmethod sql/before-save :budget
  [budget]
  (update-in budget [:budget/period] name))

(defmethod sql/deconstruct :budget
  [{:budget/keys [items] :keys [id] :as budget}]
  (cons (dissoc budget :budget/items)
        (map #(assoc % :budget-item/budget-id id)
             items)))

(defmethod sql/after-read :budget
  [budget]
  (-> budget
      (update-in [:budget/start-date] t/local-date)
      (update-in [:budget/end-date] t/local-date)
      (update-in [:budget/period] keyword)))

(defmethod sql/post-select :budget
  [storage budgets]
  (map #(assoc %
               :budget/items
               (vec (db/select
                      storage {:budget-item/budget %} {})))
       budgets))

(defmethod sql/reconstruct :budget
  [models]
  (->> models
       (map #(dissoc % :budget-item/budget))
       (util/reconstruct {:parent? :budget/name
                          :child? :budget-item/account
                          :children-key :budget/items})))
