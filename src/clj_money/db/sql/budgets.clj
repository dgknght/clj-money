(ns clj-money.db.sql.budgets
  (:require [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [parse-int]]
            [clj-money.db :as db]
            [clj-money.models :as models]
            [clj-money.db.sql :as sql]))

(defmethod sql/before-save :budget
  [budget]
  (update-in budget [:budget/period 1] name))

(defn- removed
  [{:budget/keys [items] :as budget}]
  (let [ids (->> items
                 (map :id)
                 set)]
    (when-let [existing (seq (models/before budget :budget/items))]
      (->> existing
           (remove #(ids (:id %)))
           (map #(vector ::db/delete %))))))

(defmethod sql/deconstruct :budget
  [{:budget/keys [items] :keys [id] :as budget}]
  (cons (dissoc budget :budget/items)
        (concat (map #(assoc % :budget-item/budget-id id)
                     items)
                (removed budget))))

(defmethod sql/after-read :budget
  [budget]
  (-> budget
      (update-in [:budget/period 0] parse-int)
      (update-in [:budget/period 1] keyword)))

(defmethod sql/post-select :budget
  [{:keys [storage]}
   budgets]
  (let [items (group-by (comp :id :budget-item/budget)
                        (db/select storage
                                   {:budget-item/budget [:in (map :id budgets)]}
                                   {}))]
    (map #(assoc %
                 :budget/items
                 (items (:id %)))
         budgets)))
