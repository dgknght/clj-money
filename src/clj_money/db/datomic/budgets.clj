(ns clj-money.db.datomic.budgets
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.entities :as models]
            [clj-money.db :as db]
            [clj-money.db.datomic :as datomic]))

(defmethod datomic/before-save :budget
  [budget]
  (update-in-if budget [:budget/items] (partial mapv datomic/before-save)))

(defn- removals
  [{:as budget :budget/keys [items]}]
  (when-let [before (seq (models/before budget :budget/items))]
    (let [ids (->> items
                   (map :id)
                   set)]
      (->> before
           (remove #(ids (:id %)))
           (mapcat (fn [item]
                     [[::db/delete item]
                      [:db/retract (:id budget) :budget/items (:id item)]]))))))

(defmethod datomic/deconstruct :budget
  [budget]
  (cons budget
        (removals budget)))

(defmethod datomic/after-read :budget
  [budget]
  (update-in-if budget [:budget/items] (partial mapv datomic/after-read)))
