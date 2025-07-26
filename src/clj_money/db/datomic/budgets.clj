(ns clj-money.db.datomic.budgets
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.datomic :as datomic]))

(defmethod datomic/before-save :budget
  [budget]
  (update-in-if budget [:budget/items] (partial mapv datomic/before-save)))

(defmethod datomic/after-read :budget
  [budget]
  (update-in-if budget [:budget/items] (partial mapv datomic/after-read)))
