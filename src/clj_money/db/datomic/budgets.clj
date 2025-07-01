(ns clj-money.db.datomic.budgets
  (:require [clj-money.dates :refer [->local-date]]
            [clj-money.db.datomic :as datomic]))

(defmethod datomic/after-read :budget
  [budget]
  (-> budget
      (update-in [:budget/start-date] ->local-date)
      (update-in [:budget/end-date] ->local-date)))
