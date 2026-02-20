(ns clj-money.db.datomic.memo-ledger-entries
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.dates :refer [->local-date]]
            [clj-money.db.datomic :as datomic]))

(defmethod datomic/after-read :memo-ledger-entry
  [entry]
  (update-in-if entry [:memo-ledger-entry/transaction-date] ->local-date))
