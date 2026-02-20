(ns clj-money.db.sql.memo-ledger-entries
  (:require [java-time.api :as t]
            [clj-money.db.sql :as sql]))

(defmethod sql/after-read :memo-ledger-entry
  [entry]
  (update-in entry [:memo-ledger-entry/transaction-date] t/local-date))
