(ns clj-money.db.sql.lot-notes
  (:require [java-time.api :as t]
            [clj-money.db.sql :as sql]))

(defmethod sql/after-read :lot-note
  [entry]
  (update-in entry [:lot-note/transaction-date] t/local-date))
