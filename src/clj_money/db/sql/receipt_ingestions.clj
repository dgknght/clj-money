(ns clj-money.db.sql.receipt-ingestions
  (:require [clj-money.db.sql :as sql]))

(defmethod sql/after-read :receipt-ingestion
  [ingestion]
  (update-in ingestion [:receipt-ingestion/status] keyword))
