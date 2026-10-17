(ns clj-money.db.sql.statement-ingestions
  (:require [clj-money.db.sql :as sql]))

(defmethod sql/after-read :statement-ingestion
  [ingestion]
  (update-in ingestion [:statement-ingestion/status] keyword))
