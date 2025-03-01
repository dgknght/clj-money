(ns clj-money.db.sql.lots
  (:require [java-time.api :as t]
            [clj-money.db.sql :as sql]))

(defmethod sql/after-read :lot
  [lot]
  (update-in lot [:lot/purchase-date] t/local-date))
