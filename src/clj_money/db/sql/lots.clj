(ns clj-money.db.sql.lots
  (:require [java-time.api :as t]
            [clj-money.db.sql :as sql]))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :lot/account :lot/commodity)

(declare ->model-refs)
(sql/def->model-refs ->model-refs :lot/account :lot/commodity)

(defmethod sql/before-save :lot
  [lot]
  (->sql-refs lot))

(defmethod sql/after-read :lot
  [lot]
  (-> lot
      (update-in [:lot/purchase-date] t/local-date)
      ->model-refs))
