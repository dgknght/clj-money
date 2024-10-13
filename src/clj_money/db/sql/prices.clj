(ns clj-money.db.sql.prices
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [stowaway.criteria :as criteria]
            [clj-money.db.sql :as sql]))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :price/commodity)

(declare ->model-refs)
(sql/def->model-refs ->model-refs :price/commodity)

(defmethod sql/prepare-criteria :price
  [criteria]
  (criteria/apply-to criteria ->sql-refs))

(defmethod sql/before-save :price
  [price]
  (->sql-refs price))

(defmethod sql/after-read :price
  [price]
  (-> price
      ->model-refs
      (update-in [:price/trade-date] t/local-date)))
