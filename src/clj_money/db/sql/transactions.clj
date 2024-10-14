(ns clj-money.db.sql.transactions
  (:require [clojure.pprint :refer [pprint]]
            [stowaway.criteria :as criteria]
            [clj-money.db.sql :as sql]))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :transaction/entity)

(declare ->model-refs)
(sql/def->model-refs ->model-refs :transaction/entity)

(defmethod sql/prepare-criteria :transaction
  [criteria]
  (criteria/apply-to criteria ->sql-refs))

(defmethod sql/before-save :transaction
  [trx]
  (->sql-refs trx))

(defmethod sql/after-read :transaction
  [trx]
  (->model-refs trx))
