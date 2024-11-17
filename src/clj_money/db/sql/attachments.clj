(ns clj-money.db.sql.attachments
  (:require [stowaway.criteria :as criteria]
            [clj-money.db.sql :as sql]))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :attachment/image :attachment/transaction)

(declare ->model-refs)
(sql/def->model-refs ->model-refs :attachment/image :attachment/transaction)

(defmethod sql/before-save :attachment
  [att]
  (->sql-refs att))

(defmethod sql/prepare-criteria :attachment
  [criteria]
  (criteria/apply-to criteria ->sql-refs))

(defmethod sql/after-read :attachment
  [att]
  (->model-refs att))
