(ns clj-money.db.sql.images
  (:require [stowaway.criteria :as criteria]
            [clj-money.db.sql :as sql]))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :image/user)

(declare ->model-refs)
(sql/def->model-refs ->model-refs :image/user)

(defmethod sql/before-save :image
  [img]
  (->sql-refs img))

(defmethod sql/prepare-criteria :image
  [criteria]
  (criteria/apply-to criteria ->sql-refs))

(defmethod sql/after-read :image
  [img]
  (->model-refs img))
