(ns clj-money.db.sql.entities
  (:require [stowaway.criteria :as criteria]
            [clj-money.db.sql :as sql]))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :entity/user)

(defmethod sql/prepare-criteria :entity
  [criteria]
  (criteria/apply-to criteria ->sql-refs))

(defmethod sql/before-save :entity
  [entity]
  (->sql-refs entity))
