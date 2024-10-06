(ns clj-money.db.sql.entities
  (:require [clojure.pprint :refer [pprint]]
            [stowaway.criteria :as criteria]
            [clj-money.db.sql :as sql]))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :entity/user)

(declare ->model-refs)
(sql/def->model-refs ->model-refs :entity/user)

(defmethod sql/prepare-criteria :entity
  [criteria]
  (criteria/apply-to criteria ->sql-refs))

(defmethod sql/before-save :entity
  [entity]
  (-> entity
      (update-in [:entity/settings] sql/->jsonb)
      ->sql-refs))

(defmethod sql/after-read :entity
  [entity]
  (->model-refs entity))
