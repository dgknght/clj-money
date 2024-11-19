(ns clj-money.db.sql.identities
  (:require [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [stowaway.criteria :as criteria]
            [clj-money.db.sql :as sql]))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :identity/user)

(declare ->model-refs)
(sql/def->model-refs ->model-refs :identity/user)

(defn- coerce
  [ident]
  (update-in-if ident [:identity/provider] name))

(defmethod sql/before-save :identity
  [ident]
  (-> ident
      ->sql-refs
      coerce))

(defmethod sql/prepare-criteria :identity
  [criteria]
  (criteria/apply-to criteria (comp ->sql-refs
                                    coerce)))

(defmethod sql/after-read :identity
  [ident]
  (-> ident
      ->model-refs
      (update-in [:identity/provider] keyword)))
