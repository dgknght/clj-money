(ns clj-money.db.sql.accounts
  (:require [clojure.pprint :refer [pprint]]
            [stowaway.criteria :as criteria]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.sql :as sql]))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :account/entity :account/commodity :account/parent)

(declare ->model-refs)
(sql/def->model-refs ->model-refs :account/entity :account/commodity :account/parent)

(defmethod sql/prepare-criteria :account
  [criteria]
  (criteria/apply-to criteria (comp ->sql-refs
                                    #(update-in-if % [:account/type] name))))

(defmethod sql/before-save :account
  [account]
  (-> account
      (update-in [:account/type] name)
      ->sql-refs))

(defmethod sql/after-read :account
  [account]
  (-> account
      (update-in [:account/type] keyword)
      ->model-refs))
