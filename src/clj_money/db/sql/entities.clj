(ns clj-money.db.sql.entities
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [stowaway.criteria :as criteria]
            [dgknght.app-lib.core :refer [update-in-if]]
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
      (update-in [:entity/settings] sql/->json)
      ->sql-refs))

(defmethod sql/after-read :entity
  [entity]
  (-> entity
      (update-in [:entity/settings] sql/json->map)
      (update-in-if [:entity/settings :settings/monitored-account-ids] set)
      (update-in-if [:entity/settings :settings/inventory-method] keyword)
      (update-in-if [:entity/settings :settings/earliest-transaction-date] t/local-date)
      (update-in-if [:entity/settings :settings/latest-transaction-date] t/local-date)
      (update-in-if [:entity/settings :settings/earliest-price-date] t/local-date)
      (update-in-if [:entity/settings :settings/latest-price-date] t/local-date)
      ->model-refs))
