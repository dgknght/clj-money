(ns clj-money.db.sql.entities
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.sql :as sql]
            [clj-money.db.sql.types :refer [->json json->map]]))

(defmethod sql/before-save :entity
  [entity]
  (update-in entity [:entity/settings] ->json))

(defmethod sql/after-read :entity
  [entity]
  (-> entity
      (update-in [:entity/settings] json->map)
      (update-in-if [:entity/settings :settings/monitored-account-ids] set)
      (update-in-if [:entity/settings :settings/inventory-method] keyword)
      (update-in-if [:entity/settings :settings/earliest-transaction-date] t/local-date)
      (update-in-if [:entity/settings :settings/latest-transaction-date] t/local-date)
      (update-in-if [:entity/settings :settings/earliest-price-date] t/local-date)
      (update-in-if [:entity/settings :settings/latest-price-date] t/local-date)))
