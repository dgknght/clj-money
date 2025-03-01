(ns clj-money.db.sql.entities
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.util :as util]
            [clj-money.db.sql :as sql]))

(defmethod sql/after-read :entity
  [entity]
  (-> entity
      (update-in-if [:entity/settings :settings/monitored-account-ids] set)
      (update-in-if [:entity/settings :settings/inventory-method] keyword)
      (update-in-if [:entity/settings :settings/earliest-transaction-date] t/local-date)
      (update-in-if [:entity/settings :settings/latest-transaction-date] t/local-date)
      (update-in-if [:entity/settings :settings/earliest-price-date] t/local-date)
      (update-in-if [:entity/settings :settings/latest-price-date] t/local-date)))

(defn- resolve-temp-id
  [id-map]
  (fn [id]
    (if (util/temp-id? id)
      (id-map id)
      id)))

(defmethod sql/resolve-temp-ids :entity
  [{:as entity} id-map]
  (-> entity
      (update-in-if [:entity/settings :settings/lt-capital-gains-account] (resolve-temp-id id-map))
      (update-in-if [:entity/settings :settings/lt-capital-loss-account] (resolve-temp-id id-map))
      (update-in-if [:entity/settings :settings/st-capital-gains-account] (resolve-temp-id id-map))
      (update-in-if [:entity/settings :settings/st-capital-loss-account] (resolve-temp-id id-map))))
