(ns clj-money.db.sql.commodities
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.sql :as sql]))

(defmethod sql/before-save :commodity
  [commodity]
  (-> commodity
      (update-in-if [:commodity/exchange] name)
      (update-in [:commodity/type] name)
      (update-in [:commodity/price-config] sql/->json)))

(defmethod sql/after-read :commodity
  [commodity]
  (-> commodity
      (update-in-if [:commodity/exchange] keyword)
      (update-in [:commodity/type] keyword)
      (update-in [:commodity/price-config] sql/json->map)
      (update-in-if [:commodity/earliest-price] t/local-date)
      (update-in-if [:commodity/latest-price] t/local-date)))
