(ns clj-money.db.sql.cached-prices
  (:require [clj-money.db.sql :as sql]
            [java-time.api :as t]
            [stowaway.criteria :as criteria]
            [dgknght.app-lib.core :refer [update-in-if]]))

(defn- coerce
  [price]
  (update-in-if price [:cached-price/exchange] name))

(defmethod sql/prepare-criteria :cached-price
  [criteria]
  (criteria/apply-to criteria coerce))

(defmethod sql/before-save :cached-price
  [price]
  (coerce price))

(defmethod sql/after-read :cached-price
  [price]
  (-> price
      (update-in [:cached-price/trade-date] t/local-date)
      (update-in [:cached-price/exchange] keyword)))
