(ns clj-money.db.sql.prices
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-money.db.sql :as sql]))

(defmethod sql/after-read :price
  [price]
  (update-in price [:price/trade-date] t/local-date))
