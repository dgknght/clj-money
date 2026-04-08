(ns clj-money.db.sql.commodities
  (:require [java-time.api :as t]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.sql :as sql]))

(defmethod sql/after-read :commodity
  [commodity]
  (-> commodity
      (update-in-if [:commodity/price-date-range 0] t/local-date)
      (update-in-if [:commodity/price-date-range 1] t/local-date)))
