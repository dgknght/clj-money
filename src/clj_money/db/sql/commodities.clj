(ns clj-money.db.sql.commodities
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [stowaway.criteria :as criteria]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.sql :as sql]))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :commodity/entity)

(declare ->model-refs)
(sql/def->model-refs ->model-refs :commodity/entity)

(defmethod sql/prepare-criteria :commodity
  [criteria]
  (criteria/apply-to criteria (comp ->sql-refs
                                    #(update-in-if % [:commodity/exchange] name)
                                    #(update-in-if % [:commodity/type] name))))

(defmethod sql/before-save :commodity
  [commodity]
  (-> commodity
      (update-in-if [:commodity/exchange] name)
      (update-in [:commodity/type] name)
      (update-in [:commodity/price-config] sql/->json)
      ->sql-refs))

(defmethod sql/after-read :commodity
  [commodity]
  (-> commodity
      (update-in-if [:commodity/exchange] keyword)
      (update-in [:commodity/type] keyword)
      (update-in [:commodity/price-config] sql/json->map)
      (update-in-if [:commodity/earliest-price] t/local-date)
      (update-in-if [:commodity/latest-price] t/local-date)
      ->model-refs))
