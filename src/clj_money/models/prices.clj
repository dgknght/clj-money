(ns clj-money.models.prices
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clj-time.coerce :as tc]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage]]
            [clj-money.models.storage :refer [create-price
                                              select-prices-by-commodity-id]]))

(s/def ::commodity-id integer?)
(s/def ::trade-date (partial instance? org.joda.time.LocalDate))
(s/def ::price (partial instance? BigDecimal))
(s/def ::new-price (s/keys :req-un [::commodity-id ::trade-date ::price]))

(defn- before-save
  [price]
  (update-in price [:trade-date] tc/to-long))

(defn- after-read
  [price]
  (update-in price [:trade-date] tc/to-local-date))

(def ^:private coercion-rules
  [(coercion/rule :decimal [:price])])

(defn- before-validation
  [price]
  (coercion/coerce price coercion-rules))

(defn- validate
  [spec model]
  (let [prepared (before-validation model)]
    (validation/validate spec prepared)))

(defn create
  [storage-spec price]
  (with-storage [s storage-spec]
    (let [validated (validate ::new-price price)]
      (if (validation/has-error? validated)
        validated
        (->> validated
             before-save
             (create-price s)
             after-read)))))

(defn select-by-commodity-id
  [storage-spec commodity-id]
  (with-storage [s storage-spec]
    (->> (select-prices-by-commodity-id s commodity-id)
         (map after-read))))
