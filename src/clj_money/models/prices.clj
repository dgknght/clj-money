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
  [(coercion/rule :decimal [:price])
   (coercion/rule :local-date [:trade-date])])

(defn- trade-date-exists?
  [storage {:keys [commodity-id trade-date]}]
  (seq (select-prices-by-commodity-id storage
                                      commodity-id
                                      {:trade-date (tc/to-long trade-date)})))

(defn- validation-rules
  [storage]
  [(validation/create-rule (complement (partial trade-date-exists? storage))
                           [:trade-date]
                           "Trade date must be unique")])

(defn- before-validation
  [price]
  (coercion/coerce price coercion-rules))

(defn- validate
  [storage spec model]
  (let [prepared (before-validation model)]
    (apply validation/validate spec prepared (validation-rules storage))))

(defn create
  [storage-spec price]
  (with-storage [s storage-spec]
    (let [validated (validate s ::new-price price)]
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
