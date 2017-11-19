(ns clj-money.models.prices
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clojure.spec :as s]
            [clj-time.core :as t]
            [clj-time.coerce :refer [to-local-date]]
            [clj-money.util :refer [to-sql-date]]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.authorization :as authorization]
            [clj-money.models.helpers :refer [with-storage]]
            [clj-money.models.entities :as entities]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.storage :refer [create-price
                                              find-price-by-id
                                              update-price
                                              select-prices
                                              delete-price]]))

(s/def ::commodity-id integer?)
(s/def ::trade-date (partial instance? org.joda.time.LocalDate))
(s/def ::price (partial instance? BigDecimal))
(s/def ::id integer?)
(s/def ::new-price (s/keys :req-un [::commodity-id ::trade-date ::price]))
(s/def ::existing-price (s/keys :req-un [::id ::trade-date ::price] :opt-un [::commodity-id]))

(defn- before-save
  [price]
  (update-in price [:trade-date] to-sql-date))

(defn- after-read
  [price]
  (when price
    (-> price
        (authorization/tag-resource :price)
        (update-in [:trade-date] to-local-date))))

(def ^:private coercion-rules
  [(coercion/rule :decimal [:price])
   (coercion/rule :local-date [:trade-date])
   (coercion/rule :integer [:commodity-id])])

(defn- trade-date-exists?
  [storage {:keys [id commodity-id trade-date]}]
  (seq (remove #(and id (= id (:id %)))
               (select-prices
                 storage
                 {:commodity-id commodity-id
                  :trade-date (to-sql-date trade-date)}))))

(defn- validation-rules
  [storage]
  [(validation/create-rule (complement (partial trade-date-exists? storage))
                           [:trade-date]
                           "Trade date must be unique")])

(defn- before-validation
  [price]
  (coercion/coerce coercion-rules price))

(defn- validate
  [storage spec model]
  (->> model
       before-validation
       (validation/validate spec (validation-rules storage))))

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

(defn find-by-id
  [storage-spec id]
  (with-storage [s storage-spec]
    (->> id
         (find-price-by-id s)
         after-read)))

(defn search
  [storage-spec criteria]
  (with-storage [s storage-spec]
    (->> (select-prices s criteria)
         (map after-read))))

(defn update
  [storage-spec price]
  (with-storage [s storage-spec]
    (let [validated (validate s ::existing-price price)]
      (if (validation/has-error? validated)
        validated
        (do
          (->> validated
               before-save
               (update-price s))
          (find-by-id s (:id price)))))))

(defn delete
  [storage-spec id]
  (with-storage [s storage-spec]
    (delete-price s id)))

(defn most-recent
  ([storage-spec commodity-id]
   (most-recent storage-spec commodity-id (t/today)))
  ([storage-spec commodity-id as-of]
   (with-storage [s storage-spec]
     (-> (select-prices s
                        {:commodity-id commodity-id}
                        {:limit 1
                         :as-of (to-sql-date as-of)})
         first
         after-read))))
