(ns clj-money.models.lots
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clj-time.coerce :as tc]
            [clj-money.util :refer [pprint-and-return]]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage]]
            [clj-money.models.storage :refer [create-lot
                                              select-lots-by-commodity-id]]))

(s/def ::account-id integer?)
(s/def ::commodity-id integer?)
(s/def ::purchase-date validation/local-date?)
(s/def ::shares-purchased decimal?)
(s/def ::new-lot (s/keys :req-un [::account-id
                                  ::commodity-id
                                  ::purchase-date
                                  ::shares-purchased]))

(defn- before-save
  [lot]
  (-> lot
      (update-in [:purchase-date] tc/to-long)
      (update-in [:shares-owned] (fnil identity (:shares-purchased lot)))))

(defn- after-read
  [lot]
  (update-in lot [:purchase-date] tc/to-local-date))

(defn- before-validation
  [lot]
  (coercion/coerce lot [(coercion/rule :local-date [:purchase-date])
                        (coercion/rule :decimal [:shares-purchased])
                        (coercion/rule :integer [:account-id])
                        (coercion/rule :integer [:commodity-id])]))

(defn- validate
  [storage spec lot]
  (validation/validate spec (before-validation lot)))

(defn create
  [storage-spec lot]
  (with-storage [s storage-spec]
    (let [validated (validate s ::new-lot lot)]
      (if (validation/valid? validated)
        (->> validated
             before-save
             (create-lot s)
             after-read)
        validated))))

(defn select-by-commodity-id
  [storage-spec commodity-id]
  (with-storage [s storage-spec]
    (->> commodity-id
         (select-lots-by-commodity-id s)
         (map after-read))))
