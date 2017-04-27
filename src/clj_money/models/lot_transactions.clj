(ns clj-money.models.lot-transactions
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clj-time.core :as t]
            [clj-time.coerce :as tc]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :refer [create-lot-transaction
                                              select-lot-transactions
                                              update-lot-transaction
                                              delete-lot-transaction]])
  (:import org.joda.time.LocalDate))

(s/def ::lot-id integer?)
(s/def ::trade-date (partial instance? LocalDate))
(s/def ::action #{:buy :sell})
(s/def ::shares decimal?)
(s/def ::price decimal?)
(s/def ::transaction-id integer?)
(s/def ::new-lot-transaction (s/keys :req-un [::lot-id
                                              ::transaction-id
                                              ::trade-date
                                              ::action
                                              ::shares
                                              ::price]))

(def ^:private coercion-rules
  [(coercion/rule :local-date [:trade-date])])

(defn- before-save
  [_ lot-transaction]
  (-> lot-transaction
      (update-in [:action] #(when % (name %)))
      (update-in [:trade-date] tc/to-long)))

(defn- after-read
  ([lot-transaction]
   (after-read nil lot-transaction))
  ([_ lot-transaction]
   (-> lot-transaction
       (update-in [:action] keyword)
       (update-in [:trade-date] tc/to-local-date))))

(def create
  (create-fn {:before-save before-save
              :coercion-rules coercion-rules
              :spec ::new-lot-transaction
              :create create-lot-transaction
              :after-read after-read}))

; TODO change this name to "search" for consistancy
(defn select
  [storage-spec criteria]
  (with-storage [s storage-spec]
    (map after-read (select-lot-transactions s criteria))))

(defn find-by-id
  [storage-spec id]
  (first (select storage-spec {:id id :limit 1})))

(defn link
  "Links a transaction to one or more lot transactions"
  [storage-spec transaction-id lot-transaction-ids]
  (with-storage [s storage-spec]
    (doseq [lot-transaction-id lot-transaction-ids]
      (update-lot-transaction s {:id lot-transaction-id
                                 :transaction-id transaction-id}))))

(defn delete
  "Deletes the specified lot transaction"
  [storage-spec id]
  (with-storage [s storage-spec]
    (delete-lot-transaction s id)))
