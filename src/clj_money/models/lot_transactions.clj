(ns clj-money.models.lot-transactions
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clj-time.coerce :as tc]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :refer [create-lot-transaction
                                              select-lot-transactions]]))

(s/def ::account-id integer?)
(s/def ::new-lot-transaction (s/keys :req-un [::account-id]))

(defn- before-save
  [_ lot-transaction]
  (-> lot-transaction
      (update-in [:action] name)
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
              :spec ::new-lot-transaction
              :create create-lot-transaction
              :after-read after-read}))

(defn select
  [storage-spec criteria]
  (with-storage [s storage-spec]
    (map after-read
         (select-lot-transactions s criteria))))
