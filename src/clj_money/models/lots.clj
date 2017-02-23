(ns clj-money.models.lots
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clj-time.coerce :as tc]
            [clj-money.util :refer [pprint-and-return]]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :refer [create-lot
                                              select-lots-by-commodity-id
                                              update-lot
                                              find-lot-by-id]]))

(s/def ::id integer?)
(s/def ::account-id integer?)
(s/def ::commodity-id integer?)
(s/def ::purchase-date validation/local-date?)
(s/def ::shares-purchased decimal?)
(s/def ::shares-owned decimal?)
(s/def ::new-lot (s/keys :req-un [::account-id
                                  ::commodity-id
                                  ::purchase-date
                                  ::shares-purchased]))
(s/def ::existing-lot (s/keys :req-un [::id
                                       ::account-id
                                       ::commodity-id
                                       ::purchase-date
                                       ::shares-purchased
                                       ::shares-owned]))

(defn- before-save
  [_ lot]
  (-> lot
      (update-in [:purchase-date] tc/to-long)
      (update-in [:shares-owned] (fnil identity (:shares-purchased lot)))))

(defn- after-read
  ([lot] (after-read nil lot))
  ([_ lot]
   (update-in lot [:purchase-date] tc/to-local-date)))

(def ^:private coercion-rules
  [(coercion/rule :local-date [:purchase-date])
   (coercion/rule :decimal [:shares-purchased])
   (coercion/rule :integer [:account-id])
   (coercion/rule :integer [:commodity-id])])

(def create
  (create-fn {:before-save before-save
              :create create-lot
              :after-read after-read
              :spec ::new-lot
              :coercion-rules coercion-rules}))

(defn select-by-commodity-id
  [storage-spec commodity-id]
  (with-storage [s storage-spec]
    (->> commodity-id
         (select-lots-by-commodity-id s)
         (map after-read))))

(defn find-by-id
  [storage-spec id]
  (with-storage [s storage-spec]
    (->> id
         (find-lot-by-id s)
         after-read)))

(def update
  (update-fn {:before-save before-save
              :update update-lot
              :after-read after-read
              :spec ::existing-lot
              :coercion-rules coercion-rules
              :find find-by-id}))
