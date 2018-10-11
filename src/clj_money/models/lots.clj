(ns clj-money.models.lots
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clj-time.coerce :refer [to-local-date]]
            [clj-money.util :refer [pprint-and-return
                                    to-sql-date
                                    rev-args]]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :refer [create-lot
                                              select-lots
                                              update-lot
                                              delete-lot]]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.prices :as prices]))

(s/def ::id integer?)
(s/def ::account-id integer?)
(s/def ::commodity-id integer?)
(s/def ::purchase-date validation/local-date?)
(s/def ::purchase-price decimal?)
(s/def ::shares-purchased decimal?)
(s/def ::shares-owned decimal?)
(s/def ::new-lot (s/keys :req-un [::account-id
                                  ::commodity-id
                                  ::purchase-date
                                  ::purchase-price
                                  ::shares-purchased]))
(s/def ::existing-lot (s/keys :req-un [::id
                                       ::account-id
                                       ::commodity-id
                                       ::purchase-date
                                       ::shares-purchased
                                       ::shares-owned]))

(defn- after-read
  [lot & _]
  (update-in lot [:purchase-date] to-local-date))

(defn search
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (map after-read (select-lots s criteria options)))))

(defn find-by
  ([storage-spec criteria]
   (find-by storage-spec criteria {}))
  ([storage-spec criteria options]
   (first (search storage-spec criteria (merge options {:limit 1})))))

(defn find-by-id
  [storage-spec id]
  (find-by storage-spec {:id id}))

(defn- before-save
  [lot & _]
  (-> lot
      (update-in [:purchase-date] to-sql-date)
      (update-in [:shares-owned] (fnil identity (:shares-purchased lot)))))

(defn- before-validation
  [lot storage]
  (assoc lot :account (accounts/find-by-id storage (:account-id lot))))

(defn- account-is-an-asset?
  [_ lot]
  (= :asset (-> lot :account :type)))

(defn- validation-rules
  [storage]
  [(validation/create-rule (partial account-is-an-asset? storage)
                           [:account-id]
                           "The account must be an asset account")])

(def ^:private coercion-rules
  [(coercion/rule :local-date [:purchase-date])
   (coercion/rule :decimal [:shares-purchased])
   (coercion/rule :integer [:account-id])
   (coercion/rule :integer [:commodity-id])])

(def create
  (create-fn {:before-save before-save
              :before-validation before-validation
              :rules-fn validation-rules
              :create (rev-args create-lot)
              :after-read after-read
              :spec ::new-lot
              :coercion-rules coercion-rules}))

(defn select-by-commodity-id
  [storage-spec commodity-id]
  (if commodity-id
    (search storage-spec {:commodity-id commodity-id})
    []))

(def update
  (update-fn {:before-save before-save
              :before-validation before-validation
              :rules-fn validation-rules
              :update (rev-args update-lot)
              :after-read after-read
              :spec ::existing-lot
              :coercion-rules coercion-rules
              :find find-by-id}))

(defn- lot-unrealized-gains
  [storage price-fn as-of {:keys [purchase-price
                                  commodity-id
                                  shares-owned] :as lot}]
  (let [cost (* purchase-price shares-owned)
        value (* (price-fn commodity-id) shares-owned)]
    (- value cost)))

(defn unrealized-gains
  [storage-spec entity-id as-of]
  (with-storage [s storage-spec]
    (let [lots (search s {:entity-id entity-id
                          :purchase-date [:<= as-of]})
          commodity-prices (->> lots
                                (map :commodity-id)
                                (into #{})
                                (map #(vector % (:price (prices/most-recent s % as-of))))
                                (into {}))
          price-fn #(commodity-prices %)]
      (->> lots
           (map #(lot-unrealized-gains s price-fn as-of %))
           (reduce + 0M)))))

(defn delete
  [storage-spec lot-id]
  (with-storage [s storage-spec]
    (delete-lot s lot-id)))
