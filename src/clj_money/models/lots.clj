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
                                              select-lots-by-entity-id
                                              select-lots-by-transaction-id
                                              select-lots
                                              update-lot
                                              find-lot-by-id
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

(defn- before-save
  [_ lot]
  (-> lot
      (update-in [:purchase-date] tc/to-long)
      (update-in [:shares-owned] (fnil identity (:shares-purchased lot)))))

(defn- after-read
  ([lot] (after-read nil lot))
  ([_ lot]
   (update-in lot [:purchase-date] tc/to-local-date)))

(defn- before-validation
  [storage lot]
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

(defn select-by-transaction-id
  [storage-spec commodity-id]
  (with-storage [s storage-spec]
    (->> commodity-id
         (select-lots-by-transaction-id s)
         (map after-read))))

(defn find-by-id
  [storage-spec id]
  (with-storage [s storage-spec]
    (->> id
         (find-lot-by-id s)
         after-read)))

(def update
  (update-fn {:before-save before-save
              :before-validation before-validation
              :rules-fn validation-rules
              :update update-lot
              :after-read after-read
              :spec ::existing-lot
              :coercion-rules coercion-rules
              :find find-by-id}))

(defn search
  [storage-spec criteria]
  (with-storage [s storage-spec]
    (->> criteria
         (select-lots s)
         (map after-read))))

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
    (let [lots (->> (select-lots-by-entity-id s entity-id)
                    (map after-read)
                    (filter #(< 0 (compare as-of (:purchase-date %)))))
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
