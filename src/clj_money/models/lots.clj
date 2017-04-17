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
                                              select-lots
                                              update-lot
                                              find-lot-by-id]]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.lot-transactions :as lot-transactions]
            [clj-money.models.prices :as prices]))

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

(defn- before-validation
  [storage lot]
  (assoc lot :account (accounts/find-by-id storage (:account-id lot))))

(defn- account-is-an-asset?
  [_ lot]
  (= :asset (-> lot :account :type)))

(defn- account-has-commodities-content?
  [_ lot]
  (= :commodities (-> lot :account :content-type)))

(defn- validation-rules
  [storage]
  [(validation/create-rule (partial account-is-an-asset? storage)
                           [:account-id]
                           "The account must be an asset account")
   (validation/create-rule (partial account-has-commodities-content? storage)
                           [:account-id]
                           "The account must be a commodities account")])

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

(defn shares-as-of
  [storage-spec account-id commodity-id as-of]
  (let [grouped-lot-transactions (->> {:account-id account-id
                                       :commodity-id commodity-id}
                                      ; TODO Combine the following 2 lines into 1 SQL call
                                      (search storage-spec)
                                      (mapcat #(lot-transactions/select storage-spec {:lot-id (:id %)}))
                                      (filter #(>= 0 (compare (:trade-date %) as-of)))
                                      (group-by :action))]
    (apply - (map #(->> (% grouped-lot-transactions)
                        (map :shares)
                        (reduce :+ 0M))
                  [:buy :sell]))))

(defn- lot-shares
  [storage lot-id as-of]
  (->> {:lot-id lot-id}
       (lot-transactions/select storage) ; TODO Move date filtering into the database query
       (filter #(< 0 (compare as-of (:trade-date %))))
       (map #(* (:shares %) (if (= :buy (:action %)) 1 -1)))
       (reduce +)))

(defn- lot-unrealized-gains
  [storage price-fn as-of {:keys [:commodity-id] :as lot}]
  (let [transactions (lot-transactions/select storage {:lot-id (:id lot)})
        purchase-price (->> transactions
                            (filter #(= :buy (:action %)))
                            first
                            :price)
        shares-owned (->> transactions
                          (filter #(< 0 (compare as-of (:trade-date %))))
                          (map #(* (:shares %) (if (= :buy (:action %)) 1 -1)))
                          (reduce +))
        cost (* purchase-price shares-owned)
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
