(ns clj-money.models.prices
  (:refer-clojure :exclude [update])
  (:require [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [clj-time.core :as t]
            [stowaway.core :as storage :refer [with-storage
                                               with-transacted-storage]]
            [clj-money.util :refer [deep-update-in-if]]
            [clj-money.validation :as validation :refer [with-validation]]
            [clj-money.models :as models]
            [clj-money.models.settings :as settings]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.date-helpers :refer [parse-date-range]]))

(s/def ::commodity-id integer?)
(s/def ::trade-date validation/local-date?)
(s/def ::price decimal?)
(s/def ::id uuid?)
(s/def ::new-price (s/keys :req-un [::commodity-id ::trade-date ::price]))
(s/def ::existing-price (s/keys :req-un [::id ::trade-date ::price] :opt-un [::commodity-id]))

(defn- after-read
  [price & _]
  (when price
    (storage/tag price ::models/price)))

(defn- prepare-criteria
  [criteria]
  (-> criteria
      (deep-update-in-if :trade-date parse-date-range)
      (storage/tag ::models/price)))

(defn search
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (->> (storage/select s (prepare-criteria criteria) options)
          (map after-read)))))

(defn- trade-date-exists?
  [storage {:keys [id commodity-id trade-date]}]
  (seq (remove #(and id (= id (:id %)))
               (search
                 storage
                 {:commodity-id commodity-id
                  :trade-date [:between trade-date trade-date]}))))

(defn- validation-rules
  [storage]
  [(validation/create-rule (complement (partial trade-date-exists? storage))
                           [:trade-date]
                           "Trade date must be unique")])

(defn- before-save
  [price & _]
  (storage/tag price ::models/price))

(defn- apply-date-to-commodity
  [{:keys [earliest-price latest-price] :as commodity} trade-date]
  (cond-> commodity
    (or (nil? latest-price)
        (t/after? trade-date latest-price))
    (assoc :latest-price trade-date)

    (or (nil? earliest-price)
        (t/before? trade-date earliest-price))
    (assoc :earliest-price trade-date)))

(defn create
  [storage-spec {:keys [trade-date commodity-id] :as price}]
  (with-transacted-storage [s storage-spec]
    (with-validation price ::new-price (validation-rules s)
      (when-let [commodity (commodities/find-by-id s commodity-id)]
        (commodities/update s (apply-date-to-commodity commodity trade-date)))
      (after-read (storage/create s (before-save price))))))

(defn find-by
  ([storage-spec criteria]
   (find-by storage-spec criteria {}))
  ([storage-spec criteria options]
   (first (search storage-spec criteria (assoc options :limit 1)))))

(defn find-by-id
  [storage-spec id trade-date]
  (find-by storage-spec {:id id
                         :trade-date trade-date}))

(defn reload
  [storage-spec price]
  (find-by-id storage-spec (:id price) (:trade-date price)))

(defn update
  [storage price]
  (with-storage [s storage]
    (with-validation price ::existing-price (validation-rules s)
      (storage/update s (before-save price)) ; TODO: might need to delete from the old location
      (find-by-id s (:id price) (:trade-date price)))))

(defn delete
  [storage-spec price]
  (with-storage [s storage-spec]
    (storage/delete s price)))

(defn most-recent
  ([storage-spec commodity]
   (most-recent storage-spec commodity nil))
  ([storage-spec {:keys [earliest-price latest-price] :as commodity} as-of]
   {:pre [(map? commodity)]}
   (cond
     (every? #(nil? %) [earliest-price latest-price])
     (log/warnf
       "No price bounding for commodity %s %s"
       (:id commodity)
       (:symbol commodity))

     (and as-of
          (t/after? earliest-price as-of))
     (log/warnf
       "Unable to find %s price for commodity %s %s before first available date %s"
       as-of
       (:id commodity)
       (:symbol commodity)
       earliest-price)

     :else
     (find-by storage-spec
              {:commodity-id (:id commodity)
               :trade-date [:between
                            earliest-price
                            (or as-of
                                latest-price)]}
              {:sort [[:trade-date :desc]]}))))

(defn rebound-commodity
  "Given a commodity, look up the earliest and latest prices and update the
  commodity record"
  [storage commodity]
  (let [criteria {:commodity-id (:id commodity)
                  :trade-date [:between
                               (settings/get storage :earliest-partition-date)
                               (settings/get storage :latest-partition-date)]}
        earliest (find-by storage criteria {:sort [[:trade-date :asc]]})
        latest (find-by storage criteria {:sort [[:trade-date :desc]]})]
    (when (and earliest latest)
      (commodities/update storage (assoc commodity
                                         :earliest-price (:trade-date earliest)
                                         :latest-price (:trade-date latest))))))
