(ns clj-money.models.prices
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-time.coerce :as tc]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage
                                                   with-transacted-storage]]
            [clj-money.util :refer [deep-update-in-if
                                    assoc-if]]
            [clj-money.find-in-chunks :as ch]
            [clj-money.validation :as validation :refer [with-validation]]
            [clj-money.models :as models]
            [clj-money.models.settings :as settings]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.date-helpers :refer [earliest-date
                                                   parse-date-range]]))

(s/def ::commodity-id integer?)
(s/def ::trade-date validation/local-date?)
(s/def ::price decimal?)
(s/def ::id uuid?)
(s/def ::new-price (s/keys :req-un [::commodity-id ::trade-date ::price]))
(s/def ::existing-price (s/keys :req-un [::id ::trade-date ::price] :opt-un [::commodity-id]))

(defn- after-read
  [price & _]
  (when price
    (tag price ::models/price)))

(defn- prepare-criteria
  [criteria]
  (-> criteria
      (deep-update-in-if :trade-date parse-date-range)
      (tag ::models/price)))

(defn search
  ([criteria]
   (search criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (->> (storage/select (prepare-criteria criteria) options)
          (map after-read)))))

(defn find-by
  ([criteria]
   (find-by criteria {}))
  ([criteria options]
   (first (search criteria (assoc options :limit 1)))))

(defn find
  ([{:keys [id trade-date]}]
   (find id trade-date))
  ([id trade-date]
   (find-by {:id id
             :trade-date trade-date})))

(defn reload
  [price]
  (find price))

(defn- trade-date-unique?
  [{:keys [id commodity-id trade-date]}]
  (-> {:commodity-id commodity-id
       :trade-date [:between trade-date trade-date]}
      (assoc-if :id (when id [:!= id]))
      find-by
      nil?))

(def ^:private validation-rules
  [(validation/create-rule trade-date-unique?
                           [:trade-date]
                           "Trade date must be unique")])

(defn- before-save
  [price]
  (tag price ::models/price))

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
  [{:keys [trade-date commodity-id] :as price}]
  (with-transacted-storage (env :db)
    (with-validation price ::new-price validation-rules
      (when-let [commodity (commodities/find commodity-id)]
        (commodities/update (apply-date-to-commodity commodity trade-date)))
      (-> price
          before-save
          storage/create
          after-read))))

(defn update
  [price]
  (with-storage (env :db)
    (with-validation price ::existing-price validation-rules
      (-> price
          before-save
          storage/update) ; TODO: might need to delete from the old location
      (find price))))

(defn delete
  [price]
  (with-storage (env :db)
    (storage/delete price)))

(defn most-recent
  ([commodity]
   (most-recent commodity nil))
  ([{:keys [earliest-price latest-price] :as commodity} as-of]
   {:pre [(map? commodity)]}

   (cond
     (every? nil? [earliest-price latest-price])
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
     (find-by {:commodity-id (:id commodity)
               :trade-date [:between
                            earliest-price
                            (or as-of
                                latest-price)]}
              {:sort [[:trade-date :desc]]}))))

(defn rebound-commodity
  "Given a commodity, look up the earliest and latest prices and update the
  commodity record"
  [commodity]
  (let [criteria {:commodity-id (:id commodity)
                  :trade-date [:between
                               (settings/get :earliest-partition-date)
                               (settings/get :latest-partition-date)]}
        earliest (find-by criteria {:sort [[:trade-date :asc]]})
        latest (find-by criteria {:sort [[:trade-date :desc]]})]
    (when (and earliest latest)
      (commodities/update (assoc commodity
                                 :earliest-price (:trade-date earliest)
                                 :latest-price (:trade-date latest))))))

(defn batch-fetch
  ([commodity-ids] (batch-fetch commodity-ids {}))
  ([commodity-ids opts]
   (let [as-of (or (:as-of opts (t/today)))]
     (ch/find commodity-ids
              (merge
               {:start-date as-of
                :time-step (t/years 1)
                :fetch-fn #(search
                            {:commodity-id %1
                             :trade-date [:and
                                          [:> (t/minus %2 (t/years 1))]
                                          [:<= %2]]})
                :transform-fn :price
                :id-fn :commodity-id
                :earliest-date (earliest-date)
                :find-one-fn (fn [prices]
                               (apply max-key
                                      (comp tc/to-long :trade-date)
                                      (filter #(or (= as-of (:trade-date %))
                                                   (t/before? (:trade-date %) as-of))
                                              prices)))}
               opts)))))
