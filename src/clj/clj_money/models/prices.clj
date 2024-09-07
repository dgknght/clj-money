(ns clj-money.models.prices
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [config.core :refer [env]]
            [java-time.api :as t]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage
                                                   with-transacted-storage]]
            [dgknght.app-lib.core :refer [assoc-if]]
            [dgknght.app-lib.validation :as v :refer [with-validation]]
            [clj-money.dates :as dates]
            [clj-money.find-in-chunks :as ch]
            [clj-money.models :as models]
            [clj-money.models.settings :as settings]
            [clj-money.models.entities :as entities]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.accounts :as accounts]))

(declare find-by)

(defn- trade-date-unique?
  [{:keys [id commodity-id trade-date]}]
  (-> {:commodity-id commodity-id
       :trade-date [:between trade-date trade-date]}
      (assoc-if :id (when id [:!= id]))
      find-by
      nil?))
(v/reg-spec trade-date-unique? {:message "%s already exists"
                                :path [:trade-date]})

(s/def ::commodity-id integer?)
(s/def ::trade-date v/local-date?)
(s/def ::price decimal?)
(s/def ::id uuid?)
(s/def ::new-price (s/and (s/keys :req-un [::commodity-id ::trade-date ::price])
                          trade-date-unique?))
(s/def ::existing-price (s/and (s/keys :req-un [::id ::trade-date ::price] :opt-un [::commodity-id])
                               trade-date-unique?))

(defn- after-read
  [price & _]
  (when price
    (-> price
        (tag ::models/price)
        (vary-meta assoc :original-values price))))

(defn- prepare-criteria
  [criteria]
  (tag criteria ::models/price))

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

(defn- update-commodity
  [{:keys [commodity-id trade-date] :as price}]
  (when-let [commodity (commodities/find commodity-id)]
    (commodities/update (apply-date-to-commodity commodity trade-date)))
  price)

(defn- update-entity
  [{:keys [trade-date commodity-id] :as price}]
  (let [entity (entities/find-by {[:commodity :id] commodity-id})
        updated (-> entity
                    (update-in [:settings :earliest-price-date] dates/earliest trade-date)
                    (update-in [:settings :latest-price-date] dates/latest trade-date))]
    (when-not (= entity updated)
      (entities/update updated)))
  price)

; TODO: Move this to another namespace?
(defn update-accounts
  ([price] (update-accounts price {}))
  ([{:keys [commodity-id trade-date price] :as p}
    {:keys [skip-account-update?
            price-as-of]}]
   {:pre [(:commodity-id p) (:trade-date p)]}

   (when-not (or skip-account-update?
                 (v/has-error? price))
     (let [criteria (if price-as-of
                      {:commodity-id commodity-id
                       :price-as-of price-as-of}
                      [:and
                       {:commodity-id commodity-id}
                       [:or
                        {:price-as-of nil}
                        {:price-as-of [:< trade-date]}]])]
       (->> (accounts/search criteria)
            (map (comp accounts/update
                       (fn [a]
                         (let [after (assoc a
                                            :price-as-of trade-date
                                            :value (* (get-in a [:quantity] 0M)
                                                      price))]
                           after))))
            doall)))
   p))

(defn create
  ([price] (create price {}))
  ([price opts]
   (with-transacted-storage (env :db)
     (with-validation price ::new-price
       (-> price
           before-save
           storage/create
           after-read
           update-commodity
           update-entity
           (update-accounts opts))))))

(defn- update-meta-for-change
  [after before]
  (when-not (t/= (:trade-date after)
                 (:trade-date before))
    (update-accounts after {:price-as-of (:trade-date before)})
    (update-commodity after))
  after)

(def ^:private working-trade-date
  (some-fn #(-> %
                meta
                :original-values
                :trade-date)
           :trade-date))

(defn update
  [price]
  (with-transacted-storage (env :db)
    (with-validation price ::existing-price
      (let [before (find-by {:id (:id price)
                             :trade-date (working-trade-date price)})]
        (-> price
            before-save
            (update-meta-for-change before)
            storage/update)
        (find price)))))

(defn- update-meta-with-previous
  [{:keys [commodity-id trade-date] :as price}]
  (some-> (find-by {:commodity-id commodity-id
                    :trade-date [:< trade-date]}
                   {:sort [[:trade-date :desc]]})
          (update-accounts {:price-as-of trade-date})
          update-commodity)
  price)

(defn delete
  [price]
  (with-transacted-storage (env :db)
    (-> price
        update-meta-with-previous
        storage/delete)))

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
    (when latest
      (update-accounts latest))
    (when (and earliest latest)
      (commodities/update (assoc commodity
                                 :earliest-price (:trade-date earliest)
                                 :latest-price (:trade-date latest))))))

(defn batch-fetch
  ([commodity-ids] (batch-fetch commodity-ids {}))
  ([commodity-ids opts]
   (let [as-of (or (:as-of opts) (t/local-date))]
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
                :find-one-fn (fn [prices]
                               (apply max-key
                                      (comp t/to-millis-from-epoch :trade-date)
                                      (filter #(or (= as-of (:trade-date %))
                                                   (t/before? (:trade-date %) as-of))
                                              prices)))}
               opts)))))
