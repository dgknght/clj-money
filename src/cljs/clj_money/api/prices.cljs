(ns clj-money.api.prices
  (:refer-clojure :exclude [update])
  (:require [clj-money.api :as api]
            [clj-money.decimal :refer [->decimal]]
            [clj-money.util :refer [unserialize-date
                                    serialize-date
                                    model->id]]))

(defn- after-read
  [price]
  (let [trade-date (unserialize-date (:trade-date price))]
    (-> price
        (update-in [:price] ->decimal)
        (assoc
          :trade-date trade-date
          :original-trade-date trade-date))))

(defn- adjust-trade-date
  [{:keys [trade-date] :as criteria}]
  (-> criteria
      (assoc :start-date (serialize-date (nth trade-date 1))
             :end-date (serialize-date (nth trade-date 2)))
      (dissoc :trade-date)))

(defn search
  [criteria success-fn error-fn]
  {:pre [(some #(contains? criteria %) [:commodity-id :entity-id])
         (contains? criteria :trade-date)]}
  (let [path (if (contains? criteria :entity-id)
               (api/path :entities
                         (:entity-id criteria)
                         :prices)
               (api/path :commodities
                         (:commodity-id criteria)
                         :prices))]
    (api/get-resources path
                       (-> criteria
                           (dissoc :entity-id :commodity-id)
                           adjust-trade-date)
                       (comp success-fn
                             #(map after-read %))
                       error-fn)))

(defn create
  [price success-fn error-fn]
  (api/create-resource (api/path :commodities
                                 (:commodity-id price)
                                 :prices)
                       (-> price
                           (select-keys [:price :trade-date])
                           (update-in [:trade-date] serialize-date))
                       (comp success-fn after-read)
                       error-fn))

(defn update
  [price success-fn error-fn]
  (api/update-resource (api/path :prices
                                 (serialize-date (:original-trade-date price))
                                 (:id price))
                       (-> price
                           (select-keys [:price :commodity-id :trade-date])
                           (update-in [:trade-date] serialize-date))
                       (comp success-fn after-read)
                       error-fn))

(defn save
  [price success-fn error-fn]
  (let [f (if (:id price) update create)]
    (f price success-fn error-fn)))

(defn delete
  [price success-fn error-fn]
  (api/delete-resource (api/path :prices
                                 (serialize-date (:trade-date price))
                                 (model->id price))
                       success-fn
                       error-fn))
