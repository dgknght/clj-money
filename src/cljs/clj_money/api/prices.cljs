(ns clj-money.api.prices
  (:require [clj-money.api :as api]
            [clj-money.x-platform.util :refer [unserialize-date
                                               serialize-date]]))

(defn- after-read
  [price]
  (update-in price [:trade-date] unserialize-date))

(defmulti ^:private serialize-trade-date
  (fn [{:keys [trade-date]}]
    (when trade-date
      (if (sequential? trade-date)
        :compound
        :simple))))

(defmethod ^:private serialize-trade-date :default
  [criteria]
  criteria)

(defmethod ^:private serialize-trade-date :simple
  [criteria]
  (update-in criteria [:trade-date] serialize-date))

(defmethod ^:private serialize-trade-date :compound
  [{:keys [trade-date] :as criteria}]
  (assoc criteria :trade-date (cons (first trade-date)
                                    (map serialize-date (rest trade-date)))))

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
                           serialize-trade-date)
                       (comp success-fn
                             #(map after-read %))
                       error-fn)))
