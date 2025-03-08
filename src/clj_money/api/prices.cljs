(ns clj-money.api.prices
  (:refer-clojure :exclude [update])
  (:require [dgknght.app-lib.models :refer [->id]]
            [dgknght.app-lib.web :refer [serialize-date]]
            [dgknght.app-lib.api-async :as api]
            
            [clj-money.api :refer [add-error-handler]]))

(defn- prepare-criteria
  [criteria]
  (update-in criteria [:trade-date] #(map serialize-date %)))

(defn select
  [criteria & {:as opts}]
  {:pre [(some #(contains? criteria %) [:commodity-id :entity-id])
         (contains? criteria :trade-date)]}
  (api/get (api/path :prices)
           (prepare-criteria criteria)
           (add-error-handler opts "Unable to retrieve the prices: %s")))

(defn create
  [price opts]
  (api/post (api/path :commodities
                      (:commodity-id price)
                      :prices)
            (-> price
                (select-keys [:price :trade-date])
                (update-in [:trade-date] serialize-date))
            (add-error-handler opts "Unable to create the price: %s")))

(defn update
  [price opts]
  (api/patch (api/path :prices
                       (serialize-date (:original-trade-date price))
                       (:id price))
             (-> price
                 (select-keys [:price :commodity-id :trade-date])
                 (update-in [:trade-date] serialize-date))
             (add-error-handler opts "Unable to update the price: %s")))

(defn save
  [price & {:as opts}]
  (let [f (if (:id price) update create)]
    (f price opts)))

(defn delete
  [price & {:as opts}]
  (api/delete (api/path :prices
                        (serialize-date (:trade-date price))
                        (->id price))
              (add-error-handler opts "Unable to remove the price: %s")))

(defn fetch
  "Gets commodity prices from an external source"
  [commodity-ids & {:as opts}]
  (api/get (api/path :prices :fetch)
           {:commodity-id commodity-ids}
           (add-error-handler opts "Unable to fetch external price information: %s")))
