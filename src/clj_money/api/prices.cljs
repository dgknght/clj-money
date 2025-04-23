(ns clj-money.api.prices
  (:refer-clojure :exclude [update])
  (:require [cljs.pprint :refer [pprint]]
            [clj-money.dates :refer [serialize-local-date]]
            [clj-money.util :as util :refer [update-keys]]
            [clj-money.models :as models]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn- prepare-criteria
  [criteria]
  (-> criteria
      (dissoc :price/commodity)
      (update-in [:price/trade-date] #(map serialize-local-date %))
      (update-keys (comp keyword name))))

(defn select
  [{:as criteria :price/keys [commodity]} & {:as opts}]
  {:pre [(contains? criteria :price/trade-date)]}
  (let [p (if commodity
            (api/path :commodities commodity :prices)
            (api/path :prices))]
    (api/get p
             (prepare-criteria criteria)
             (add-error-handler opts "Unable to retrieve the prices: %s"))))

(defn create
  [price opts]
  (api/post (api/path :commodities
                      (:price/commodity price)
                      :prices)
            (dissoc price :price/commodity)
            (add-error-handler opts "Unable to create the price: %s")))

(defn update
  [price opts]
  (api/patch (api/path :prices
                       (serialize-local-date (:price/original-trade-date price))
                       price)
             (dissoc price :price/commodity)
             (add-error-handler opts "Unable to update the price: %s")))

(defn save
  [price & {:as opts}]
  (let [f (if (:id price) update create)]
    (f (models/prune price :price)
       opts)))

(defn delete
  [price & {:as opts}]
  (api/delete (api/path :prices
                        (serialize-local-date (:price/trade-date price))
                        price)
              (add-error-handler opts "Unable to remove the price: %s")))

(defn fetch
  "Gets commodity prices from an external source"
  [commodity-ids & {:as opts}]
  (api/get (api/path :prices :fetch)
           {:commodity-id commodity-ids}
           (add-error-handler opts "Unable to fetch external price information: %s")))
