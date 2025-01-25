(ns clj-money.api.prices
  (:refer-clojure :exclude [update])
  (:require [cljs.pprint :refer [pprint]]
            [dgknght.app-lib.web :refer [serialize-date]]
            [clj-money.util :refer [update-keys]]
            [clj-money.models :as models]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn- prepare-criteria
  [criteria]
  (-> criteria
      (update-in [:price/trade-date] #(map serialize-date %))
      (update-keys (comp keyword name))))

(defn select
  [criteria & {:as opts}]
  {:pre [(some #(contains? criteria %) [:price/commodity
                                        :commodity/entity])
         (contains? criteria :price/trade-date)]}
  (api/get (api/path :prices)
           (prepare-criteria criteria)
           (add-error-handler opts "Unable to retrieve the prices: %s")))

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
                       (serialize-date (:price/original-trade-date price))
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
                        (serialize-date (:price/trade-date price))
                        price)
              (add-error-handler opts "Unable to remove the price: %s")))

(defn fetch
  "Gets commodity prices from an external source"
  [commodity-ids & {:as opts}]
  (api/get (api/path :prices :fetch)
           {:commodity-id commodity-ids}
           (add-error-handler opts "Unable to fetch external price information: %s")))
