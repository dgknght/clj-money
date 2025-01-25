(ns clj-money.api.prices
  (:refer-clojure :exclude [update])
  (:require [dgknght.app-lib.models :refer [->id]]
            [dgknght.app-lib.web :refer [serialize-date]]
            [clj-money.util :refer [update-keys]]
            [clj-money.models :as models]
            [clj-money.api :as api :refer [handle-ex]]))

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
           (merge
             {:on-error (handle-ex "Unable to retrieve the prices: %s")}
             opts)))

(defn create
  [price opts]
  (api/post (api/path :commodities
                      (:commodity-id price)
                      :prices)
            price
            (merge
              {:on-error (handle-ex "Unable to create the price: %s")}
              opts)))

(defn update
  [price opts]
  (api/patch (api/path :prices
                       (serialize-date (:original-trade-date price))
                       (:id price))
             price
             (merge
               {:on-error (handle-ex "Unable to update the price: %s")}
               opts)))

(defn save
  [price & {:as opts}]
  (let [f (if (:id price) update create)]
    (f (models/prune price :price)
       opts)))

(defn delete
  [price xf]
  (api/delete (api/path :prices
                        (serialize-date (:trade-date price))
                        (->id price))
              {:transform xf
               :handle-ex (handle-ex "Unable to remove the price: %s")}))

(defn fetch
  "Gets commodity prices from an external source"
  [commodity-ids & {:as opts}]
  (api/get (api/path :prices :fetch)
           {:commodity-id commodity-ids}
           (merge {:on-error (handle-ex "Unable to fetch external price information: %s")}
                  opts)))
