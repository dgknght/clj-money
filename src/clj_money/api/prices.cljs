(ns clj-money.api.prices
  (:refer-clojure :exclude [update])
  (:require [dgknght.app-lib.models :refer [->id]]
            [dgknght.app-lib.web :refer [unserialize-date
                                         serialize-date]]
            [dgknght.app-lib.api-async :as api]
            [dgknght.app-lib.decimal :refer [->decimal]]
            [clj-money.api :refer [handle-ex]]))

(defn- after-read
  [price]
  (let [trade-date (unserialize-date (:trade-date price))]
    (-> price
        (update-in [:price] ->decimal)
        (assoc
          :trade-date trade-date
          :original-trade-date trade-date))))

(defn- prepare-criteria
  [criteria]
  (update-in criteria [:trade-date] #(map serialize-date %)))

(defn- transform
  [xf]
  (comp (api/apply-fn after-read)
        xf))

(defn search
  [criteria xf]
  {:pre [(some #(contains? criteria %) [:commodity-id :entity-id])
         (contains? criteria :trade-date)]}
  (api/get (api/path :prices)
           (prepare-criteria criteria)
           {:transform (transform xf)
            :handle-ex (handle-ex "Unable to retrieve the prices: %s")}))

(defn create
  [price xf]
  (api/post (api/path :commodities
                      (:commodity-id price)
                      :prices)
            (-> price
                (select-keys [:price :trade-date])
                (update-in [:trade-date] serialize-date))
            {:transform (transform xf)
             :handle-ex (handle-ex "Unable to create the price: %s")}))

(defn update
  [price xf]
  (api/patch (api/path :prices
                       (serialize-date (:original-trade-date price))
                       (:id price))
             (-> price
                 (select-keys [:price :commodity-id :trade-date])
                 (update-in [:trade-date] serialize-date))
             {:transform (transform xf)
              :handle-ex (handle-ex "Unable to update the price: %s")}))

(defn save
  [price xf]
  (let [f (if (:id price) update create)]
    (f price xf)))

(defn delete
  [price xf]
  (api/delete (api/path :prices
                        (serialize-date (:trade-date price))
                        (->id price))
              {:transform xf
               :handle-ex (handle-ex "Unable to remove the price: %s")}))

(defn fetch
  "Gets commodity prices from an external source"
  [commodity-ids xf]
  (api/get (api/path :prices :fetch)
           {:commodity-id commodity-ids}
           {:transform (transform xf)
            :handle-ex (handle-ex "Unable to fetch external price information: %s")}))
