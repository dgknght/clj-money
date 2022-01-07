(ns clj-money.api.prices
  (:refer-clojure :exclude [update])
  (:require [dgknght.app-lib.models :refer [->id]]
            [dgknght.app-lib.web :refer [unserialize-date
                                         serialize-date]]
            [dgknght.app-lib.api :as api]
            [dgknght.app-lib.decimal :refer [->decimal]]))

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

(defn search
  [criteria success-fn error-fn]
  {:pre [(some #(contains? criteria %) [:commodity-id :entity-id])
         (contains? criteria :trade-date)]}
  (api/get (api/path :prices)
           (prepare-criteria criteria)
           (comp success-fn
                 #(map after-read %))
           error-fn))

(defn create
  [price success-fn error-fn]
  (api/post (api/path :commodities
                      (:commodity-id price)
                      :prices)
            (-> price
                (select-keys [:price :trade-date])
                (update-in [:trade-date] serialize-date))
            (comp success-fn after-read)
            error-fn))

(defn update
  [price success-fn error-fn]
  (api/patch (api/path :prices
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
  (api/delete (api/path :prices
                        (serialize-date (:trade-date price))
                        (->id price))
              success-fn
              error-fn))

(defn fetch
  [commodity-ids success-fn error-fn]
  (api/get (api/path :prices :fetch)
           {:commodity-id commodity-ids}
           success-fn
           error-fn))
