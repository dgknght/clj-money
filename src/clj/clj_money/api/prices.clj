(ns clj-money.api.prices
  (:require [clj-money.api :refer [index-resource]]
            [clj-money.x-platform.util :refer [serialize-date
                                               unserialize-date
                                               update-in-criteria]]
            [clj-money.models.prices :as prices]
            [clj-money.permissions.prices]))

(defn- parse-int
  [s]
  (when (and s (string? s) (re-find #"^\d+$" s))
    (Integer/parseInt s)))

(defn index
  [{:keys [params]}]
  (let [criteria (-> (:criteria params)
                     (merge (dissoc params :criteria))
                     (update-in-criteria :trade-date unserialize-date)
                     (select-keys [:commodity-id :entity-id :trade-date]))]
    (index-resource prices/search
                    criteria
                    :price)))
