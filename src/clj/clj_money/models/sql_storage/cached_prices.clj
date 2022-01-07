(ns clj-money.models.sql-storage.cached-prices
  (:require [honeysql.helpers :refer [select
                                      from]]
            [stowaway.sql :refer [apply-limit
                                  apply-sort
                                  select-count]]
            [dgknght.app-lib.core :refer [deep-contains?]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          apply-criteria]]
            [clj-money.models.sql-storage :as stg]))

(defmethod stg/select ::models/cached-price
  [criteria options db-spec]
  {:pre [(deep-contains? criteria :trade-date)]}
  (let [sql (-> (select :cached-prices.*)
                (from :cached_prices)
                (apply-criteria criteria {:target :cached-price})
                (apply-limit options)
                (apply-sort options)
                (select-count options))]
    (query db-spec sql)))

(defmethod stg/insert ::models/cached-price
  [cached-price db-spec]
  (insert-model db-spec
                :cached-prices
                cached-price
                :symbol
                :trade-date
                :exchange
                :price))
