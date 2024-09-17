(ns clj-money.models.sql-storage.cached-prices
  (:require [clojure.pprint :refer [pprint]]
            [honeysql.helpers :refer [select
                                      from]]
            [java-time.api :as t]
            [stowaway.sql :refer [apply-limit
                                  apply-sort
                                  select-count]]
            [stowaway.criteria :as criteria]
            [dgknght.app-lib.core :refer [deep-contains?]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          apply-criteria]]
            [clj-money.models.sql-storage :as stg]))

(defn- after-read
  [cached-price]
  (update-in cached-price [:trade-date] t/local-date))

(defmethod stg/select ::models/cached-price
  [criteria options db-spec]
  {:pre [(deep-contains? criteria :trade-date)]}
  (let [sql (-> (select :cached-prices.*)
                (from :cached_prices)
                (apply-criteria (criteria/apply-to criteria
                                                   #(update-in % [:trade-date] t/sql-date))
                                {:target :cached-price})
                (apply-limit options)
                (apply-sort options)
                (select-count options))]
    (map after-read (query db-spec sql))))

(defmethod stg/insert ::models/cached-price
  [cached-price db-spec]
  (after-read
    (insert-model db-spec
                  :cached-prices
                  (update-in cached-price [:trade-date] t/sql-date)
                  :symbol
                  :trade-date
                  :exchange
                  :price)))
