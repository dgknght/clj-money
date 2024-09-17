(ns clj-money.models.sql-storage.prices
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [honeysql.helpers :refer [select
                                      from]]
            [stowaway.sql :refer [apply-limit
                                  apply-sort
                                  select-count]]
            [stowaway.criteria :as criteria]
            [dgknght.app-lib.core :refer [deep-contains?
                                          update-in-if]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          update-model
                                                          apply-criteria]]
            [clj-money.models.sql-storage :as stg]))

(defn- after-read
  [price]
  (update-in price [:trade-date] t/local-date))

(defmulti ->sql-date type)

(defmethod ->sql-date clojure.lang.PersistentVector
  [[oper & vs]]
  (apply vector oper (map ->sql-date vs)))

(defmethod ->sql-date :default
  [d]
  (t/sql-date d))

(defmethod stg/select ::models/price
  [criteria options db-spec]
  {:pre [(deep-contains? criteria :trade-date)]}
  (let [sql (-> (select :prices.*)
                (from :prices)
                (apply-criteria (criteria/apply-to criteria
                                                   #(update-in-if % [:trade-date] ->sql-date))
                                {:target :price})
                (apply-limit options)
                (apply-sort options)
                (select-count options))]
    (map after-read (query db-spec sql))))

(defmethod stg/insert ::models/price
  [price db-spec]
  {:pre [(:trade-date price)]}
  (after-read
    (insert-model db-spec
                  :prices
                  price
                  :commodity-id
                  :trade-date
                  :price)))

(defmethod stg/update ::models/price
  [price db-spec]
  {:pre [(:trade-date price)]}
  (update-model db-spec
                :prices
                price
                :trade-date
                :price))

(defmethod stg/delete ::models/price
  [{:keys [id trade-date]} db-spec]
  {:pre [trade-date]}
  (jdbc/delete! db-spec
                :prices
                ["id = ? and trade_date = ?" id trade-date]))
