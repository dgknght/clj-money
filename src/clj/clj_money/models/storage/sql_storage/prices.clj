(ns clj-money.models.storage.sql-storage.prices
  (:require [clojure.java.jdbc :as jdbc]
            [honeysql.helpers :refer [select
                                      from]]
            [clj-money.x-platform.util :refer [deep-contains?
                                    deep-get]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          update-model
                                                          append-where
                                                          append-limit
                                                          append-sort]]
            [clj-money.partitioning :refer [table-name
                                            with-partitioning]]
            [clj-money.models.storage.sql-storage :as stg]))

(defmethod stg/select ::models/price
  [criteria options db-spec]
  {:pre [(deep-contains? criteria :trade-date)]}
  (let [options (if (:sort options)
                  options
                  (assoc options :sort [[:trade-date :desc]]))]
    (with-partitioning
      (partial query db-spec)
      :prices
      (rest (deep-get criteria :trade-date))
      options
      [table]
      (-> (select :prices.*)
          (from [table :prices])
          (append-where criteria {:prefix "prices"
                                  :target :price})
          (append-sort options)
          (append-limit options)))))

(defmethod stg/insert ::models/price
  [price db-spec]
  (insert-model db-spec
                (table-name (:trade-date price) :prices)
                price
                :commodity-id
                :trade-date
                :price))

(defmethod stg/update ::models/price
  [{:keys [trade-date] :as price} db-spec]
  (update-model db-spec
                (keyword (table-name trade-date :prices))
                price
                :trade-date
                :price))

(defmethod stg/delete ::models/price
  [{:keys [id trade-date]} db-spec]
  (jdbc/delete! db-spec
                (keyword (table-name trade-date :prices))
                ["id = ?" id]))
