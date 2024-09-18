(ns clj-money.models.sql-storage.commodities
  (:require [clojure.java.jdbc :as jdbc]
            [java-time.api :as t]
            [honeysql.helpers :refer [select
                                      from]]
            [stowaway.sql :refer [select-count
                                  apply-limit]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          update-model
                                                          apply-criteria]]
            [clj-money.models.sql-storage :as stg]))

(defn- after-read
  [commodity]
  (-> commodity
      (update-in-if [:earliest-price] t/local-date)
      (update-in-if [:latest-price] t/local-date)))

(defmethod stg/select ::models/commodity
  [criteria options db-spec]
  (map after-read
       (query db-spec (-> (select :*)
                          (from :commodities)
                          (select-count options)
                          (apply-criteria criteria {:target :commodity})
                          (apply-limit options)))))

(defmethod stg/insert ::models/commodity
  [commodity db-spec]
  (after-read
    (insert-model db-spec :commodities commodity
                  :name
                  :type
                  :symbol
                  :exchange
                  :entity-id
                  :earliest-price
                  :latest-price
                  :price-config)))

(defmethod stg/update ::models/commodity
  [commodity db-spec]
  (update-model db-spec :commodities commodity
                :entity-id
                :type
                :name
                :symbol
                :exchange
                :earliest-price
                :latest-price
                :price-config))

(defmethod stg/delete ::models/commodity
  [{:keys [id]} db-spec]
  (jdbc/delete! db-spec :commodities ["id = ?" id]))
