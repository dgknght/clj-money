(ns clj-money.models.storage.sql-storage.commodities
  (:require [clojure.java.jdbc :as jdbc]
            [honeysql.helpers :refer [select
                                      from]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          update-model
                                                          append-where
                                                          select-count
                                                          append-limit]]
            [clj-money.models.storage.sql-storage :as stg]))

(defmethod stg/select ::models/commodity
  [criteria options db-spec]
  (query db-spec (-> (select :*)
                     (from :commodities)
                     (select-count options)
                     (append-where criteria {:target :commodity})
                     (append-limit options))))

(defmethod stg/count ::models/commodity
  [criteria db-spec]
  (query db-spec (-> (select :%count.1)
                     (from :commodities)
                     (append-where criteria {:target :commodity}))))

(defmethod stg/insert ::models/commodity
  [commodity db-spec]
  (insert-model db-spec :commodities commodity
                :name
                :type
                :symbol
                :exchange
                :entity-id))

(defmethod stg/update ::models/commodity
  [commodity db-spec]
  (update-model db-spec :commodities commodity
                :entity-id
                :type
                :name
                :symbol
                :exchange))

(defmethod stg/delete ::models/commodity
  [{:keys [id]} db-spec]
  (jdbc/delete! db-spec :commodities ["id = ?" id]))
