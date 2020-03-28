(ns clj-money.models.storage.sql-storage.budgets
  (:require [clojure.java.jdbc :as jdbc]
            [honeysql.helpers :refer [select
                                      from]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          update-model
                                                          append-where
                                                          append-limit]]
            [clj-money.models.storage.sql-storage :as stg]))

(defmethod stg/select ::models/budget
  [criteria options db-spec]
  (query db-spec (-> (select :*)
                     (from :budgets)
                     (append-where criteria)
                     (append-limit options))))

(defmethod stg/insert ::models/budget
  [budget db-spec]
  (insert-model db-spec :budgets budget
                :entity-id
                :name
                :period
                :period-count
                :start-date
                :end-date))

(defmethod stg/update ::models/budget
  [budget db-spec]
  (update-model db-spec :budgets budget
                :name
                :period
                :period-count
                :start-date
                :end-date))

(defmethod stg/delete ::models/budget
  [{:keys [id]} db-spec]
  (jdbc/delete! db-spec :budgets ["id = ?" id]))
