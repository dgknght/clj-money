(ns clj-money.models.storage.sql-storage.accounts
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

(defmethod stg/select ::models/grant
  [criteria options db-spec]
  (let [sql (-> (select :*)
                (from :grants)
                (append-where criteria)
                (append-limit options))]
    (query db-spec sql)))

(defmethod stg/insert ::models/grant
  [grant db-spec]
  (insert-model db-spec :grants grant
                :entity-id
                :user-id
                :permissions))

(defmethod stg/update ::models/grant
  [grant db-spec]
  (update-model db-spec :grants grant :permissions))

(defmethod stg/delete ::models/grant
  [{:keys [id]} db-spec]
  (jdbc/delete! db-spec :grants ["id = ?" id]))
