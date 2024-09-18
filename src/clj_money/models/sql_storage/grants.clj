(ns clj-money.models.sql-storage.grants
  (:require [clojure.java.jdbc :as jdbc]
            [honeysql.helpers :refer [select
                                      from]]
            [stowaway.sql :refer [apply-limit]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          update-model
                                                          apply-criteria]]
            [clj-money.models.sql-storage :as stg]))

(defmethod stg/select ::models/grant
  [criteria options db-spec]
  (let [sql (-> (select :*)
                (from :grants)
                (apply-criteria criteria)
                (apply-limit options))]
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
