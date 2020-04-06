(ns clj-money.models.storage.sql-storage.entities
  (:require [clojure.java.jdbc :as jdbc]
            [honeysql.helpers :refer [select
                                      from]]
            [stowaway.sql :refer [apply-limit
                                  apply-sort]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          update-model
                                                          apply-criteria]]
            [clj-money.models.storage.sql-storage :as stg]))

(defmethod stg/insert ::models/entity
  [entity db-spec]
  (insert-model db-spec :entities entity :name
                                     :user-id
                                     :settings))

(defmethod stg/select ::models/entity
  [criteria options db-spec]
  (query db-spec (-> (select :entities.*)
                     (from :entities)
                     (apply-criteria criteria)
                     (apply-limit options)
                     (apply-sort (merge {:sort [:name]} options)))))

(defmethod stg/update ::models/entity
  [entity db-spec]
  (update-model db-spec :entities entity :name :settings))

(defmethod stg/delete ::models/entity
  [{:keys [id]} db-spec]
  (jdbc/delete! db-spec :entities ["id = ?" id]))
