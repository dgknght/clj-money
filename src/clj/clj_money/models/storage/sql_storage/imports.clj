(ns clj-money.models.storage.sql-storage.imports
  (:require [clojure.java.jdbc :as jdbc]
            [honeysql.helpers :refer [select
                                      from]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          update-model
                                                          append-sort
                                                          append-where
                                                          append-limit]]
            [clj-money.models.storage.sql-storage :as stg]))

(defmethod stg/select ::models/import
  [criteria options db-spec]
  (query db-spec (-> (select :imports.*)
                     (from :imports)
                     (append-where criteria)
                     (append-limit options)
                     (append-sort options))))

(defmethod stg/insert ::models/import
  [imp db-spec]
  (insert-model db-spec :imports imp
                :entity-name
                :user-id
                :image-ids))

(defmethod stg/update ::models/import
  [imp db-spec]
  (update-model db-spec :imports imp :progress))

(defmethod stg/delete ::models/import
  [{:keys [id]} db-spec]
  (jdbc/delete! db-spec :imports ["id = ?" id]))
