(ns clj-money.models.storage.sql-storage.images
  (:require [clojure.java.jdbc :as jdbc]
            [honeysql.helpers :refer [select
                                      merge-select
                                      from]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          append-where
                                                          append-limit]]
            [clj-money.models.storage.sql-storage :as stg]))

(defn- append-body
  [sql {:keys [include-body?]}]
  (if include-body?
    (merge-select sql :body)
    sql))

(defmethod stg/select ::models/image
  [criteria options db-spec]
  (query db-spec (-> (select :id
                             :user_id
                             :original_filename
                             :content_type
                             :body_hash
                             :created_at)
                     (from :images)
                     (append-where criteria)
                     (append-body options)
                     (append-limit options))))

(defmethod stg/insert ::models/image
  [image db-spec]
  (insert-model db-spec :images image
                :user-id
                :original-filename
                :content-type
                :body-hash
                :body))

(defmethod stg/delete ::models/image
  [{:keys [id]} db-spec]
  (jdbc/delete! db-spec :images ["id = ?" id]))
