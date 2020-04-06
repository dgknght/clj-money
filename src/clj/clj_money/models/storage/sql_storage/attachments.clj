(ns clj-money.models.storage.sql-storage.attachments
  (:require [clojure.java.jdbc :as jdbc]
            [honeysql.helpers :refer [select
                                      from]]
            [stowaway.sql :refer [apply-limit]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          apply-criteria]]
            [clj-money.models.storage.sql-storage :as stg]))

(defmethod stg/select ::models/attachment
  [criteria options db-spec]
  (query db-spec (-> (select :*)
                     (from :attachments)
                     (apply-criteria criteria)
                     (apply-limit options))))

(defmethod stg/insert ::models/attachment
  [attachment db-spec]
  (insert-model db-spec :attachments attachment
                :transaction-id
                :transaction-date
                :caption
                :image-id))

(defmethod stg/delete ::models/attachment
  [{:keys [id]} db-spec]
  (jdbc/delete! db-spec :attachments ["id = ?" id]))
