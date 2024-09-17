(ns clj-money.models.sql-storage.attachments
  (:require [clojure.java.jdbc :as jdbc]
            [honeysql.helpers :refer [select
                                      from]]
            [java-time.api :as t]
            [stowaway.sql :refer [apply-limit]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          update-model
                                                          insert-model
                                                          apply-criteria]]
            [clj-money.models.sql-storage :as stg]))

(defn- after-read
  [attachment]
  (update-in attachment [:transaction-date] t/local-date))

(defmethod stg/select ::models/attachment
  [criteria options db-spec]
  (map after-read
       (query db-spec (-> (select :attachments.*)
                          (from :attachments)
                          (apply-criteria criteria {:target :attachment})
                          (apply-limit options)))))

(defmethod stg/insert ::models/attachment
  [attachment db-spec]
  (after-read
    (insert-model db-spec :attachments attachment
                  :transaction-id
                  :transaction-date
                  :caption
                  :image-id)))

(defmethod stg/update ::models/attachment
  [attachment db-spec]
  (update-model db-spec :attachments attachment
                :caption))

(defmethod stg/delete ::models/attachment
  [{:keys [id]} db-spec]
  (jdbc/delete! db-spec :attachments ["id = ?" id]))
