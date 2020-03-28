(ns clj-money.models.storage.sql-storage.users
  (:require [clojure.java.jdbc :as jdbc]
            [honeysql.helpers :refer [select
                                      merge-select
                                      from
                                      join]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          update-model
                                                          append-where
                                                          append-limit]]
            [clj-money.models.storage.sql-storage :as stg]))

(defn- append-select-password
  "For a user query, adds the password to the list of selected
  fields if :include-password? is truthy"
  [sql options]
  (if {:include-password? options}
    (merge-select sql :password)
    sql))

(defmethod stg/select ::models/user
  [criteria options db-spec]
  (query db-spec (-> (select :id :first_name :last_name :email :updated_at :created_at)
                     (from :users)
                     (append-select-password options)
                     (append-where criteria)
                     (append-limit options))))

(defmethod stg/insert ::models/user
  [user db-spec]
  (insert-model db-spec :users user
                :first-name
                :last-name
                :email
                :password))

(defmethod stg/update ::models/user
  [user db-spec]
  (update-model db-spec :users user
                :first-name
                :last-name
                :password
                :password-reset-token
                :token-expires-at))
