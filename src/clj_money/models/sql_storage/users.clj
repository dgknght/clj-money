(ns clj-money.models.sql-storage.users
  (:require [clojure.pprint :refer [pprint]]
            [honeysql.helpers :refer [select
                                      merge-select
                                      from]]
            [java-time.api :as t]
            [stowaway.sql :refer [apply-limit]]
            [stowaway.criteria :as criteria]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          update-model
                                                          apply-criteria]]
            [clj-money.models.sql-storage :as stg]))

(defn- after-read
  [user]
  (update-in-if user [:token-expires-at] t/instant))

(defn- before-save
  [user]
  (update-in-if user [:token-expires-at] t/instant->sql-timestamp))

(defn- apply-select-password
  "For a user query, adds the password to the list of selected
  fields if :include-password? is truthy"
  [sql options]
  (if {:include-password? options}
    (merge-select sql :password)
    sql))

(defmulti ^:private ->sql-timestamp type)

(defmethod ->sql-timestamp :default
  [d]
  (t/instant->sql-timestamp d))

(defmethod ->sql-timestamp clojure.lang.PersistentVector
  [[oper & vs]]
  (apply vector oper (map ->sql-timestamp vs)))

(defmethod stg/select ::models/user
  [criteria options db-spec]
  (map after-read
       (query db-spec (-> (select :id :first_name :last_name :email :updated_at :created_at)
                          (from :users)
                          (apply-select-password options)
                          (apply-criteria (criteria/apply-to criteria #(update-in-if % [:token-expires-at] ->sql-timestamp)))
                          (apply-limit options)))))

(defmethod stg/insert ::models/user
  [user db-spec]
  (after-read
    (insert-model db-spec :users (before-save user)
                  :first-name
                  :last-name
                  :email
                  :password)))

(defmethod stg/update ::models/user
  [user db-spec]
  (update-model db-spec :users (before-save user)
                :first-name
                :last-name
                :password
                :password-reset-token
                :token-expires-at))
