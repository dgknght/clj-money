(ns clj-money.models.sql-storage.identities
  (:require [clojure.tools.logging :as log]
            [honeysql.helpers :refer [select
                                      from
                                      join]]
            [honeysql.format :as sql]
            [stowaway.sql :refer [apply-limit]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          apply-criteria]]
            [clj-money.models.sql-storage :as stg]))

(defmethod stg/insert ::models/identity
  [ident db-spec]
  (insert-model db-spec :identities ident
                :user-id
                :provider
                :provider-id))

(defmethod stg/select ::models/identity
  [criteria options db-spec]
  (let [sql (-> (select :i.*
                            [:u.first_name :user_first_name]
                            [:u.last_name :user_last_name]
                            [:u.email :user_email])
                  (from [:identities :i])
                  (join [:users :u] [:= :u.id :i.user_id])
                  (apply-criteria criteria {:prefix "i"})
                  (apply-limit options))]
      (log/debugf "select-identities %s - %s" (prn-str criteria) (prn-str (sql/format sql)))
      (query db-spec sql)))
