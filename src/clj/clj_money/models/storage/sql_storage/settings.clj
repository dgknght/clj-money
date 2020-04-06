(ns clj-money.models.storage.sql-storage.settings
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [honeysql.helpers :refer [select
                                      update
                                      sset
                                      from
                                      where]]
            [honeysql.format :as sql]
            [stowaway.sql :refer [apply-limit]]
            [clojure.java.jdbc :as jdbc]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          apply-criteria]]
            [clj-money.models.storage.sql-storage :as stg]))

(defmethod stg/select ::models/setting
  [criteria options db-spec]
  (query db-spec (-> (select :name :value)
                     (from :settings)
                     (apply-criteria criteria)
                     (apply-limit options))))

(defmethod stg/insert ::models/setting
  [setting db-spec]
  (jdbc/insert! db-spec
                :settings
                (select-keys setting [:name :value])))

(defmethod stg/update ::models/setting
  [setting db-spec]
  (let [sql (sql/format (-> (update :settings)
                            (sset {:value (:value setting)})
                            (where [:= :name (:name setting)])))]
    (log/debug "update-setting" (prn-str setting) ": " (prn-str sql))
    (jdbc/execute! db-spec sql)))
