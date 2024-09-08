(ns clj-money.models.sql-storage.lots
  (:require [clojure.tools.logging :as log]
            [clojure.java.jdbc :as jdbc]
            [java-time.api :as t]
            [honeysql.helpers :refer [select
                                      from]]
            [stowaway.sql :refer [apply-sort
                                  apply-limit]]
            [stowaway.criteria :as criteria]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          update-model
                                                          apply-criteria]]
            [clj-money.models.sql-storage :as stg]))

(defmulti update-purchase-date type)

(defmethod update-purchase-date clojure.lang.PersistentVector
  [[oper & vs]]
  (apply vector oper (map update-purchase-date vs)))

(defmethod update-purchase-date :default
  [d]
  (t/sql-date d))

(defmethod stg/select ::models/lot
  [criteria options db-spec]
  (log/debugf "select %s" (prn-str criteria))
  (query db-spec (-> (select :*)
                     (from :lots)
                     (apply-criteria (criteria/apply-to criteria
                                                        #(update-in-if % [:purchase-date] update-purchase-date))
                                     {:target :lot})
                     (apply-limit options)
                     (apply-sort options))))

(defmethod stg/insert ::models/lot
  [lot db-spec]
  (insert-model db-spec :lots lot
                :commodity-id
                :account-id
                :purchase-price
                :purchase-date
                :shares-purchased
                :shares-owned))

(defmethod stg/update ::models/lot
  [lot db-spec]
  (update-model db-spec :lots lot
                :purchase-date
                :account-id
                :commodity-id
                :purchase-price
                :shares-owned
                :shares-purchased))

(defmethod stg/delete ::models/lot
  [{:keys [id]} db-spec]
  (jdbc/delete! db-spec :lots ["id = ?" id]))
