(ns clj-money.models.sql-storage.scheduled-transactions
  (:require [clojure.java.jdbc :as jdbc]
            [java-time.api :as t]
            [honeysql.helpers :refer [select
                                      from]]
            [stowaway.sql :refer [apply-limit
                                  apply-sort]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          update-model
                                                          apply-criteria]]
            [clj-money.models.sql-storage :as stg]))

(defn- after-read
  [sched-tran]
  (-> sched-tran
      (update-in-if [:start-date] t/local-date)
      (update-in-if [:end-date] t/local-date)
      (update-in-if [:last-occurrence] t/local-date)))

(defmethod stg/insert ::models/scheduled-transaction
  [sched-tran db-spec]
  (after-read
    (insert-model db-spec :scheduled_transactions sched-tran
                  :entity-id
                  :description
                  :memo
                  :start-date
                  :end-date
                  :enabled
                  :last-occurrence
                  :date-spec
                  :interval-type
                  :interval-count)))

(defmethod stg/select ::models/scheduled-transaction
  [criteria options db-spec]
  (map after-read
       (query db-spec (-> (select :scheduled_transactions.*)
                          (from :scheduled_transactions)
                          (apply-criteria criteria {:target :scheduled-transaction})
                          (apply-limit options)
                          (apply-sort options)))))

(defmethod stg/update ::models/scheduled-transaction
  [sched-tran db-spec]
  (update-model db-spec
                :scheduled_transactions
                sched-tran
                :description
                :memo
                :start-date
                :end-date
                :enabled
                :date-spec
                :last-occurrence
                :interval-type
                :interval-count))

(defmethod stg/delete ::models/scheduled-transaction
  [{:keys [id]} db-spec]
  (jdbc/delete! db-spec :scheduled_transactions ["id = ?" id]))
