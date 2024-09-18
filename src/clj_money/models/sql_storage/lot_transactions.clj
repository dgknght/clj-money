(ns clj-money.models.sql-storage.lot-transactions
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clojure.java.jdbc :as jdbc]
            [java-time.api :as t]
            [honeysql.helpers :refer [select
                                      sset
                                      update
                                      where
                                      from]]
            [honeysql.core :as sql]
            [stowaway.sql :refer [apply-limit
                                  select-count]]
            [stowaway.criteria :as criteria]
            [dgknght.app-lib.core :refer [deep-contains?]]
            [clj-money.models :as models]
            [clj-money.models.sql-storage :as stg]
            [clj-money.models.storage.sql-helpers :refer [insert-model
                                                          query
                                                          apply-criteria]]))

(defn- after-read
  [lot-transaction]
  (update-in lot-transaction [:transaction-date] t/local-date))

(defmulti ->sql-date type)

(defmethod ->sql-date clojure.lang.PersistentVector
  [[oper & vs]]
  (apply vector oper (map ->sql-date vs)))

(defmethod ->sql-date :default
  [d]
  (t/sql-date d))

(defmethod stg/select ::models/lot-transaction
  [criteria options db-spec]
  {:pre [(deep-contains? criteria :transaction-date)]}

  (let [result (query db-spec
                      (-> (select :lots_transactions.*)
                          (from :lots_transactions)
                          (select-count options)
                          (apply-criteria (criteria/apply-to criteria
                                                             #(update-in % [:transaction-date] ->sql-date))
                                          {:target :lot-transaction})
                          (apply-limit options)))]
    (if (:count options)
      result
      (map after-read result))))

(defmethod stg/insert ::models/lot-transaction
  [lot-transaction db-spec]
  (after-read
    (insert-model db-spec :lots_transactions lot-transaction
                  :lot-id
                  :transaction-id
                  :transaction-date
                  :lot-action
                  :shares
                  :price
                  :action)))

(defmethod stg/update ::models/lot-transaction
  [{:keys [lot-id
           transaction-id]
    :as lot-transaction}
   db-spec]
  (let [sql (-> (update :lots_transactions)
                (sset (select-keys lot-transaction [:shares :price]))
                (where [:and [:= :lot-id lot-id]
                        [:= :transaction-id transaction-id]])
                sql/format)]
    (log/debugf "update lot-transaction %s: %s"
                lot-transaction
                sql)
    (jdbc/execute! db-spec sql)))

(defmethod stg/delete ::models/lot-transaction
  [{:keys [lot-id transaction-id]} db-spec]
  (jdbc/delete! db-spec :lots_transactions [:and
                                            [:= :lot_id lot-id]
                                            [:= :transaction_id transaction-id]]))
