(ns clj-money.models.sql-storage.transaction-items
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.java.jdbc :as jdbc]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [honeysql.helpers :refer [with-recursive
                                      select
                                      update
                                      sset
                                      from
                                      join
                                      merge-join
                                      left-join
                                      where]]
            [honeysql.core :as sql]
            [camel-snake-kebab.core :refer [->snake_case_string]]
            [stowaway.sql :refer [apply-sort
                                  apply-limit
                                  select-count]]
            [stowaway.criteria :as criteria]
            [dgknght.app-lib.core :refer [deep-contains?
                                          deep-get
                                          deep-dissoc
                                          update-in-if]]
            [clj-money.models :as models]
            [clj-money.models.storage.sql-helpers :refer [query
                                                          insert-model
                                                          update-model
                                                          apply-criteria]]
            [clj-money.models.sql-storage :as stg]))

(defn- downward-recursion
  [criteria]
  {:pre [(deep-contains? criteria :account-id)]}

  (-> (with-recursive [:raccounts
                       {:union [(-> (select :a.id, :a.parent_id)
                                    (from [:accounts :a])
                                    (where [:= :a.id (deep-get criteria :account-id)]))
                                (-> (select :a.id, :a.parent-id)
                                    (from [:accounts :a])
                                    (join [:raccounts :p] [:= :p.id :a.parent_id]))]}])
      (join :raccounts [:= :raccounts.id :transaction_items.account_id])))

(defn- after-read
  [item]
  (update-in item [:transaction-date] t/local-date))

(defmethod stg/select ::models/transaction-item
  [criteria {:keys [include-children?] :as options} db-spec]
  {:pre [(deep-contains? criteria :transaction-date)]}

  (let [[criteria sql] (if include-children?
                         [(deep-dissoc criteria :account-id)
                          (downward-recursion criteria)]
                         [criteria {}])
        sql (-> sql
                (select :transaction_items.*
                        :transactions.description
                        :transactions.attachment_count
                        [:reconciliations.status :reconciliation_status])
                (from :transaction_items)
                (merge-join :transactions [:= :transactions.id :transaction_items.transaction_id])
                (left-join :reconciliations [:= :reconciliations.id :transaction_items.reconciliation_id])
                (apply-criteria criteria {:target :transaction-item})
                (apply-limit options)
                (apply-sort options)
                (select-count options))]
    (map after-read (query db-spec sql))))

(defn- before-save
  [item]
  (update-in item [:transaction-date] t/sql-date))

(defmethod stg/insert ::models/transaction-item
  [item db-spec]
  {:pre [(:transaction-date item)]}
  (after-read
    (insert-model db-spec
                  :transaction_items
                  (before-save item)
                  :transaction-id
                  :transaction-date
                  :account-id
                  :reconciliation-id
                  :action
                  :quantity
                  :negative
                  :value
                  :index
                  :balance
                  :memo)))

(defn- update-item
  [item db-spec]
  {:pre [(:transaction-date item)]}
  (update-model db-spec
                :transaction_items
                (before-save item)
                :transaction-date
                :reconciliation-id
                :quantity
                :value
                :negative
                :memo
                :action
                :index
                :balance
                :account-id))

(def ^:private allowed-keys
  #{:transaction-date
    :account-id
    :action
    :quantity
    :value
    :balance
    :memo
    :index
    :reconciliation-id
    :negative})

(defmulti update-trx-date type)

(defmethod update-trx-date clojure.lang.PersistentVector
  [[oper & vs]]
  (apply vector oper (map update-trx-date vs)))

(defmethod update-trx-date :default
  [d]
  (t/sql-date d))

(defn- update-items
  [attr criteria db-spec]
  {:pre [(deep-contains? criteria :transaction-date)]}
  (let [sql (-> (update :transaction_items)
                (sset (-> attr
                          (update-in-if [:transaction-date] t/sql-date)
                          (select-keys allowed-keys)
                          (assoc :updated-at (t/sql-timestamp))))
                (apply-criteria
                  (criteria/apply-to
                    criteria
                    #(update-in-if % [:transaction-date] update-trx-date)))
                sql/format)]
    (log/debugf "update transaction_items %s %s -> %s" attr criteria sql)
    (first (jdbc/execute! db-spec sql {:entities ->snake_case_string}))))

(defmethod stg/update ::models/transaction-item
  [& args]
  (case (count args)
    2 (apply update-item args)
    3 (apply update-items args)
    (throw (.ArityException "Expected 2 or 3 arguments"))))

(defmethod stg/delete ::models/transaction-item
  [{:keys [transaction-date id]} db-spec]
  {:pre [transaction-date]}
  (jdbc/delete! db-spec
                :transaction_items
                ["id = ? and transaction_date = ?" id transaction-date]))
