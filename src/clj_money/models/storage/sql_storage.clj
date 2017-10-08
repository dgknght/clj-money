(ns clj-money.models.storage.sql-storage
  (:require [clojure.tools.logging :as log]
            [clojure.spec :as s]
            [clojure.string :as string]
            [clojure.java.jdbc :as jdbc]
            [clojure.pprint :refer [pprint]]
            [clj-time.jdbc]
            [clj-time.core :as t]
            [clj-time.coerce :as tc]
            [honeysql.core :as sql]
            [honeysql.helpers :as h]
            [clj-postgresql.types]
            [clj-money.util :refer [pprint-and-return]]
            [clj-money.models.storage :refer [Storage]])
  (:import java.sql.BatchUpdateException))

(defn- id-criteria?
  [value]
  (or (integer? value)
      (and (seq? value)
           (seq value)
           (every? integer? value))))

(s/def ::user-id integer?)
(s/def ::entity-id id-criteria?)
(s/def ::lot-id integer?)
(s/def ::account-id integer?)
(s/def ::commodity-id integer?)
(s/def ::transaction-id integer?)
(defmulti lot-criteria #(contains? % :account-id))
(defmethod lot-criteria true [_]
  (s/keys :req-un [::account-id] :opt-un[::commodity-id ::entity-id]))
(defmethod lot-criteria false [_]
  (s/keys :req-un [::entity-id] :opt-un[::account-id ::commodity-id]))
(s/def ::lot-criteria (s/multi-spec lot-criteria #(contains? % :account-id)))
(s/def ::lot-transaction-criteria
  (fn [c] (integer? (some #(% c) [:id :lot-id :transaction-id]))))
(s/def ::entity-or-account-id (s/or ::entity-id ::account-id))
(s/def ::commodity-criteria (s/keys :req-un [::entity-id]))
(s/def ::image-criteria (s/keys :req-un [::user-id]))
(defmulti price-criteria #(contains? % :commodity-id))
(defmethod price-criteria true [_]
  (s/keys :req-un [::commodity-id]))
(defmethod price-criteria false [_]
  (s/keys :req-un [::entity-id]))
(s/def ::price-criteria (s/multi-spec price-criteria #(contains? % :commodity-id)))
(defmulti attachment-criteria #(contains? % :id))
(defmethod attachment-criteria true [_]
  (s/keys :req-un [::id]))
(defmethod attachment-criteria false [_]
  (s/keys :req-un [::transaction-id]))
(s/def ::attachment-criteria (s/multi-spec attachment-criteria #(contains? % :id)))
(defmulti transaction-criteria #(contains? % :lot-id))
(defmethod transaction-criteria true [_]
  (s/keys :req-un [::lot-id]))
(defmethod transaction-criteria false [_]
  (s/keys :req-un [::entity-id]))
(s/def ::transaction-criteria (s/multi-spec transaction-criteria #(contains? % :lot-id)))
(defmulti budget-criteria #(contains? % :id))
(defmethod budget-criteria true [_]
  (s/keys :req-un [::id]))
(defmethod budget-criteria false [_]
  (s/keys :req-un [::entity-id]))
(s/def ::budget-criteria (s/multi-spec budget-criteria #(contains? % :id)))
(defmulti grant-criteria #(contains? % :id))
(defmethod grant-criteria true [_]
  (s/keys :req-un [::id]))
(defmethod grant-criteria false [_]
  (s/keys :req-un [::entity-id]))
(s/def ::grant-criteria (s/multi-spec grant-criteria #(contains? % :id)))

(defn- exists?
  [db-spec table where]
  (let [sql (sql/format (-> (h/select [:%count.id :record_count])
                              (h/from table)
                              (h/where where)))]
      (= 1 (->> sql
           (jdbc/query db-spec)
           first
           :record_count))))

(defn- update-keys
  [model f]
  (when model
    (->> model
         (map #(update-in % [0] f))
         (into {}))))

(defn- ->sql-key
  "Accepts a keyword returns it with hyphens replaced with underscores"
  [k]
  (keyword (string/replace (name k) "-" "_")))

(defn- ->sql-keys
  "Accepts a hash and replaces hyphens in key names
  with underscores"
  [model]
  (update-keys model ->sql-key))

(defn- ->clojure-key
  "Accepts a keyword returns it with underscores replaced with hyphens"
  [k]
  (keyword (string/replace (name k) "_" "-")))

(defn- ->clojure-keys
  "Accepts a hash and replaces underscores in key names
  with hyphens"
  [model]
  (if (map? model)
    (update-keys model ->clojure-key)
    model))

(defn- insert
  "Inserts a record into the specified table"
  [db-spec table model & allowed-keys]
  (->> (select-keys model allowed-keys)
       ->sql-keys
       (jdbc/insert! db-spec table)
       first
       ->clojure-keys))

(defn- ->update-set
  "Prepares a model for update"
  [model & keys]
  (-> model
      (select-keys keys)
      (assoc :updated-at (t/now))
      ->sql-keys))

(defn- map->where
  ([m] (map->where m {}))
  ([m options]
   (let [prefix-fn (if-let [prefix (:prefix options)]
                     #(keyword (format "%s.%s" prefix (name %)))
                     identity)]
     (if (= 1 (count m))
       [:= (-> m keys first prefix-fn) (-> m vals first)]
       (reduce (fn [result [k v]]
                 (let [operator (if (coll? v) :in :=)
                       val-fn #(if (keyword? %) (name %) %)
                       value (if (coll? v)
                               (mapv val-fn v)
                               (val-fn v))]
                   (conj result [operator (prefix-fn k) value])))
               [:and]
               m)))))

(defn- append-where
  [sql options]
  (if-let [where (:where options)]
    (reduce (fn [s [k v]]
              (h/merge-where s [:= k v]))
            sql
            where)
    sql))

(defn- append-paging
  [sql options]
  (cond-> sql
    (:per-page options) (h/limit (:per-page options))
    (:page options) (h/offset (* (:per-page options) (- (:page options) 1)))))

(defn- append-limit
  [sql options]
  (if-let [limit (:limit options)]
    (h/limit sql limit)
    sql))

(defn- append-prices-as-of
  "This is bit of a kludge because the logic for converting a map
  to a where clause is limited. Should really make that more robust, 
  then we won't need this."
  [criteria options]
  (if-let [as-of (:as-of options)]
    (-> criteria
        (h/order-by [:trade-date :desc])
        (h/merge-where [:<= :trade-date as-of]))
    criteria))

(defn- query
  "Executes a SQL query and maps field names into
  clojure keys"
  [db-spec sql-map]
  (->> (sql/format sql-map)
       (jdbc/query db-spec)
       (map ->clojure-keys)))

(defn- query-scalar
  "Executes the SQL query and returns the first column of
  the first record of the result"
  [db-spec sql-map]
  (->> (sql/format sql-map)
       (jdbc/query db-spec)
       first
       vals
       first))

(defn- validate-criteria
  [criteria spec]
  (when-not (s/valid? spec criteria)
      (let [explanation (s/explain-data spec criteria)]
        (throw (ex-info
                 (str "The criteria is not valid: " explanation)
                 {:criteria criteria
                  :explanation explanation})))))

(defn- transaction-item-base-query
  []
  (-> (h/select :i.* :t.transaction_date :t.description, [:r.status "reconciliation_status"])
      (h/from [:transaction_items :i])
      (h/join [:transactions :t] [:= :t.id :i.transaction_id])
      (h/left-join [:reconciliations :r] [:= :r.id :i.reconciliation_id])))

(defn- transaction-query
  [select criteria options]
  (cond-> (-> (h/select select)
              (h/from [:transactions :t])
              (append-paging options))

    (not (empty? (dissoc criteria :lot-id)))
    (h/where (map->where (dissoc criteria :lot-id)))

    (contains? criteria :lot-id)
    (-> (h/join [:lots_transactions :lt]
                [:= :t.id :lt.transaction_id])
        (h/merge-where [:= :lt.lot_id (:lot-id criteria)]))))

(deftype SqlStorage [db-spec]
  Storage

  ; Users
  (create-user
    [_ user]
    (insert db-spec :users user :first-name
                                :last-name
                                :email
                                :password))

  (select-users
    [_]
    (query db-spec (-> (h/select :first_name :last_name :email)
                       (h/from :users))))

  (find-user-by-email
    [this email]
    (->> (-> (h/select :id :first_name :last_name :email :password)
             (h/from :users)
             (h/where [:= :email email])
             (h/limit 1))
         (query db-spec)
         first))

  (find-user-by-id
    [this id]
    (->clojure-keys (jdbc/get-by-id db-spec :users id)))

  ; Entities
  (create-entity
    [_ entity]
    (insert db-spec :entities entity :name
                                     :user-id
                                     :settings))

  (select-entities
    [_ user-id]
    (query db-spec (-> (h/select :*)
                       (h/from :entities)
                       (h/where [:= :user_id user-id])
                       (h/order-by :name))))

  (find-entity-by-id
    [_ id]
    (->clojure-keys (jdbc/get-by-id db-spec :entities id)))

  (update-entity
    [_ entity]
    (let [sql (sql/format (-> (h/update :entities)
                              (h/sset (->update-set entity
                                                    :name
                                                    :settings))
                              (h/where [:= :id (:id entity)])))]
      (jdbc/execute! db-spec sql)))

  (delete-entity
    [_ id]
    (jdbc/delete! db-spec :entities ["id = ?" id]))

  ; Grants
  (create-grant
    [_ grant]
    (insert db-spec :grants grant :entity-id
                                  :user-id
                                  :permissions))

  (update-grant
    [_ grant]
    (let [sql (sql/format (-> (h/update :grants)
                              (h/sset (->update-set grant
                                                    :permissions))
                              (h/where [:= :id (:id grant)])))]
      (jdbc/execute! db-spec sql)))

  (delete-grant
    [_ id]
    (jdbc/delete! db-spec :grants ["id = ?" id]))

  (select-grants
    [this criteria]
    (.select-grants this criteria {}))

  (select-grants
    [_ criteria options]
    (validate-criteria criteria ::grant-criteria)
    (let [sql (-> (h/select :*)
                  (h/from :grants)
                  (h/where (map->where criteria))
                  (append-limit options))]
      (query db-spec sql)))

  ; Accounts
  (create-account
    [_ account]
    (insert db-spec :accounts account :name
                                      :type
                                      :tags
                                      :commodity-id
                                      :entity-id
                                      :parent-id
                                      :balance))

  (find-account-by-id
    [_ id]
    (->clojure-keys (jdbc/get-by-id db-spec :accounts id)))

  ; TODO Remove this method
  (find-account-by-entity-id-and-name
    [_ entity-id account-name]
    (first (query db-spec (-> (h/select :*)
                       (h/from :accounts)
                       (h/where [:and
                                 [:= :entity_id entity-id]
                                 [:= :name account-name]])
                       (h/limit 1)))))

  (update-account
    [_ account]
    (let [updates (->update-set account :name
                                        :type
                                        :tags
                                        :commodity-id
                                        :parent-id
                                        :balance)]
      (jdbc/update! db-spec :accounts updates ["id = ?" (:id account)])))

  (delete-account
    [_ id]
    (jdbc/delete! db-spec :accounts ["id = ?" id]))

  (select-accounts
    [_ criteria]
    (when-not (some #(% criteria) [:parent-id :entity-id])
      (throw (ex-info
               "The criteria must specify parent-id or entity-id"
               {:criteria criteria})))
    (let [sql (-> (h/select :a.*
                            [:c.name :commodity-name]
                            [:c.symbol :commodity-symbol]
                            [:c.type :commodity-type]
                            [:c.exchange :commodity-exchange]
                            [:e.settings :entity-settings])
                  (h/from [:accounts :a])
                  (h/join [:commodities :c] [:= :c.id :a.commodity-id])
                  (h/merge-join [:entities :e] [:= :e.id :a.entity-id])
                  (h/where (map->where criteria {:prefix "a"})))]
      (query db-spec sql)))

  ; Commodities
  (create-commodity
    [_ commodity]
    (insert db-spec :commodities commodity :name
                                           :type
                                           :symbol
                                           :exchange
                                           :entity-id))

  (find-commodity-by-id
    [_ id]
    (->> (-> (h/select :*)
             (h/from :commodities)
             (h/where [:= :id id])
             (h/limit 1))
        (query db-spec )
        first))

  (update-commodity
    [_ commodity]
    (let [sql (sql/format (-> (h/update :commodities)
                              (h/sset (->update-set commodity
                                                    :entity-id
                                                    :type
                                                    :name
                                                    :symbol
                                                    :exchange))
                              (h/where [:= :id (:id commodity)])))]
      (jdbc/execute! db-spec sql)))

  (select-commodities
    [this criteria]
    (.select-commodities this criteria {}))

  (select-commodities
    [_ criteria options]
    (validate-criteria criteria ::commodity-criteria)
    (query db-spec (-> (h/select :*)
                       (h/from :commodities)
                       (h/where (map->where criteria))
                       (append-paging options))))

  (delete-commodity
    [_ id]
    (jdbc/delete! db-spec :commodities ["id = ?" id]))

  ; Prices
  (create-price
    [_ price]
    (insert db-spec :prices price :commodity-id
                                  :trade-date
                                  :price))

  (select-prices
    [this criteria]
    (.select-prices this criteria {}))

  (select-prices
    [_ criteria options]
    (validate-criteria criteria ::price-criteria)
    (query db-spec (-> (h/select :p.*)
                       (h/from [:prices :p])
                       (h/join [:commodities :c] [:= :c.id :p.commodity_id])
                       (h/where (map->where criteria))
                       (append-prices-as-of options)
                       (append-limit options)
                       (append-paging options))))

  (find-price-by-id
    [_ id]
    (->clojure-keys (jdbc/get-by-id db-spec :prices id)) )

  (update-price
    [_ price]
    (let [sql (sql/format (-> (h/update :prices)
                              (h/sset (->update-set price
                                                    :trade-date
                                                    :price))
                              (h/where [:= :id (:id price)])))]
      (jdbc/execute! db-spec sql)))

  (delete-price
    [_ id]
    (jdbc/delete! db-spec :prices ["id = ?" id]))

  (delete-prices-by-commodity-id
    [_ commodity-id]
    (jdbc/delete! db-spec :prices ["commodity_id = ?" commodity-id]))

  ; Lots
  (create-lot
    [_ lot]
    (insert db-spec :lots lot :commodity-id
                              :account-id
                              :purchase-price
                              :purchase-date
                              :shares-purchased
                              :shares-owned))

  (select-lots-by-entity-id
    [_ entity-id]
    (query db-spec (-> (h/select :*)
                       (h/from [:lots :l])
                       (h/join [:accounts :a] [:= :a.id :l.account_id])
                       (h/where [:= :a.entity_id entity-id]))))

  (select-lots-by-commodity-id
    [_ commodity-id]
    (query db-spec (-> (h/select :*)
                       (h/from :lots)
                       (h/where [:= :commodity_id commodity-id]))))

  (update-lot
    [_ lot]
    (let [sql (sql/format (-> (h/update :lots)
                              (h/sset (->update-set lot
                                                    :purchase-date
                                                    :account-id
                                                    :commodity-id
                                                    :shares-owned
                                                    :shares-purchased))
                              (h/where [:= :id (:id lot)])))]
      (jdbc/execute! db-spec sql)))

  (find-lot-by-id
    [_ id]
    (->clojure-keys (jdbc/get-by-id db-spec :lots id)))

  (select-lots
    [_ criteria]
    (validate-criteria criteria ::lot-criteria)
    (query db-spec (-> (h/select :*)
                       (h/from :lots)
                       (h/where (map->where criteria))
                       (h/merge-where [:!= :shares_owned 0]))))

  (delete-lot
    [_ id]
    (jdbc/delete! db-spec :lots ["id = ?" id]))

  (create-lot->transaction-link
    [_ link]
    (insert db-spec :lots_transactions link :lot-id
                                            :transaction-id
                                            :lot-action
                                            :shares
                                            :price
                                            :action))

  (delete-lot->transaction-link
    [_ lot-id transaction-id]
    (jdbc/delete! db-spec :lots_transactions [:and
                                              [:= :lot_id lot-id]
                                              [:= :transaction_id transaction-id]]))

  (select-lots-transactions-by-transaction-id
    [_ transaction-id]
    (query db-spec (-> (h/select :*)
                       (h/from :lots_transactions)
                       (h/where [:= :transaction_id transaction-id]))))

  ; Transactions
  (select-transactions
    [_ criteria options]
    (validate-criteria criteria ::transaction-criteria)
    (query db-spec (transaction-query :t.* criteria options)))

  (count-transactions
    [_ criteria]
    (validate-criteria criteria ::transaction-criteria)
    (query-scalar db-spec (transaction-query :%count.* criteria {})))

  (create-transaction
    [_ transaction]
    (insert db-spec :transactions transaction :entity-id
                                              :description
                                              :transaction-date
                                              :memo))

  (find-transaction-by-id
    [_ id]
    (->> (-> (h/select :*)
            (h/from :transactions)
            (h/where [:= :id id])
            (h/limit 1))
        (query db-spec )
        first))

  (delete-transaction
    [_ id]
    (jdbc/delete! db-spec :transactions ["id = ?" id]))

  (update-transaction
    [_ transaction]
    (let [sql (sql/format (-> (h/update :transactions)
                              (h/sset (->update-set
                                        transaction
                                        :description
                                        :transaction-date
                                        :memo))
                              (h/where [:= :id (:id transaction)])))]
      (jdbc/execute! db-spec sql)))

  ; Transaction Items
  (create-transaction-item
    [_ transaction-item]
    (insert db-spec :transaction_items transaction-item :transaction-id
                                                        :account-id
                                                        :action
                                                        :amount
                                                        :value
                                                        :index
                                                        :balance
                                                        :memo))

  (select-transaction-items-by-transaction-id
    [_ transaction-id]
    (query db-spec (-> (transaction-item-base-query)
                       (h/where [:= :transaction_id transaction-id])
                       (h/order-by [:action :desc] [:amount :desc]))))

  (select-transaction-items-by-reconciliation-id
    [_ reconciliation-id]
    (query db-spec (-> (transaction-item-base-query)
                       (h/where [:= :reconciliation_id reconciliation-id]))))

  (select-transaction-items-by-account-id
    [this account-id]
    (.select-transaction-items-by-account-id this account-id {}))

  (select-transaction-items-by-account-id
    [_ account-id options]
    (let [sql (-> (transaction-item-base-query)
                  (h/where [:= :i.account_id account-id])
                  (h/order-by [:i.index :desc])
                  (append-paging options))
          sql (cond-> sql
                (contains? options :reconciled?)
                (h/merge-where (if (:reconciled? options)
                                 [:= :r.status "completed"]
                                 [:or [:= :r.status nil]
                                      [:= :r.status "new"]])))]
      (query db-spec sql)))

  (count-transaction-items-by-account-id
    [this account-id]
    (query-scalar db-spec (-> (h/select :%count.*)
                              (h/from :transaction_items)
                              (h/where [:= :account_id account-id]))))

  (select-transaction-items-by-account-id-and-starting-index
    [_ account-id index]
    (query db-spec (-> (h/select :*)
                      (h/from :transaction_items)
                      (h/where [:and
                                [:= :account_id account-id]
                                [:>= :index index]])
                      (h/order-by [:index :desc]))))

  (select-transaction-items-by-account-id-on-or-after-date
    [_ account-id transaction-date]
    (query db-spec (-> (h/select :i.*)
                      (h/from [:transaction_items :i])
                      (h/join [:transactions :t] [:= :t.id :i.transaction-id])
                      (h/where [:and
                                [:= :i.account_id account-id]
                                [:>= :t.transaction_date transaction-date]])
                      (h/order-by :index))))

  (find-transaction-item-by-id
    [_ id]
    (->> (-> (h/select :*)
            (h/from :transaction_items)
            (h/where [:= :id id])
            (h/limit 1))
        (query db-spec)
        first))

  (find-transaction-items-by-ids
    [_ ids]
    (->> (-> (h/select :*)
             (h/from :transaction_items)
             (h/where [:in :id ids]))
        (query db-spec)))

  (select-transaction-items-preceding-date
    [_ account-id transaction-date]
    (query db-spec (-> (h/select :i.*)
                      (h/from [:transaction_items :i])
                      (h/join [:transactions :t] [:= :t.id :i.transaction-id])
                      (h/where [:and
                                [:= :i.account-id account-id]
                                [:< :t.transaction-date transaction-date]])
                      (h/order-by [:t.transaction-date :desc] [:i.index :desc])
                      (h/limit 2))))

  (find-last-transaction-item-on-or-before
    [_ account-id transaction-date]
    (first (query db-spec (-> (h/select :i.*)
                              (h/from [:transaction_items :i])
                              (h/join [:transactions :t] [:= :t.id :i.transaction_id])
                              (h/where [:and
                                        [:= :i.account_id account-id]
                                        [:<= :t.transaction_date transaction-date]])
                              (h/order-by [:t.transaction_date :desc] [:i.index :desc])
                              (h/limit 1)))))

  (update-transaction-item
    [_ transaction-item]
    (let [sql (sql/format (-> (h/update :transaction_items)
                              (h/sset (->update-set transaction-item
                                                    :amount
                                                    :memo
                                                    :action
                                                    :index
                                                    :balance
                                                    :account-id))
                              (h/where [:= :id (:id transaction-item)])))]
      (try
        (jdbc/execute! db-spec sql)
        (catch BatchUpdateException e
          (pprint {:sql sql
                   :batch-update-exception (.getNextException e)})))))

  (update-transaction-item-index-and-balance
    [_ transaction-item]
    (let [sql (sql/format (-> (h/update :transaction_items)
                                (h/sset (->update-set transaction-item
                                                      :index
                                                      :balance))
                                (h/where [:and
                                          [:= :id (:id transaction-item)]
                                          [:or
                                           [:!= :balance (:balance transaction-item)]
                                           [:!= :index (:index transaction-item)]]])))]
        (try
          (jdbc/execute! db-spec sql)
          (catch BatchUpdateException e
            (pprint {:sql sql
                    :batch-update-exception (.getNextException e)})))))

  (delete-transaction-item
    [_ id]
    (jdbc/delete! db-spec :transaction_items ["id = ?" id]))

  (delete-transaction-items-by-transaction-id
    [_ transaction-id]
    (jdbc/delete! db-spec
                  :transaction_items
                  ["transaction_id = ?" transaction-id]))

  (set-transaction-items-reconciled
    [_ reconciliation-id transaction-item-ids]
    (jdbc/execute! db-spec (-> (h/update :transaction_items)
                               (h/sset {:reconciliation_id reconciliation-id})
                               (h/where [:in :id transaction-item-ids])
                               sql/format)))

  (unreconcile-transaction-items-by-reconciliation-id
      [_ reconciliation-id]
      (jdbc/execute! db-spec (-> (h/update :transaction_items)
                                 (h/sset {:reconciliation_id nil})
                                 (h/where [:= :reconciliation-id reconciliation-id])
                                 sql/format)))

  (select-transaction-items
    [_ criteria]
    (query db-spec (-> (h/select :i.* :t.transaction_date :t.description)
                      (h/from [:transaction_items :i])
                      (h/join [:transactions :t] [:= :t.id :i.transaction_id])
                      (h/where (map->where criteria))
                      (h/order-by :t.transaction_date :i.index))))

  ; Reconciliations
  (create-reconciliation
    [_ reconciliation]
    (insert db-spec :reconciliations reconciliation :account-id
                                                    :balance
                                                    :end-of-period
                                                    :status))

  (find-reconciliation-by-id
    [_ id]
    (first (query db-spec (-> (h/select :*)
                              (h/from :reconciliations)
                              (h/where [:= :id id])
                              (h/limit 1)))))

  (select-reconciliations-by-account-id
    [_ account-id]
    (query db-spec (-> (h/select :*)
                       (h/from :reconciliations)
                       (h/where [:= :account-id account-id]))))

  (find-last-reconciliation-by-account-id
    [this account-id]
    (.find-last-reconciliation-by-account-id this account-id nil))

  (find-last-reconciliation-by-account-id
    [_ account-id status]
    (let [sql (-> (h/select :*)
                  (h/from :reconciliations)
                  (h/where [:= :account-id account-id])
                  (h/order-by [:end_of_period :desc])
                  (h/limit 1))]
      (first (query db-spec (cond-> sql
                              status (h/merge-where [:= :status (name status)]))))))

  (find-new-reconciliation-by-account-id
    [_ account-id]
    (first (query db-spec (-> (h/select :*)
                              (h/from :reconciliations)
                              (h/where [:and [:= :status "new"]
                                        [:= :account-id account-id]])
                              (h/limit 1)))))

  (update-reconciliation
    [_ reconciliation]
    (let [sql (sql/format (-> (h/update :reconciliations)
                              (h/sset (->update-set reconciliation
                                                    :account-id
                                                    :balance
                                                    :status
                                                    :end-of-period))
                              (h/where [:= :id (:id reconciliation)])))]
      (jdbc/execute! db-spec sql)))

  (delete-reconciliation
    [_ id]
    (jdbc/delete! db-spec :reconciliations ["id = ?" id]))

  ; Budgets
  (create-budget
    [_ budget]
    (insert db-spec :budgets budget :entity-id
                                    :name
                                    :period
                                    :period-count
                                    :start-date
                                    :end-date))

  (find-budget-by-date
    [_ entity-id date]
    (first (query db-spec (-> (h/select :*)
                              (h/from :budgets)
                              (h/where [:and
                                        [:= :entity-id entity-id]
                                        [:<= :start-date date]
                                        [:>= :end-date date]])
                              (h/limit 1)))))

  (select-budgets
    [this criteria]
    (.select-budgets this criteria {}))

  (select-budgets
    [_ criteria options]
    (validate-criteria criteria ::budget-criteria)
    (query db-spec (-> (h/select :*)
                       (h/from :budgets)
                       (h/where (map->where criteria))
                       (append-limit options))))

  (update-budget
    [_ budget]
    (let [sql (sql/format (-> (h/update :budgets)
                              (h/sset (->update-set budget
                                                    :name
                                                    :period
                                                    :period-count
                                                    :start-date
                                                    :end-date))
                              (h/where [:= :id (:id budget)])))]
      (jdbc/execute! db-spec sql)))

  (delete-budget
    [_ id]
    (jdbc/delete! db-spec :budgets ["id = ?" id]))

  ; Budget items
  (create-budget-item
    [_ budget-item]
    (insert db-spec :budget_items budget-item :budget-id
                                              :account-id
                                              :periods))

  (update-budget-item
    [_ budget-item]
    (let [sql (sql/format (-> (h/update :budget_items)
                              (h/sset (->update-set budget-item
                                                    :account-id
                                                    :periods))
                              (h/where [:= :id (:id budget-item)])))]
      (jdbc/execute! db-spec sql)))

  (find-budget-item-by-id
    [_ id]
    (first (query db-spec (-> (h/select :*)
                              (h/from :budget_items)
                              (h/where [:= :id id])
                              (h/limit 1)))))

  (select-budget-items-by-budget-id
    [_ budget-id]
    (query db-spec (-> (h/select :*)
                       (h/from :budget_items)
                       (h/where [:= :budget_id budget-id]))))

  ; Images

  (create-image
    [_ image]
    (insert db-spec :images image :user-id
                                  :original-filename
                                  :content-type
                                  :body-hash
                                  :body))

  (find-image-by-id
    [this id]
    (->clojure-keys (jdbc/get-by-id db-spec :images id)))

  (select-images
    [this criteria]
    (.select-images this criteria {}))

  (select-images
    [_ criteria options]
    (validate-criteria criteria ::image-criteria)
    (query db-spec (-> (h/select :id :user_id :original_filename :body_hash :created_at)
                        (h/from :images)
                        (h/where (map->where criteria))
                        (append-limit options))))

  (delete-image
    [_ id]
    (jdbc/delete! db-spec :images ["id = ?" id]))

  ; Attachments
  (create-attachment
    [_ attachment]
    (insert db-spec :attachments attachment :transaction-id
                                            :caption
                                            :image-id))

  (select-attachments
    [this criteria]
    (.select-attachments this criteria {}))

  (select-attachments
    [_ criteria options]
    (validate-criteria criteria ::attachment-criteria)
    (query db-spec (-> (h/select :*)
                       (h/from :attachments)
                       (h/where (map->where criteria))
                       (append-limit options))))

  (delete-attachment
    [_ id]
    (jdbc/delete! db-spec :attachments ["id = ?" id]))

  ; Imports

  (create-import
    [_ import]
    (insert db-spec :imports import :entity-name
                                    :user-id
                                    :image-id))

  (find-import-by-id
    [_ id]
    (->clojure-keys (jdbc/get-by-id db-spec :imports id)))

  (update-import
    [_ import]
    (let [sql (sql/format (-> (h/update :imports)
                              (h/sset (->update-set import
                                                    :progress))
                              (h/where [:= :id (:id import)])))]
      (jdbc/execute! db-spec sql)))

  ; Database Transaction
  (with-transaction
    [_ func]
    (jdbc/with-db-transaction [trans db-spec {:isolation :serializable :read-only false}]
      (func (SqlStorage. trans)))))
