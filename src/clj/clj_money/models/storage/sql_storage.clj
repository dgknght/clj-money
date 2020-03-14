(ns clj-money.models.storage.sql-storage
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [honeysql.core :as sql]
            [honeysql.helpers :as h]
            [clj-postgresql.types]
            [clj-money.models.storage.sql-helpers :refer [->clojure-keys
                                                          query
                                                          date-range
                                                          select-count
                                                          insert-model
                                                          update-model
                                                          append-sort
                                                          append-paging
                                                          append-limit
                                                          append-where]]
            [clj-money.partitioning :refer [table-name
                                            with-partitioning]]
            [clj-money.models.storage :refer [Storage]])
  (:import java.sql.BatchUpdateException))

(defn- append-select-password
  "For a user query, adds the password to the list of selected
  fields if :include-password? is truthy"
  [sql options]
  (if {:include-password? options}
    (h/merge-select sql :password)
    sql))

(defn- delete
  [db-spec table where]
  (try
    (jdbc/delete! db-spec table where)
    (catch BatchUpdateException e
      (pprint {:delete-from table
               :where where
               :batch-update-exception (.getNextException e)})
      (throw (ex-info (.getMessage (.getNextException e)) {:table table
                                                           :where where})))))

(deftype SqlStorage [db-spec]
  Storage

  ; Users
  (create-user
    [_ user]
    (insert-model db-spec
                  :users
                  user
                  :first-name
                  :last-name
                  :email
                  :password))

  (select-users
    [_ criteria options]
    (query db-spec (-> (h/select :id :first_name :last_name :email :updated_at :created_at)
                       (h/from :users)
                       (append-select-password options)
                       (append-where criteria)
                       (append-limit options))))

  (update-user
    [_ user]
    (update-model db-spec :users user
                  :first-name
                  :last-name
                  :password
                  :password-reset-token
                  :token-expires-at))

  ; Identities
  (create-identity
    [_ ident]
    (insert-model db-spec :identities ident
            :user-id
            :provider
            :provider-id))

  (select-identities
    [_ criteria options]
    (let [sql (-> (h/select :i.*
                            [:u.first_name :user_first_name]
                            [:u.last_name :user_last_name]
                            [:u.email :user_email])
                  (h/from [:identities :i])
                  (h/join [:users :u] [:= :u.id :i.user_id])
                  (append-where criteria {:prefix "i"})
                  (append-limit options))]
      (log/debugf "select-identities %s - %s" (prn-str criteria) (prn-str (sql/format sql)))
      (query db-spec sql)))

  ; Entities
  (create-entity
    [_ entity]
    (insert-model db-spec :entities entity :name
                                     :user-id
                                     :settings))

  (select-entities
    [_ criteria options]
    {:pre [(map? criteria)]}
    (query db-spec (-> (h/select :entities.*)
                       (h/from :entities)
                       (append-where criteria)
                       (h/order-by :name))))

  (find-entity-by-id
    [_ id]
    (->clojure-keys (jdbc/get-by-id db-spec :entities id)))

  (update-entity
    [_ entity]
    (update-model db-spec :entities entity :name :settings))

  (delete-entity
    [_ id]
    (jdbc/delete! db-spec :entities ["id = ?" id]))

  ; Grants
  (create-grant
    [_ grant]
    (insert-model db-spec :grants grant :entity-id
                                  :user-id
                                  :permissions))

  (update-grant
    [_ grant]
    (update-model db-spec :grants grant :permissions))

  (delete-grant
    [_ id]
    (jdbc/delete! db-spec :grants ["id = ?" id]))

  (select-grants
    [_ criteria options]
    (let [sql (-> (h/select :*)
                  (h/from :grants)
                  (append-where criteria)
                  (append-limit options))]
      (query db-spec sql)))

  ; Accounts
  (create-account
    [_ account]
    (insert-model db-spec :accounts account :name
                                      :type
                                      :tags
                                      :commodity-id
                                      :entity-id
                                      :parent-id
                                      :quantity
                                      :value
                                      :earliest-transaction-date
                                      :latest-transaction-date))

  (update-account
    [_ account]
    (update-model db-spec :accounts account
                  :name
                  :type
                  :tags
                  :commodity-id
                  :parent-id
                  :quantity,
                  :value
                  :earliest-transaction-date
                  :latest-transaction-date))

  (delete-account
    [_ id]
    (jdbc/delete! db-spec :accounts ["id = ?" id]))

  (select-accounts
    [_ criteria options]
    (let [sql (-> (h/select :accounts.*
                            [:c.name :commodity-name]
                            [:c.symbol :commodity-symbol]
                            [:c.type :commodity-type]
                            [:c.exchange :commodity-exchange]
                            [:entities.settings :entity-settings])
                  (h/from :accounts)
                  (h/join [:commodities :c] [:= :c.id :accounts.commodity-id]
                          :entities [:= :entities.id :accounts.entity-id])
                  (append-where criteria {:target :account
                                          :prefix :accounts})
                  (append-limit options))]
      (query db-spec sql)))

  ; Commodities
  (create-commodity
    [_ commodity]
    (insert-model db-spec :commodities commodity :name
                                           :type
                                           :symbol
                                           :exchange
                                           :entity-id))

(update-commodity
  [_ commodity]
  (update-model db-spec :commodities commodity
                :entity-id
                :type
                :name
                :symbol
                :exchange))

  (count-commodities
    [_ criteria]
    (query db-spec (-> (h/select :%count.1)
                       (h/from :commodities)
                       (append-where criteria {:target :commodity}))))

  (select-commodities
    [_ criteria options]
    (query db-spec (-> (h/select :*)
                       (h/from :commodities)
                       (append-where criteria {:target :commodity})
                       (append-paging options)
                       (append-limit options))))

  (delete-commodity
    [_ id]
    (jdbc/delete! db-spec :commodities ["id = ?" id]))

  ; Prices
  (create-price
    [_ price]
    (insert-model db-spec
            (table-name (:trade-date price) :prices)
            price
            :commodity-id
            :trade-date
            :price))

  (select-prices
    [this criteria options]
    {:pre [(or (:commodity-id criteria)
               (:id criteria))
           (:trade-date criteria)]}
    (let [d-range (date-range this (:trade-date criteria))
          options (if (:sort options)
                    options
                    (assoc options :sort [[:trade-date :desc]]))]
      (with-partitioning (partial query db-spec) :prices d-range options [table]
        (-> (h/select :prices.*)
            (h/from [table :prices])
            (append-where criteria {:prefix "prices"
                                    :target :price})
            (append-sort options)
            (append-limit options)))))

  (update-price
    [_ {:keys [trade-date] :as price}]
    (update-model db-spec
                  (keyword (table-name trade-date :prices))
                  price
                  :trade-date
                  :price))

  (delete-price
    [_ price]
    (jdbc/delete! db-spec
                  (keyword (table-name (:trade-date price) :prices))
                  ["id = ?" (:id price)]))

  (delete-prices-by-commodity-id
    [_ commodity-id]
    (jdbc/delete! db-spec :prices ["commodity_id = ?" commodity-id]))

  ; Lots
  (create-lot
    [_ lot]
    (insert-model db-spec :lots lot :commodity-id
                              :account-id
                              :purchase-price
                              :purchase-date
                              :shares-purchased
                              :shares-owned))
  (update-lot
    [_ lot]
    (update-model db-spec :lots lot
                  :purchase-date
                  :account-id
                  :commodity-id
                  :purchase-price
                  :shares-owned
                  :shares-purchased))

  (select-lots
    [_ criteria options]
    (query db-spec (-> (h/select :*)
                       (h/from :lots)
                       (append-where criteria {:target :lot})
                       (append-limit options)
                       (append-sort options))))

  (delete-lot
    [_ id]
    (jdbc/delete! db-spec :lots ["id = ?" id]))

  (create-lot->transaction-link
    [_ link]
    (insert-model db-spec :lots_transactions link :lot-id
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
    [this criteria options]
    (let [d-range (date-range this (:transaction-date criteria))
          result
          (with-partitioning (partial query db-spec) :transactions d-range options
            [table]
            (-> (h/select :transactions.*)
                (h/from [table :transactions])
                (select-count options)
                (append-sort options)
                (append-where criteria {:target :transaction})))]
      (if (:count options) ; TODO remove this duplication with select-transaction-items
        (-> result first vals first)
        result)))

  (create-transaction
    [_ transaction]
    (insert-model db-spec
                  (table-name (:transaction-date transaction) :transactions)
                  transaction
                  :entity-id
                  :description
                  :transaction-date
                  :memo
                  :value))

  (delete-transaction
    [_ id transaction-date]
    (jdbc/delete! db-spec (table-name transaction-date :transactions) ["id = ?" id]))

  (update-transaction
    [_ {:keys [transaction-date] :as transaction}]
    (update-model db-spec
                  (table-name transaction-date :transactions)
                  transaction
                  :description
                  :transaction-date
                  :memo
                  :value))

  ; Transaction Items
  (create-transaction-item
    [_ transaction-item]
    (insert-model db-spec
                  (table-name (:transaction-date transaction-item) :transaction_items)
                  transaction-item
                  :transaction-id
                  :transaction-date
                  :account-id
                  :action
                  :quantity
                  :negative
                  :value
                  :index
                  :balance
                  :memo))

  (update-transaction-item
    [_ {:keys [transaction-date] :as transaction-item}]
    (update-model db-spec
                  (table-name transaction-date :transaction_items)
                  transaction-item
                  :transaction-date
                  :quantity
                  :value
                  :negative
                  :memo
                  :action
                  :index
                  :balance
                  :account-id))

  (update-transaction-item-index-and-balance
    [_ transaction-item]
    (update-model db-spec
                  (table-name (:transaction-date transaction-item)
                              :transaction_items)
                  transaction-item
                  [:and
                   [:= :id (:id transaction-item)]
                   [:or
                    [:!= :balance (:balance transaction-item)]
                    [:!= :index (:index transaction-item)]]]
                  :transaction-date
                  :index
                  :balance))

  (delete-transaction-item
    [_ id transaction-date]
    (delete db-spec
            (table-name transaction-date :transaction_items)
            ["id = ?" id]))

  (delete-transaction-items-by-transaction-id
    [_ transaction-id transaction-date]
    (delete db-spec
            (table-name transaction-date :transaction_items)
            ["transaction_id = ?" transaction-id]))

  (set-transaction-item-reconciled
    [_ reconciliation-id transaction-item-id transaction-date]
    (jdbc/execute! db-spec (-> (h/update (table-name transaction-date :transaction_items))
                               (h/sset {:reconciliation_id reconciliation-id})
                               (h/where [:= :id transaction-item-id])
                               sql/format)))

  (unreconcile-transaction-items-by-reconciliation-id
    [_ reconciliation-id date-range]
    (with-partitioning (partial jdbc/execute! db-spec) :transaction_items date-range {} [table]
      (-> (h/update table)
          (h/sset {:reconciliation_id nil})
          (h/where [:= :reconciliation-id reconciliation-id])
          sql/format)))

  (select-transaction-items
    [this criteria options]
    (let [d-range (date-range this (:transaction-date criteria))
          opts (if (:count options)
                options
                (merge
                  {:sort [[:transaction_items.index :desc]]}
                  options))
          result (with-partitioning
                  (partial query db-spec)
                  [:transaction_items :transactions]
                  d-range
                  opts
                  [tables]
                  (-> (h/select :transaction_items.* :transactions.description, [:reconciliations.status :reconciliation_status])
                      (h/from [(first tables) :transaction_items])
                      (h/join [(second tables) :transactions]
                              [:= :transactions.id :transaction_items.transaction_id])
                      (h/left-join :reconciliations [:= :reconciliations.id :transaction_items.reconciliation_id])
                      (select-count opts)
                      (append-where criteria
                                    {:prefix "transaction_items"
                                      :target :transaction-item})
                      (append-sort opts)
                      (append-limit opts)))]
      (if (:count opts)
        (-> result first vals first)
        result)))

  ; Reconciliations
  (create-reconciliation
    [_ reconciliation]
    (insert-model db-spec :reconciliations reconciliation :account-id
                                                    :balance
                                                    :end-of-period
                                                    :status))

  (select-reconciliations
    [_ criteria options]
    (query db-spec (-> (h/select :*)
                       (h/from :reconciliations)
                       (append-where criteria)
                       (append-sort options)
                       (append-limit options))))

  (update-reconciliation
    [_ reconciliation]
    (update-model db-spec :reconciliations reconciliation
                  :account-id
                  :balance
                  :status
                  :end-of-period))

  (delete-reconciliation
    [_ id]
    (jdbc/delete! db-spec :reconciliations ["id = ?" id]))

  ; Budgets
  (create-budget
    [_ budget]
    (insert-model db-spec :budgets budget :entity-id
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
    [_ criteria options]
    (query db-spec (-> (h/select :*)
                       (h/from :budgets)
                       (append-where criteria)
                       (append-limit options))))

  (update-budget
    [_ budget]
    (update-model db-spec :budgets budget
                  :name
                  :period
                  :period-count
                  :start-date
                  :end-date))

  (delete-budget
    [_ id]
    (jdbc/delete! db-spec :budgets ["id = ?" id]))

  ; Budget items
  (create-budget-item
    [_ budget-item]
    (insert-model db-spec :budget_items budget-item :budget-id
                                              :account-id
                                              :periods))

  (update-budget-item
    [_ budget-item]
    (update-model db-spec :budget_items budget-item
                  :account-id
                  :periods))

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

  (delete-budget-item
    [_ id]
    (jdbc/delete! db-spec :budget_items ["id = ?" id]))

  ; Images

  (create-image
    [_ image]
    (insert-model db-spec :images image :user-id
                                  :original-filename
                                  :content-type
                                  :body-hash
                                  :body))

  (find-image-by-id
    [this id]
    (->clojure-keys (jdbc/get-by-id db-spec :images id)))

  (select-images
    [_ criteria options]
    (query db-spec (-> (h/select :id :user_id :original_filename :body_hash :created_at)
                        (h/from :images)
                        (append-where criteria)
                        (append-limit options))))

  (delete-image
    [_ id]
    (jdbc/delete! db-spec :images ["id = ?" id]))

  ; Attachments
  (create-attachment
    [_ attachment]
    (insert-model db-spec :attachments attachment :transaction-id
                                            :transaction-date
                                            :caption
                                            :image-id))

  (select-attachments
    [_ criteria options]
    (query db-spec (-> (h/select :*)
                       (h/from :attachments)
                       (append-where criteria)
                       (append-limit options))))

  (delete-attachment
    [_ id]
    (jdbc/delete! db-spec :attachments ["id = ?" id]))

  ; Imports
  (select-imports
    [_ criteria options]
    (query db-spec (-> (h/select :imports.*)
                   (h/from :imports)
                   (append-where criteria)
                   (append-limit options)
                   (h/order-by :created_at))))

  (create-import
    [_ import]
    (insert-model db-spec :imports import :entity-name
                                    :user-id
                                    :image-ids))

  (find-import-by-id
    [_ id]
    (->clojure-keys (jdbc/get-by-id db-spec :imports id)))

  (update-import
    [_ import]
    (update-model db-spec :imports import :progress))

  (delete-import
    [_ id]
    (jdbc/delete! db-spec :imports ["id = ?" id]))

  ; Settings
  (put-setting
    [_ setting-name setting-value]
    (if (= 1 (->> (query db-spec (-> (h/select :%count.name)
                                     (h/from :settings)
                                     (h/where [:= :name [setting-name]])))
                  first
                  vals
                  first))
      (jdbc/execute! db-spec (-> (h/update :settings)
                                 (h/sset {:value setting-value})
                                 (h/where [:= :name setting-name])
                                 (sql/format)))
      (insert-model db-spec
              :settings
              {:name (name setting-name)
               :value (prn-str setting-value)}
              :name,
              :value)))

  (get-setting
    [this setting-name]
    (.get-setting this setting-name identity))

  (get-setting
    [_ setting-name transform-fn]
    (->> (query db-spec (-> (h/select :value)
                            (h/from :settings)
                            (h/where [:= :name setting-name])
                            (h/limit 1)))
        first
        :value
        transform-fn))

  ; Database Transaction
  (with-transaction
    [_ func]
    (jdbc/with-db-transaction [trans db-spec {:isolation :serializable :read-only false}]
      (func (SqlStorage. trans)))))
