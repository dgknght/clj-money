(ns clj-money.db.sql.tasks
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.core.async :as a :refer [go chan go-loop >! <! <!! buffer]]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [java-time.api :as t]
            [ragtime.jdbc :refer [sql-database
                                  load-resources]]
            [ragtime.repl :as rt]
            [ragtime.strategy :refer [apply-new]]
            [clj-money.config :refer [env]]
            [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [honey.sql.helpers :refer [select from where limit]]
            [clj-money.core]
            [clj-money.dates :as dates]
            [clj-money.db.sql.partitioning :refer [create-partition-tables]]))

(defn- sql-config []
  (get-in env [:db :strategies :sql]))

(defn- sql-ddl-config []
  (assoc (sql-config)
         :user (env :sql-ddl-user)
         :password (env :sql-ddl-password)))

(defn- sql-adm-config []
  (assoc (sql-config)
         :user (env :sql-adm-user)
         :password (env :sql-adm-password)))

(defn ragtime-config []
  {:datastore (sql-database (sql-ddl-config))
   :migrations (load-resources "migrations")
   :strategy apply-new})

(defn migrate []
  (rt/migrate (ragtime-config)))

(defn rollback
  []
  (rt/rollback (ragtime-config)))

(defn remigrate []
  (rollback)
  (migrate))

(def ^:private create-partitions-options
  [["-n" "--dry-run" "Dry run"]
   ["-s" "--silent" "Do not output SQL commands"]])

(defn- parse-date
  [date-str]
  (t/local-date (t/formatter :iso-date) date-str))

(defn create-partitions
  [& args]
  (let [{:keys [arguments options]} (parse-opts args create-partitions-options)
        start-date (or (some-> arguments first parse-date)
                       (dates/first-day-of-the-year))
        end-date (or (some-> arguments second parse-date)
                     (dates/last-day-of-the-year start-date))]
    (create-partition-tables start-date end-date options)))

(def ^:private create-role-cmd
  "create role %s with LOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE PASSWORD '%s'")

(def ^:private role-exists-cmd
  "select 1 from pg_roles where rolname = '%s'")

(defn- init-cmds
  [dbname]
  [{:label "ddl user"
    :exists? (format role-exists-cmd (env :sql-ddl-user))
    :create (format create-role-cmd (env :sql-ddl-user) (env :sql-ddl-password))}
   {:label "app user"
    :exists? (format role-exists-cmd (env :sql-app-user))
    :create (format create-role-cmd (env :sql-app-user) (env :sql-app-password))}
   {:label "database"
    :exists? (format "SELECT 1 FROM pg_database WHERE datname = '%s'"
                     dbname)
    :create (format "CREATE DATABASE %s WITH OWNER = %s"
                    dbname
                    (env :sql-ddl-user))}])

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn create
  "Creates the database and users specified in the configuration.

  We assume that by default there is a database with the same
  name as the user. We connect using that database, and then create
  the one that the app is configured to us."
  []
  (let [{:keys [dbname] :as cfg} (sql-adm-config)
        _ (assert (and (:dbtype cfg)
                       (:dbname cfg)
                       (:user cfg)
                       (:password cfg)
                       (:host cfg))
                  "The configuration is not valid.")
        ds (jdbc/get-datasource (assoc cfg :dbname (:user cfg)))]
    (doseq [{:keys [exists? create label]} (init-cmds dbname)]
      (if (empty? (jdbc/execute! ds [exists?]))
        (do
          (println (format "Creating %s..." label))
          (jdbc/execute! ds [create])
          (println "done."))
        (println (format "%s already exists." label))))))

(def ^:private check-transaction-balances-options
  [["-e" "--entity" "The entity for which balances are to be checked"
    :required "The name of the entity for which balances are to be checked"
    :id :entity-name]
   ["-u" "--user" "The user email for which balances are to be checked"
    :required "The email of the user that owns the entity for which balances are to be checked"
    :id :user-email]])

(defn- out-of-balance?
  [transaction query]
  (let [items (query (-> (select :action :value :quantity :account_id)
                         (from :transaction_items)
                         (where [:and [:= :transaction_id (:id transaction)]
                                 [:= :transaction_date (:transaction_date transaction)]])))
        {:keys [credit debit]} (->> items
                                    (map #(update-in % [:action] keyword))
                                    (group-by :action)
                                    (map (fn [entry]
                                           (update-in entry [1] #(->> %
                                                                      (map :value)
                                                                      (reduce + 0M)))))
                                    (into {}))]
    (when (and credit
               debit
               (not= credit debit))
      (assoc transaction
             :items items
             :total-credits credit
             :total-debits debit))))

(defn- resolve-account-fn
  [query]
  (let [accounts (atom {})]
    (fn [{:keys [account_id] :as item}]
      (assoc item :account (or (get-in @accounts [account_id])
                               (let [account (first
                                              (query
                                               (-> (select :*)
                                                   (from :accounts)
                                                   (where [:= :id account_id])
                                                   (limit 1))))]
                                 (swap! accounts assoc account_id account)
                                 account))))))

(defn- write-out-of-balance-file
  [out-of-balance query]
  (let [resolve-account (resolve-account-fn query)]
    (pprint
     (map (fn [trns]
            (-> trns
                (select-keys [:id
                              :transaction_date
                              :description
                              :items
                              :total-credits
                              :total-debits])
                (update-in [:items] (fn [items]
                                      (map (comp #(select-keys % [:account-name
                                                                  :action
                                                                  :quantity
                                                                  :value])
                                                 #(assoc % :account-name (get-in % [:account :name]))
                                                 resolve-account)
                                           items)))))
          out-of-balance)
     (io/writer "./target/out-of-balance.edn"))))

(defn- trace
  [progress f]
  (completing
   (fn [acc trns]
     (go (>! progress (if trns "X" ".")))
     (f acc trns))))

(defn- check-transactions
  [{:keys [user-email entity-name]}]
  {:pre [user-email entity-name]}
  (println "Begin checking transactions...")
  (let [ds (jdbc/get-datasource (sql-config))
        query #(jdbc/execute! ds (sql/format %))
        _ (println "get the entity")
        entity (first
                 (query (-> (select :*)
                            (from :entities)
                            (where [:= :name entity-name])
                            (limit 1))))
        _ (println "get the transactions")
        transactions (query (-> (select :*)
                                (from :transactions)
                                (where [:= :entity_id (:id entity)])))
        progress (chan (buffer 100))
        _ (go-loop []
                   (print (<! progress))
                   (flush)
                   (recur))
        _ (println "check the transactions")
        out-of-balance (<!! (go (->> transactions
                                     (map #(out-of-balance? % query))
                                     (transduce (comp (partial trace progress)
                                                      (filter identity))
                                                conj
                                                []))))]
    (when (seq out-of-balance)
      (write-out-of-balance-file out-of-balance query))
    (println (format "\nDone. Found %s transaction(s) out of balance." (count out-of-balance)))))

(defn check-transaction-balances
  [& args]
  (let [{:keys [options errors summary]} (parse-opts
                                          args
                                          check-transaction-balances-options)]
    (try
      (check-transactions options)
      (catch AssertionError _
        (println "")
        (println "USAGE")
        (println "lein check-trans -ue")
        (when errors
          (println "")
          (println "ERRORS")
          (println errors))
        (println summary)))))
