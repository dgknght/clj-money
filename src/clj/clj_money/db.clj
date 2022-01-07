(ns clj-money.db
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.core.async :as a :refer [go chan go-loop >! <! <!! buffer]]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clj-time.core :as t]
            [ragtime.jdbc :refer [sql-database
                                  load-resources]]
            [ragtime.repl :as rt]
            [environ.core :refer [env]]
            [clojure.java.jdbc :as jdbc]
            [honeysql.core :as sql]
            [honeysql.helpers :refer [select from where limit]]
            [dgknght.app-lib.web :refer [unserialize-date]]
            [clj-money.core]
            [clj-money.models.settings :as settings]
            [clj-money.partitioning :refer [create-partition-tables]]))

(defn ragtime-config []
  {:datastore (sql-database (env :db))
   :migrations (load-resources "migrations")})

(defn migrate
  []
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

(defn put-partition-date
  [setting-name date compare-fn]
  (when (if-let [existing (settings/get setting-name)]
          (when (compare-fn date existing)
            date)
          date)
    (settings/put setting-name date)))

(defn- put-earliest-partition-date
  [date]
  (put-partition-date "earliest-partition-date"
                      date
                      t/before?))

(defn- put-latest-partition-date
  [date]
  (put-partition-date "latest-partition-date"
                      date
                      t/after?))

(defn create-partitions
  [& args]
  (let [{:keys [arguments options]} (parse-opts args create-partitions-options)
        start-date (-> arguments first unserialize-date)
        end-date (or (-> arguments second unserialize-date)
                     start-date)]
    (create-partition-tables start-date end-date options)
    (put-earliest-partition-date start-date)
    (put-latest-partition-date end-date)))

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
  (jdbc/with-db-connection [db (env :db)]
    (let [query #(jdbc/query db (sql/format %))
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
      (println (format "\nDone. Found %s transaction(s) out of balance." (count out-of-balance))))))

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
