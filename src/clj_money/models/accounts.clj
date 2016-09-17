(ns clj-money.models.accounts
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.pprint :refer [pprint]]
            [honeysql.core :as sql]
            [honeysql.helpers :as h]))

(defn prepare-accounts-for-save
  "Adjusts account data for saving in the database"
  [accounts]
  ; convert account type from keyword to string
  (map #(update-in % [:type] name)
       accounts))

(defn create
  "Creates a new account in the system"
  [data-store account]
  (let [values (prepare-accounts-for-save [account])
        sql (sql/format (-> (h/insert-into :accounts)
                            (h/values values)))]
    (jdbc/execute! data-store sql)))

(defn prepare-account-data-from-db
  "Adjusts account data read from the database for use"
  [accounts]
  ; change account type to a keyword
  (map #(update-in % [:type] keyword)
       accounts))

(defn select
  "Returns a list of all accounts in the system"
  [data-store]
  (let [sql (sql/format (-> (h/select :*)
                            (h/from :accounts)))]
    (->> (jdbc/query data-store sql)
         (prepare-account-data-from-db))))
