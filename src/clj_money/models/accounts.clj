(ns clj-money.models.accounts
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.pprint :as pp]
            [honeysql.core :as sql]
            [honeysql.helpers :refer :all]))

(defn create
  "Creates a new account in the system"
  [data-store account]
  (try
    (let [sql (sql/format (-> (insert-into :accounts)
                              (values [(update-in account [:type] name)])))]

      (pp/pprint (str "sql: " sql))

      (jdbc/execute! data-store sql))
    (catch Exception e
      (println "Unable to insert the account")
      (pp/pprint account)
      (pp/pprint (.getMessage (.getNextException e)))
      [])))

(defn list
  "Returns a list of all accounts in the system"
  [data-store]
  (jdbc/query data-store
              (sql/format (-> (select :*)
                              (from :accounts)))))
