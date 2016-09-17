(ns clj-money.models.accounts
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.pprint :refer [pprint]]
            [honeysql.core :as sql]
            [honeysql.helpers :as h]))

(defn create
  "Creates a new account in the system"
  [data-store account]
  (let [sql (sql/format (-> (h/insert-into :accounts)
                            (h/values [(update-in account [:type] name)])))]
    (jdbc/execute! data-store sql)))

(defn select
  "Returns a list of all accounts in the system"
  [data-store]
  (let [sql (sql/format (-> (h/select :*)
                            (h/from :accounts)))]
    (->> (jdbc/query data-store sql)
         (map #(update-in % [:type] keyword)))))
