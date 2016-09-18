(ns clj-money.test-helpers
  (:require [clojure.java.jdbc :as jdbc]))

(def all-tables ["accounts"
                 "users"])

(defn reset-db
  "Deletes all records from all tables in the database prior to test execution"
  [db-spec f]
  (jdbc/with-db-connection [db db-spec]
    (doseq [table all-tables]
      (jdbc/execute! db (str "truncate table " table ";"))))
  (f))
