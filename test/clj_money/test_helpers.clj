(ns clj-money.test-helpers
  (:require [clojure.pprint :refer [pprint]]
            [clojure.java.jdbc :as jdbc]
            [config.core :refer [env]]))

(defn reset-db
  "Deletes all records from all tables in the database prior to test execution"
  [f]
  (jdbc/with-db-connection [db (env :db)]
    (jdbc/execute! db "truncate table cached_prices; truncate table users cascade"))
  (f))
