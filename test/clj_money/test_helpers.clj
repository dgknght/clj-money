(ns clj-money.test-helpers
  (:require [clojure.pprint :refer [pprint]]
            [clojure.java.jdbc :as jdbc]
            [java-time.api :as t]
            [config.core :refer [env]]))

(defn reset-db
  "Deletes all records from all tables in the database prior to test execution"
  [f]
  (jdbc/with-db-connection [db (env :db)]
    (jdbc/execute! db "truncate table cached_prices; truncate table users cascade"))
  (f))

(defmulti ->instant type)

(defmethod ->instant String
  [s]
  (t/instant (t/formatter :iso-instant)
             s))

(defmacro with-fixed-time
  [time & body]
  `(t/with-clock (t/fixed-clock (->instant ~time))
       ~@body))
