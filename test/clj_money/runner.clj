(ns clj-money.runner
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [eftest.runner :refer [find-tests run-tests]]
            [clj-money.config :refer [env]]
            [clj-money.db.sql.tasks :as sql]
            [clj-money.db.sql.partitioning :refer [create-partition-tables]]))

(defn- init-sql-db
  [config]
  (sql/create config :silent true)
  (sql/migrate config)
  (create-partition-tables config
                           (t/local-date 2015 1 1)
                           (t/local-date 2017 12 31)
                           {:silent true}))

(defn- init-sql-dbs []
  (let [config (get-in env [:db :strategies :sql])
        db-count (.availableProcessors (Runtime/getRuntime))
        configs (map (comp #(assoc %
                                   :user (env :sql-adm-user)
                                   :password (env :sql-adm-password))
                           #(update-in config [:dbname] str "_" %))
                     (range 0 db-count))]
    (doseq [c configs]
      (init-sql-db c))))

(defn eftest
  [& args]
  (init-sql-dbs)
  (let [options {:multithread? :namespaces
                 :capture-output? false}]
    (run-tests (find-tests "test") options)))
