(ns clj-money.test-helpers
  (:require [clojure.pprint :refer [pprint]]
            [clojure.java.jdbc :as jdbc]
            [ring.mock.request :as req]
            [java-time.api :as t]
            [dgknght.app-lib.test :as test]
            [config.core :refer [env]]))

(defn reset-db
  "Deletes all records from all tables in the database prior to test execution"
  [f]
  (jdbc/with-db-connection [db (env :db)]
    (jdbc/execute! db "truncate table cached_prices; truncate table users cascade"))
  (f))

(defn edn-body
  [req payload]
  (-> req
      (req/content-type "application/edn")
      (req/body (pr-str payload))))

(defn parse-edn-body
  [res]
  (test/parse-edn-body res :readers {'local-date t/local-date
                                     'local-date-time t/local-date-time}))
