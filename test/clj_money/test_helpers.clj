(ns clj-money.test-helpers
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.db :as db]))

(defn reset-db
  "Deletes all records from all tables in the database prior to test execution"
  [f]
  (db/reset (db/storage))
  (f))
