(ns clj-money.db-test
  (:require [clojure.test :refer [deftest is]]
            [clj-money.db :as db]))

(deftest assert-test-db-allows-identifiers-that-look-like-test-databases
  (is (nil? (db/assert-test-db! "money_test")))
  (is (nil? (db/assert-test-db! "money_test_0")))
  (is (nil? (db/assert-test-db! "datomic:mem://money_test"))))

(deftest assert-test-db-rejects-identifiers-that-do-not-look-like-test-databases
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"does not appear to be a test database"
                        (db/assert-test-db! "money_development")))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"does not appear to be a test database"
                        (db/assert-test-db! "datomic:sql://money_development?jdbc:postgresql://localhost:5432/datomic"))))
