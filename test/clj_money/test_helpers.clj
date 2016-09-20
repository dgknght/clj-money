(ns clj-money.test-helpers
  (:require [clojure.test :refer :all]
            [clojure.java.jdbc :as jdbc]
            [clojure.tools.logging :as log]))

(def all-tables ["accounts"
                 "users"])

(defn reset-db
  "Deletes all records from all tables in the database prior to test execution"
  [db-spec f]
  (jdbc/with-db-connection [db db-spec]
    (doseq [table all-tables]
      (jdbc/execute! db (str "truncate table " table ";"))))
  (f))

(defn subset?
  "Accepts two maps, the first of which may or may not
  be a subset of the second. Returns true if the first
  is a subset of the second, false if not"
  [smaller-map larger-map]
  (= (select-keys larger-map (keys smaller-map))
     smaller-map))

(defmacro assert-throws-ex-info
  "Tests to see if the specified code raises ExceptionInfo
  containing the specified data"
  [expected-data & body]
  `(try
     ~@body
     (is false "An exception was expected, but none was thrown.")
     (catch clojure.lang.ExceptionInfo e#
       (is (subset? ~expected-data (ex-data e#))
           (str "Expected ex-data to contain " ~expected-data)))))

(defmacro assert-throws-ex-info-with-key
  "Tests to see if the specified code raises ExceptionInfo
  containing a non-nil value at the specified key. The expected
  key can be a sequence for nested data structures."
  [expected-key & body]
  `(try
     ~@body
     (is false "An exception was expected, but none was thrown.")
     (catch clojure.lang.ExceptionInfo e#
       (is (not (nil? (if (seq ~expected-key)
                        (get-in (ex-data e#) ~expected-key)
                        (get (ex-data e#) ~expected-key))))
           (str "Expected a value at the key "
                ~expected-key
                ", but none was found.")))))

(defmacro assert-throws-validation-exception
  "Tests to see if the specified code raises a schema validation
  exception with the specified violation errors"
  [validation-errors & body]
  `(assert-throws-ex-info {:error ~validation-errors} ~@body))
