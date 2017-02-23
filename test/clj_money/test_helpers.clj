(ns clj-money.test-helpers
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.java.jdbc :as jdbc]
            [clojure.tools.logging :as log]
            [clj-money.validation :as validation]))

(def all-tables ["reconciliations"
                 "transaction_items"
                 "transactions"
                 "accounts"
                 "budgets"
                 "budget_items"
                 "commodities"
                 "prices"
                 "lots"
                 "entities"
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

(defmacro assert-validation-error
  "Asserts the expected validation error"
  [attribute message & body]
  `(let [result# ~@body
         found-message# (validation/error-messages result# ~attribute)]
     (is (= [~message] found-message#)
         (format "Expected error \"%s\" on %s, but received %s instead"
                 ~message
                 ~attribute
                 found-message#))))

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
       (let [data# (ex-data e#)]
         (is (not (nil? (if (seq ~expected-key)
                          (get-in data# ~expected-key)
                          (get data# ~expected-key))))
             (str "Expected a value at the key "
                  ~expected-key
                  ", but none was found in "
                  data#))))))

(defmacro assert-throws-validation-exception
  "Tests to see if the specified code raises a validation
  exception with the specified violation errors"
  [validation-errors & body]
  `(assert-throws-ex-info {:error ~validation-errors} ~@body))

(defn- simplify-accounts
  [accounts additional-attributes]
  (map #(if (seq (:children %))
          ( -> %
               (select-keys (concat [:name :path :children] additional-attributes))
               (update-in [:children] simplify-accounts additional-attributes))
          (select-keys % [:name :path]))
       accounts))

(defn simplify-account-groups
  "Accept a list of hashes containing :type keyword and :accounts [],
  drill down into each account, filtering out every attribute of the
  account except the name"
  ([groups]
    (simplify-account-groups groups []))
  ([groups additional-attributes]
  (map #(update-in % [:accounts] (fn [accounts]
                                   (simplify-accounts accounts additional-attributes)))
       groups)))

(defn ->budget-item-periods
  [amounts]
  (map-indexed #(hash-map :index %1 :amount %2) amounts))
