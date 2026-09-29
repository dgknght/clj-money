(ns clj-money.db-test
  (:require [clojure.test :refer [deftest is]]
            [integrant.core :as ig]
            [clj-money.config :refer [env]]
            [clj-money.db :as db]
            [clj-money.db.sql]
            [clj-money.db.datomic :as datomic]
            [clj-money.spies :as spy]))

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

(deftest get-the-active-storage-config
  (is (= {:uri "datomic:mem://money_test"}
         (db/active-config {:db {:strategies {:sql {:dbname "money_test"}
                                              :datomic {:uri "datomic:mem://money_test"}}
                                 :active :datomic}}))))

(deftest reuse-the-default-storage-when-none-is-bound
  (is (identical? (db/storage) (db/storage))
      "The same storage instance is returned on each call"))

(deftest prefer-the-bound-storage
  (let [storage (db/reify-storage (get-in env [:db :strategies :datomic-peer]))]
    (db/with-storage [storage]
      (is (identical? storage (db/storage))))))

(deftest initialize-and-halt-a-storage-component
  (let [storage (ig/init-key ::db/storage
                             (get-in env [:db :strategies :datomic-peer]))]
    (is (satisfies? db/Storage storage)
        "init-key returns a storage instance")
    (let [spy (spy/storage-spy storage)]
      (ig/halt-key! ::db/storage spy)
      (is (= [[]] (spy/calls spy :close))
          "halt-key! closes the storage"))))

(deftest closing-sql-storage-closes-the-connection-pool
  (let [storage (db/reify-storage (get-in env [:db :strategies :sql]))]
    (is (seq? (db/select storage {:user/email "nobody@example.com"} {}))
        "The storage is usable before it is closed")
    (db/close storage)
    (is (thrown-with-msg? java.sql.SQLException
                          #"has been closed"
                          (db/select storage {:user/email "nobody@example.com"} {}))
        "The storage cannot be used after it is closed")))

(deftest query-the-bound-datomic-storage
  (db/with-storage [(get-in env [:db :strategies :datomic-peer])]
    (db/reset (db/storage))
    (is (= 1 (count (datomic/q '[:find ?e :where [?e :db/ident :user/email]])))
        "The query is executed using the bound storage's API"))
  (db/with-storage [(db/reify-storage (get-in env [:db :strategies :sql]))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"not a Datomic storage"
                          (datomic/q '[:find ?e :where [?e :user/email]]))
        "An exception is thrown if the bound storage isn't a Datomic storage")))
