(ns clj-money.db-test
  (:require [clojure.test :refer [deftest is]]
            [integrant.core :as ig]
            [clj-money.config :refer [env]]
            [clj-money.db :as db]
            [clj-money.db.sql]
            [clj-money.db.datomic :as datomic]
            [clj-money.otel-helpers :refer [capture]]
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

(defn- stub-storage
  "Returns a storage that records the calls made to it in the calls atom and
  returns the name of the function called"
  [calls]
  (letfn [(call [f-name & args]
            (swap! calls conj (into [f-name] args))
            f-name)]
    (reify db/Storage
      (put [_ opts entities] (call :put opts entities))
      (find [_ id] (call :find id))
      (find-many [_ ids] (call :find-many ids))
      (select [_ criteria opts] (call :select criteria opts))
      (update [_ changes criteria] (call :update changes criteria))
      (delete [_ entities] (call :delete entities))
      (purge! [_ entity] (call :purge! entity))
      (history [_ entity-id attr] (call :history entity-id attr))
      (close [_] (call :close))
      (reset [_] (call :reset)))))

(deftest trace-calls-to-a-storage
  (let [calls (atom [])
        storage (db/tracing-storage (stub-storage calls) "test")
        account {:id 101 :account/name "Checking"}
        {:keys [result spans]}
        (capture #(vector (db/put storage {} [account])
                          (db/find storage 101)
                          (db/find-many storage [101 102])
                          (db/select storage {:account/name "Checking"} {:limit 1})
                          (db/update storage {:account/name "Savings"} {:id 101})
                          (db/delete storage [account])
                          (db/purge! storage account)
                          (db/history storage 101 :account/name)
                          (db/close storage)))]
    (is (= [:put :find :find-many :select :update :delete :purge! :history :close]
           result)
        "The values returned by the decorated storage are returned")
    (is (= [[:put {} [account]]
            [:find 101]
            [:find-many [101 102]]
            [:select {:account/name "Checking"} {:limit 1}]
            [:update {:account/name "Savings"} {:id 101}]
            [:delete [account]]
            [:purge! account]
            [:history 101 :account/name]
            [:close]]
           @calls)
        "The calls are passed to the decorated storage")
    (is (= ["test/put :account"
            "test/find 101"
            "test/find-many [101 102]"
            "test/select :account"
            "test/update :account"
            "test/delete :account"
            "test/purge! :account"
            "test/history :account/name"
            "test/close"]
           (map :name spans))
        "A span is created for each call")))

(deftest trace-a-reset-of-a-storage
  (let [storage (db/tracing-storage (stub-storage (atom [])) "test")
        {:keys [result spans]} (capture #(db/reset storage))]
    (is (= storage result)
        "The tracing storage is returned")
    (is (= ["test/reset"] (map :name spans))
        "A span is created for the reset")))

(deftest trace-a-failed-call-to-a-storage
  (let [storage (db/tracing-storage (reify db/Storage
                                      (find [_ _id]
                                        (throw (ex-info "Not available" {}))))
                                    "test")
        {:keys [exception] [span] :spans} (capture #(db/find storage 101))]
    (is (= "Not available" (ex-message exception))
        "The exception is rethrown")
    (is (= [:error "Not available"]
           ((juxt :status :status-description) span))
        "The span records the error")))
