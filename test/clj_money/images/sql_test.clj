(ns clj-money.images.sql-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [clj-money.config :refer [env]]
            [clj-money.images :as images]
            [clj-money.images.sql]
            [clj-money.images.storage-contract :as contract])
  (:import [org.postgresql.util PSQLException PSQLState]))

(def ^:private fake-config
  {:clj-money.images/strategy :clj-money.images/sql})

; ---------------------------------------------------------------
; Implementation-specific unit tests (JDBC calls are mocked)
; ---------------------------------------------------------------

(deftest fetch-queries-image-content-table
  (let [calls (atom [])
        content (.getBytes "image data")]
    (with-redefs [jdbc/get-datasource (constantly ::fake-ds)
                  jdbc/execute-one! (fn [_ds sql & _opts]
                                      (swap! calls conj sql)
                                      {:content content})]
      (let [storage (images/reify-storage fake-config)]
        (images/fetch storage "the-uuid")
        (is (= 1 (count @calls))
            "execute-one! called once")
        (let [[sql-str uuid-param] (first @calls)]
          (is (str/includes? sql-str "image_content")
              "queries the image_content table")
          (is (= "the-uuid" uuid-param)
              "parameterizes the uuid"))))))

(deftest fetch-returns-nil-when-row-not-found
  (with-redefs [jdbc/get-datasource (constantly ::fake-ds)
                jdbc/execute-one! (constantly nil)]
    (let [storage (images/reify-storage fake-config)]
      (is (nil? (images/fetch storage "missing"))
          "returns nil when no row exists"))))

(deftest stash-inserts-into-image-content-table
  (let [calls (atom [])
        content (.getBytes "image data")]
    (with-redefs [jdbc/get-datasource (constantly ::fake-ds)
                  jdbc/execute-one! (fn [_ds sql & _opts]
                                      (swap! calls conj sql)
                                      nil)]
      (let [storage (images/reify-storage fake-config)]
        (images/stash storage "the-uuid" content)
        (is (= 1 (count @calls))
            "execute-one! called once")
        (let [[sql-str uuid-param] (first @calls)]
          (is (str/includes? sql-str "image_content")
              "inserts into the image_content table")
          (is (= "the-uuid" uuid-param)
              "parameterizes the uuid"))))))

(deftest stash-falls-back-to-fetch-on-duplicate-key
  (let [existing (.getBytes "original content")
        call-count (atom 0)]
    (with-redefs [jdbc/get-datasource (constantly ::fake-ds)
                  jdbc/execute-one! (fn [_ds _sql & _opts]
                                      (case (swap! call-count inc)
                                        1 (throw (PSQLException.
                                                   "duplicate key value violates unique constraint"
                                                   PSQLState/UNIQUE_VIOLATION))
                                        {:content existing}))]
      (let [storage (images/reify-storage fake-config)]
        (images/stash storage "the-uuid" (.getBytes "new content"))
        (is (= 2 @call-count)
            "a second query is issued to fetch the existing content")))))

(deftest stash-rethrows-non-duplicate-psql-exceptions
  (with-redefs [jdbc/get-datasource (constantly ::fake-ds)
                jdbc/execute-one! (fn [_ds _sql & _opts]
                                    (throw (PSQLException.
                                             "connection refused"
                                             PSQLState/CONNECTION_FAILURE)))]
    (let [storage (images/reify-storage fake-config)]
      (is (thrown? PSQLException
                   (images/stash storage "the-uuid" (.getBytes "data")))
          "rethrows non-duplicate-key PSQLExceptions"))))

; ---------------------------------------------------------------
; Behavioral contract (requires a real PostgreSQL test database)
; ---------------------------------------------------------------

(deftest ^:sql sql-storage-behavioral-contract
  (contract/run-contract (images/reify-storage (:image-storage env))))
