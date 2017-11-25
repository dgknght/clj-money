(ns clj-money.models.settings-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clj-time.core :refer [local-date]]
            [clj-money.models.settings :as settings]
            [clj-money.test-helpers :refer [reset-db]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(deftest get-an-unset-value
  (is (nil? (settings/get storage-spec :nothing-here))
      "Nil is returned"))

(deftest save-and-retrieve-a-string
  (settings/put storage-spec :some-string "this is a test")
  (is (= "this is a test" (settings/get storage-spec :some-string))
      "The value is returned correctly."))

(deftest save-and-retrieve-an-integer
  (settings/put storage-spec :some-int 42)
  (is (= 42 (settings/get storage-spec :some-int))
      "The value is returned correctly."))

(deftest save-and-retrieve-a-local-date
  (settings/put storage-spec :some-date (local-date 2017 3 2))
  (is (= (local-date 2017 3 2) (settings/get storage-spec :some-date))
      "The value is returned correctly."))
