(ns clj-money.import-test
  (:refer-clojure :exclude [update])
  (:require [clojure.test :refer :all]
            [clj-time.core :as t]
            [environ.core :refer [env]]
            [clj-factory.core :refer [factory]]
            [clj-money.serialization :as serialization]
            [clj-money.factories.user-factory]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models.entities :as entities]
            [clj-money.reports :as reports]
            [clj-money.import :refer [read-input]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def import-context
  {:users [(factory :user, {:email "john@doe.com"})] })

(def gnucash-sample
  ; TODO referece the file in resources/fixtures
  )

(deftest import-a-simple-file
  (let [context (serialization/realize storage-spec import-context)
        user (-> context :users first)
        _ (import-data storage-spec "Personal" gnucash-sample :gnucash)
        entity (-> storage-spec (entities/select (:id user)) first)
        expected-inc-stmt [{:caption "Income"
                            :value 2000M
                            :style :header}]
        actual-inc-stmt (reports/income-statement storage-spec
                                                  (:id entity)
                                                  (t/local-date 1999 1 1)
                                                  (t/local-date 9999 12 31))
        expected-bal-sheet [{:caption "Assets"
                             :value 2000M
                             :style :header}]
        actual-bal-sheet (reports/balance-sheet storage-spec
                                                (:id entity)
                                                (t/local-date 9999 12 31))]
    (is (= expected-inc-stmt actual-inc-stmt)
        "The income statement is correct after import")
    (is (= expected-bal-sheet actual-bal-sheet)
        "The balance sheet is correct after import")))
