(ns clj-money.models.budgets-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.budgets :as budgets]
            [clj-money.serialization :as serialization]
            [clj-money.test-helpers :refer [reset-db
                                            assert-validation-error]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def create-context
  {:users [{:email "john@doe.com"
            :first-name "John"
            :last-name "Doe"
            :password "please01"}]
   :entities [{:user-id "john@doe.com"
               :name "Personal"}]})

(deftest create-a-budget
  (let [context (serialization/realize storage-spec create-context)
        [entity] (:entities context)
        budget (budgets/create
                 storage-spec
                 {:entity-id (:id entity)
                  :name "2017"
                  :start-date (t/local-date 2017 1 1)
                  :period :month
                  :period-count 12})
        budgets (budgets/select-by-entity-id storage-spec (:id entity))]
    (is (not (nil? (:id budget))) "The returned value has an id")
    (is (= ["2017"] (map :name budgets))
        "A query for budgets returns the new budget")))
