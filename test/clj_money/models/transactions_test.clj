(ns clj-money.models.transactions-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clj-factory.core :refer [factory]]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.factories.user-factory]
            [clj-money.factories.entity-factory]
            [clj-money.test-helpers :refer [reset-db
                                            assert-validation-error]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def user (users/create storage-spec (factory :user)))
(def entity (entities/create storage-spec
                             (assoc (factory :entity) :user-id (:id user))))

(def account-defs
  [{:name "Checking"
    :type :asset}
   {:name "Salary"
    :type :income}
   {:name "Groceries"
    :type :expense}])

(defn create-accounts
  "Creates the specified accounts"
  [storage-spec entity defs]
  (->> defs
       (map #(assoc % :entity-id (:id entity)))
       (map #(accounts/create storage-spec %))))

(deftest create-a-transaction
  (let [[checking
         salary
         groceries] (create-accounts storage-spec entity account-defs)
        transaction (transactions/create storage-spec {:transaction-date "2016-03-02"
                                                       :entity-id (:id entity)
                                                       :items [{:account-id (:id checking)
                                                                :action :debit
                                                                :amount (bigdec 1000)}
                                                               {:account-id (:id salary)
                                                                :action :credit
                                                                :amount (bigdec 1000)}]})]
    (is (number? (:id transaction)) "A map with the new ID is returned")))
