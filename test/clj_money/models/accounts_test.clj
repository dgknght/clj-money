(ns clj-money.models.accounts-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clojure.java.jdbc :as jdbc]
            [clj-factory.core :refer [factory]]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.test-helpers :refer [reset-db]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def user (users/create storage-spec (factory :user)))
(def entity {:id 1} #_(entities/create storage-spec
                             (assoc (factory :entity) :user-id (:id user))))

(deftest select-accounts
  (let [a1 (accounts/create storage-spec {:name "Checking"
                                          :type :asset
                                          :entity-id (:id entity)})
        a2 (accounts/create storage-spec {:name "Credit card"
                                          :type :liability
                                          :entity-id (:id entity)})
        actual (accounts/select-by-entity-id storage-spec (:id entity))
        expected [{:name "Checking"
                   :type :asset}
                  {:name "Credit card"
                   :type :liability}]]
    (is (= expected actual) "It returns the correct accounts")))

(deftest create-an-account
  (testing "After I add an account, I can retrieve it"
    (accounts/create storage-spec {:name "Checking"
                                   :type :asset
                                   :entity-id (:id entity)})
    (let [accounts (->> storage-spec
                        (accounts/select-by-entity-id (:id entity))
                        (map #(select-keys % [:name :type])))
          expected [{:name "Checking"
                     :type :asset}]]
      (is (= expected
             accounts)))))
