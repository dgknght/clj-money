(ns clj-money.models.users-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]])
  (:use [clj-money.models.users :as users]
        [clj-money.test-helpers :refer :all]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def attributes {:first-name "John"
                 :last-name "Doe"
                 :email "john@doe.com"
                 :password "please01"})

(deftest create-a-user
  (let [user (users/create storage-spec attributes)]
    (testing "An created user can be retreived"
      (let [users (->> (users/select storage-spec)
                       (map #(select-keys % [:first-name
                                             :last-name
                                             :email
                                             :password])))
            expected [{:first-name "John"
                       :last-name "Doe"
                       :email "john@doe.com"}]]
        (is (= expected users))))
    (testing "It returns a user map"
      (is (number? (:id user)) "The id should be a number")
      (is (= {:first-name "John"
              :last-name "Doe"
              :email "john@doe.com"}
             (dissoc user :id))
          "The map should contain the user properties"))))

(deftest try-to-create-with-invalid-data
  (testing "Email is required"
    (assert-throws-validation-exception
      {:email 'missing-required-key}
      (users/create storage-spec (dissoc attributes :email))) )
  (testing "Email is unique"
    (users/create storage-spec attributes)
    (assert-throws-validation-exception
      {:email :duplicate-key}
      (users/create storage-spec attributes)))
  (testing "Email must be a valid email address"
    (assert-throws-ex-info-with-key
      [:error :email]
      (users/create storage-spec (assoc attributes :email "notavalidemail"))))
  (testing "First name is required"
    (assert-throws-validation-exception
      {:first-name 'missing-required-key}
      (users/create storage-spec (dissoc attributes :first-name))))
  (testing "First name cannot be empty"
    (assert-throws-ex-info-with-key
      [:error :first-name]
      (users/create storage-spec (assoc attributes :first-name ""))))
  (testing "Last name is required"
    (assert-throws-validation-exception
      {:last-name 'missing-required-key}
      (users/create storage-spec (dissoc attributes :last-name))))
  (testing "Last name cannot be empty"
    (assert-throws-ex-info-with-key
      [:error :last-name]
      (users/create storage-spec (assoc attributes :last-name "")))))

(deftest authenticate-a-user
  (let [user (users/create storage-spec attributes)
        actual (users/authenticate storage-spec {:username "john@doe.com"
                                               :password "please01"})
        expected {:identity (:id user)
                  :id (:id user)
                  :email "john@doe.com"
                  :first-name "John"
                  :last-name "Doe"
                  :type :cemerick.friend/auth
                  :roles #{:user}}]
    (is (= expected actual) "The returned value should be the user information")))
