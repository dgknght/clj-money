(ns clj-money.models.users-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]])
  (:use [clj-money.models.users :as users]
        [clj-money.test-helpers :refer :all])
  (:import clojure.lang.ExceptionInfo))

(def data-store (env :db))

(use-fixtures :each (partial reset-db data-store))

(def attributes {:first_name "John"
                 :last_name "Doe"
                 :email "john@doe.com"
                 :password "please01"})

(deftest create-a-user
  (let [user (users/create data-store attributes)]
    (testing "An created user can be retreived"
      (let [users (->> (users/select data-store)
                       (map #(select-keys % [:first_name
                                             :last_name
                                             :email
                                             :password])))
            expected [{:first_name "John"
                       :last_name "Doe"
                       :email "john@doe.com"}]]
        (is (= expected users))))
    (testing "It returns a user map"
      (is (number? (:id user)) "The id should be a number")
      (is (= {:first_name "John"
              :last_name "Doe"
              :email "john@doe.com"}
             (dissoc user :id))
          "The map should contain the user properties"))))

(deftest try-to-create-with-invalid-data
  (testing "Email is required"
    (assert-throws-validation-exception
      {:email 'missing-required-key}
      (users/create data-store (dissoc attributes :email))) )
  (testing "Email must be a valid email address"
    (assert-throws-ex-info-with-key
      [:error :email]
      (users/create data-store (assoc attributes :email "notavalidemail"))))
  (testing "First name is required"
    (assert-throws-validation-exception
      {:first_name 'missing-required-key}
      (users/create data-store (dissoc attributes :first_name))))
  (testing "First name cannot be empty"
    (assert-throws-ex-info-with-key
      [:error :first_name]
      (users/create data-store (assoc attributes :first_name ""))))
  (testing "Last name is required"
    (assert-throws-validation-exception
      {:last_name 'missing-required-key}
      (users/create data-store (dissoc attributes :last_name))))
  (testing "Last name cannot be empty"
    (assert-throws-ex-info-with-key
      [:error :last_name]
      (users/create data-store (assoc attributes :last_name "")))))

(deftest authenticate-a-user
  (let [user (users/create data-store attributes)
        actual (users/authenticate data-store {:username "john@doe.com"
                                               :password "please01"})
        expected {:identity (:id user)
                  :id (:id user)
                  :email "john@doe.com"
                  :first_name "John"
                  :last_name "Doe"
                  :type :cemerick.friend/auth
                  :roles #{:user}}]
    (is (= expected actual) "The returned value should be the user information")))
