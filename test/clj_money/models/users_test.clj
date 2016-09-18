(ns clj-money.models.users-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]])
  (:use [clj-money.models.users :as users]
        [clj-money.test-helpers :refer [reset-db]]))

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
    (try
      (users/create data-store (dissoc attributes :email))
      (catch clojure.lang.ExceptionInfo e
        (is (= {:email 'missing-required-key}
               (:error (ex-data e)))))))
  (testing "Email must be a valid email address"
    (try
      (users/create data-store (assoc attributes :email "notavalidemail"))
      (is false "The expected exception was not thrown")
      (catch clojure.lang.ExceptionInfo e
        (is (contains? (-> e ex-data :error) :email) "There should be an error for the email"))))
  (testing "First name is required"
    (try
      (users/create data-store (dissoc attributes :first_name))
      (is false "The expected exception was not thrown")
      (catch clojure.lang.ExceptionInfo e
        (is (= {:first_name 'missing-required-key}
               (:error (ex-data e)))))))
  (testing "Last name is required"
    (try
      (users/create data-store (dissoc attributes :last_name))
      (is false "The expected exception was not thrown")
      (catch clojure.lang.ExceptionInfo e
        (is (= {:last_name 'missing-required-key}
               (:error (ex-data e))))))))
