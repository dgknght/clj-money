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
             user)
          "The map should contain the user properties"))))
