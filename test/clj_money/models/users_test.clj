(ns clj-money.models.users-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]])
  (:use [clj-money.models.users :as users]
        [clj-money.test-helpers :refer [reset-db]]))

(def data-store (env :db))

(use-fixtures :each (partial reset-db data-store))

(deftest create-a-user
  (testing "An user can be created with valid attributes"
    (users/create data-store {:first_name "John"
                              :last_name "Doe"
                              :email "john@doe.com"
                              :password "please01"})
    (let [users (->> (users/select data-store)
                     (map #(select-keys % [:first_name
                                           :last_name
                                           :email
                                           :password])))
          expected [{:first_name "John"
                     :last_name "Doe"
                     :email "john@doe.com"}]]
      (is (= expected users)))))
