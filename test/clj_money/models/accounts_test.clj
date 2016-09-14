(ns clj-money.models.accounts-test
  (:require [clojure.test :refer :all]
            [clj-money.web :refer :all]))

(def db {:host "localhost"})
(deftest create-an-account
  (testing "After I add an account, I can retrieve it"
    (create-account db {:name "Checking"
                        :type :asset})
    (is (= [{:name "Checking"
             :type :asset}]
           (get-accounts db)))))
