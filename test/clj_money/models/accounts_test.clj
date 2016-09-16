(ns clj-money.models.accounts-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clj-money.web :refer :all])
  (:use [clj-money.models.accounts :as accounts])
  )

(deftest create-an-account
  (testing "After I add an account, I can retrieve it"
    (accounts/create (env :db) {:name "Checking"
                               :type :asset})
    (is (= [{:name "Checking"
             :type :asset}]
           (accounts/list (env :db))))))
