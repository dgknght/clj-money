(ns clj-money.models.accounts-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clj-money.web :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]])
  (:use [clj-money.models.accounts :as accounts]))

(deftest create-an-account
  (testing "After I add an account, I can retrieve it"
    (accounts/create (env :db) {:name "Checking"
                               :type :asset})
    (let [accounts (->> (env :db)
                        (accounts/select)
                        (map #(select-keys % [:name :type])))
          expected [{:name "Checking"
                     :type :asset}]]
      (is (= expected
             accounts)))))
