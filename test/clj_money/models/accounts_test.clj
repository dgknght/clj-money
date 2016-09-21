(ns clj-money.models.accounts-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clojure.java.jdbc :as jdbc])
  (:use [clj-money.models.accounts :as accounts]
        [clj-money.test-helpers :refer [reset-db]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(deftest create-an-account
  (testing "After I add an account, I can retrieve it"
    (accounts/create storage-spec {:name "Checking"
                                 :type :asset})
    (let [accounts (->> storage-spec
                        (accounts/select)
                        (map #(select-keys % [:name :type])))
          expected [{:name "Checking"
                     :type :asset}]]
      (is (= expected
             accounts)))))