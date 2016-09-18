(ns clj-money.models.accounts-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clojure.java.jdbc :as jdbc])
  (:use [clj-money.models.accounts :as accounts]
        [clj-money.test-helpers :refer [reset-db]]))

(def data-store (env :db))

(use-fixtures :each (partial reset-db data-store))

(deftest create-an-account
  (testing "After I add an account, I can retrieve it"
    (accounts/create data-store {:name "Checking"
                                 :type :asset})
    (let [accounts (->> data-store
                        (accounts/select)
                        (map #(select-keys % [:name :type])))
          expected [{:name "Checking"
                     :type :asset}]]
      (is (= expected
             accounts)))))
