(ns clj-money.api.util-test
  (:require #?(:clj [clojure.test :refer [deftest testing is]]
               :cljs [cljs.test :refer [deftest testing is]])
            [clj-money.api.util :as u]))

(deftest add-query-values-to-a-uri
  (testing "no existing query"
    (is (= "/api/transactions?date=2020-01-01&amount=100"
           (u/+query "/api/transactions"
                     {:date "2020-01-01"
                      :amount 100}))))
  (testing "existing query"
    (is (= "/api/transactions?date=2020-01-01&amount=100"
           (u/+query "/api/transactions?date=2020-01-01"
                     {:amount 100})))))
