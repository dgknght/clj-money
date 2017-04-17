(ns clj-money.coercion-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clj-time.core :as t]
            [clj-money.coercion :as coercion]))

(deftest coerce-an-integer
  (let [rules [(coercion/rule :integer [:id])]]
    (is (= 1 (:id (coercion/coerce rules {:id "1"})))
        "An string is parsed into an integer")
    (is (= 1 (:id (coercion/coerce rules {:id 1})))
        "An integer remains an integer")
    (is (nil? (:id (coercion/coerce rules {:id nil})))
        "A nil remails nil")
    (is (nil? (:id (coercion/coerce rules {})))
        "An omitted value is nil")))

(deftest coerce-a-local-date
  (let [rules [(coercion/rule :local-date [:some-date])]]
    (is (= (t/local-date 2017 3 2)
           (:some-date (coercion/coerce rules {:some-date "2017-03-02"})))
        "It parses an international date string")
    (is (= (t/local-date 2017 3 2)
           (:some-date (coercion/coerce rules {:some-date "3/2/2017"})))
        "It parses an US date string")
    (is (= (t/local-date 2017 3 2)
           (:some-date (coercion/coerce rules {:some-date (t/local-date 2017 3 2)})))
        "It leaves a local date alone")))

(deftest coerce-a-decimal
  (let [rules [(coercion/rule :decimal [:amount])]]
    (is (= 100M
           (:amount (coercion/coerce rules {:amount "100"})))
        "It parses a string")
    (is (= 100M
           (:amount (coercion/coerce rules {:amount 100M})))
        "It leaves a decimal alone")))

(deftest fn-not-found
  (is (thrown? RuntimeException
               (coercion/rule :not-a-valid-key identity))
      "An exception is thrown if the key is not registered"))
