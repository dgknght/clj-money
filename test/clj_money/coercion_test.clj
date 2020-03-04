(ns clj-money.coercion-test
  (:require [clojure.test :refer [deftest is]]
            [clj-time.core :as t]
            [clj-money.coercion :as coercion]))

(deftest coerce-an-integer
  (let [rules [(coercion/rule :integer [:id])]]
    (is (= 1 (:id (coercion/coerce {:id "1"} rules)))
        "An string is parsed into an integer")
    (is (= 1 (:id (coercion/coerce {:id 1} rules)))
        "An integer remains an integer")
    (is (nil? (:id (coercion/coerce {:id nil} rules)))
        "A nil remails nil")
    (is (nil? (:id (coercion/coerce {} rules)))
        "An omitted value is nil")))

(deftest coerce-a-local-date
  (let [rules [(coercion/rule :local-date [:some-date])]]
    (is (= (t/local-date 2017 3 2)
           (:some-date (coercion/coerce {:some-date "2017-03-02"} rules)))
        "It parses an international date string")
    (is (= (t/local-date 2017 3 2)
           (:some-date (coercion/coerce {:some-date "3/2/2017"} rules)))
        "It parses an US date string")
    (is (= (t/local-date 2017 3 2)
           (:some-date (coercion/coerce {:some-date (t/local-date 2017 3 2)} rules)))
        "It leaves a local date alone")))

(deftest coerce-a-decimal
  (let [rules [(coercion/rule :decimal [:amount])]]
    (is (= 100M
           (:amount (coercion/coerce {:amount "100"} rules)))
        "It parses a string")
    (is (= 100M
           (:amount (coercion/coerce {:amount 100M} rules)))
        "It leaves a decimal alone")))

(deftest fn-not-found
  (is (thrown? RuntimeException
               (coercion/rule :not-a-valid-key identity))
      "An exception is thrown if the key is not registered"))
