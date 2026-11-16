(ns clj-money.decimal-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is]])
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            [clj-money.decimal :as d]))

(deftest parse-a-string-of-unspecified-format
  (is (= (d/d 0.5) (d/parse "1/2"))
      "A ratio is parsed into a decimal")
  (is (= (d/d 5000) (d/parse "5,000"))
      "A number formatted with commas is parsed into a decimal")
  (is (= (d/d 1000.0) (d/parse "1,000.0"))
      "A decimal formatted with commas is parsed into a decimal")
  (is (= (d/d 2) (d/parse "1+1"))
      "A basic mathematical expression is calculated")
  (is (= (d/d 6) (d/parse "10-4"))
      "Unspaced subtraction is calculated"))

(deftest identify-a-zero-value
  (is (d/zero? (d/d 0)) "Zero is zero")
  (is (d/zero? (d/d "0.00")) "Zero with a scale is zero")
  (is (not (d/zero? (d/d 0.01))) "A non-zero value is not zero"))
