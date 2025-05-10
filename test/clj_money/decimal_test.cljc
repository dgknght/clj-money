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
  (is (= (d/d 2) (d/parse "1+1"))
      "A basic mathematical expression is calculated"))
