(ns clj-money.util-test
  (:refer-clojure :exclude [update])
  (:require [clojure.test :refer :all]
            [clj-time.core :as t]
            [clj-money.util :refer [parse-date
                                    format-number]]))

(deftest parse-a-date
  (testing "A US standard date is parsed correctly"
    (is (= (t/local-date 2016 3 2)
           (parse-date "3/2/2016"))
        "The date is parsed correctly"))
  (testing "a universal date is parsed correctly"
    (is (= (t/local-date 2016 3 2)
           (parse-date "2016-03-02"))
        "The date is parsed correctly")))

(deftest format-a-number
  (is (= "1,234.56")
      (format-number (bigdec "1234.56"))))
