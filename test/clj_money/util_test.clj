(ns clj-money.util-test
  (:refer-clojure :exclude [update])
  (:require [clojure.test :refer :all]
            [clj-time.core :as t]
            [clj-money.util :refer [parse-date
                                    format-date
                                    format-number]]))

(deftest format-a-date
  (is (= "3/2/2016" (format-date (t/local-date 2016 3 2)))))

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
  (is (= "1,234.56" (format-number 1234.56M))
      "The default format uses a comma, no currency symbol, and 2 decimal places")
  (is (= "1234.56" (format-number 1234.56 {:format :no-comma}))
      "The :no-comma format uses 2 decimal places, no comma, and no currency symbol"))