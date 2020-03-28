(ns clj-money.models.date-helpers-test
  (:require [clojure.test :refer [deftest is]]
            [clj-time.core :as t]
            [clj-money.models.date-helpers :refer [parse-date-range]]))

(deftest parse-input-into-a-date-range
  (is (= [:between (t/local-date 2015 1 1) (t/local-date 2015 12 31)]
         (parse-date-range "2015"))
      "A string year is parsed into the first and last days of the year")
  (is (= [:between (t/local-date 2015 3 1) (t/local-date 2015 3 31)]
         (parse-date-range "2015-03"))
      "A string month is parsed into the first and last days of the month")
  (is (= [:between (t/local-date 2015 3 2) (t/local-date 2015 3 2)]
         (parse-date-range "2015-03-02"))
      "A string date is parsed into a range starting and ending on the specified date")
  (is (= [:between (t/local-date 2015 3 2) (t/local-date 2015 3 2)]
         (parse-date-range (t/local-date 2015 3 2)))
      "A date is converted into a range start and ending on itself")
  (is (= [:between (t/local-date 2015 1 1) (t/local-date 2015 3 2)]
         (parse-date-range [:<= (t/local-date 2015 3 2)]))
      "<= date becomes a range between the earliest partition date and the specified date")
  (is (= [:between (t/local-date 2015 3 2) (t/local-date 2017 12 31)]
         (parse-date-range [:>= (t/local-date 2015 3 2)]))
      ">= date becomes a range between the specified date and the last partition date")
  (is (= [:between (t/local-date 2015 1 1) (t/local-date 2015 3 1)]
         (parse-date-range [:< (t/local-date 2015 3 2)]))
      "< date becomes a range between the earliest partition date and the day before specified date")
  (is (= [:between (t/local-date 2015 3 3) (t/local-date 2017 12 31)]
         (parse-date-range [:> (t/local-date 2015 3 2)]))
      "> date becomes a range between the day after specified date and the last partition date"))
