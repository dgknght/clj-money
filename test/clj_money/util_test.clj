(ns clj-money.util-test
  (:refer-clojure :exclude [update])
  (:require [clojure.test :refer :all]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            [clj-time.core :as t]
            [clj-money.util :refer [parse-local-date
                                    format-date
                                    format-number
                                    keywordify-keys]]))

(deftest format-a-date
  (is (= "3/2/2016" (format-date (t/local-date 2016 3 2)))))

(deftest parse-a-local-date
  (testing "A US standard date is parsed correctly"
    (is (= (t/local-date 2016 3 2)
           (parse-local-date "3/2/2016"))
        "The date is parsed correctly"))
  (testing "a universal date is parsed correctly"
    (is (= (t/local-date 2016 3 2)
           (parse-local-date "2016-03-02"))
        "The date is parsed correctly")))

(deftest format-a-number
  (is (= "1,234.56" (format-number 1234.56M))
      "The default format uses a comma, no currency symbol, and 2 decimal places")
  (is (= "1234.56" (format-number 1234.56 {:format :no-comma}))
      "The :no-comma format uses 2 decimal places, no comma, and no currency symbol"))

(deftest convert-keys-to-keywords
  (let [input {"outer-1" {"inner-1" 1
                          "inner-2" [{"v1" 1}
                                     {"v1" 2}]}}
        output (keywordify-keys input)
        expected {:outer-1 {:inner-1 1
                            :inner-2 [{:v1 1}
                                      {:v1 2}]}}]
    (is (= expected output)
        "The map is returned with keys replaced by keyword equivalents")))
