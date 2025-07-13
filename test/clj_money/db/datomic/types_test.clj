(ns clj-money.db.datomic.types-test
  (:require [clojure.test :refer [deftest is]]
            [java-time.api :as t]
            [clj-money.db.datomic.types :as types]))

(deftest coerce-an-id-value
  (is (= 1 (types/coerce-id "1"))
      "A String is parsed as an integer")
  (is (= 1 (types/coerce-id 1))
      "An integer is returned as-is")
  (is (= [1 2] (types/coerce-id ["1" 2]))
      "A vector is returne with coerc-id applied to the members"))

(deftest convert-a-java-time-date-to-a-java-date
  (is (= #inst "2020-01-01T00:00:00.000Z"
         (types/->java-date (t/local-date 2020 1 1)))
      "A LocalDate is converted to a java Date at UTC midnight"))

(deftest convert-date-values-in-a-map
  (is (= {:transaction/transaction-date #inst "2020-01-01T00:00:00.000Z"
          :transaction/description "Paycheck"}
         (types/->java-dates
           {:transaction/transaction-date (t/local-date 2020 1 1)
            :transaction/description "Paycheck"}))))
