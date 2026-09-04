(ns clj-money.views.transaction-search-test
  (:require [cljs.test :refer [deftest is]]
            [clj-money.dates :as dates]
            [clj-money.views.transaction-search :as search]))

;; A Wednesday, so "this week" and "last week" span a month boundary too.
(def ^:private ref-date (dates/local-date "2024-06-05"))

(deftest build-criteria-from-filters
  (is (= {:transaction/transaction-date [:between
                                          (dates/local-date "2024-06-05")
                                          (dates/local-date "2024-06-05")]}
         (search/->criteria {:date-range "today"} ref-date))
      "The date range is always included, even with no other filters")
  (is (= {:transaction/transaction-date [:between
                                          (dates/local-date "2024-06-05")
                                          (dates/local-date "2024-06-05")]
          :transaction/description "Kroger"}
         (search/->criteria {:date-range "today" :description "Kroger"} ref-date))
      "A non-blank description is added to the criteria")
  (is (= {:transaction/transaction-date [:between
                                          (dates/local-date "2024-06-05")
                                          (dates/local-date "2024-06-05")]}
         (search/->criteria {:date-range "today" :description ""} ref-date))
      "A blank description is omitted from the criteria"))
