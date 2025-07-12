(ns clj-money.comparatives-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
            [clj-money.comparatives :as comparatives]))

(deftest symbolize-a-criteria-map
  (testing "simple keys"
    (is (= {:start-on [:> "2015-01-01"]}
           (comparatives/symbolize {:start-after "2015-01-01"}))
        "-after translates to >. Also keys that don't end with -date and have a local-date value receive the -on suffix")
    (is (= {:transaction-date [:> "2015-01-01"]}
           (comparatives/symbolize {:transaction-date-after "2015-01-01"}))
        "Keys that end with -date don't receive -on or -at")
    (is (= {:start-on [:between "2015-01-01" "2015-02-01"]}
           (comparatives/symbolize {:start-on-or-after "2015-01-01"
                                    :start-on-or-before "2015-02-01"}))
        "-on-or-after and -on-or-before with the same prefix translate to :between"))
  (testing "namespaced keys"
    (is (= {:scheduled-transaction/start-on [:> "2015-01-01"]}
           (comparatives/symbolize
             {:scheduled-transaction/start-after "2015-01-01"}))
        "Keys that don't end with -date and have a local-date value receive the -on suffix")
    (is (= {:transaction/transaction-date [:> "2015-01-01"]}
           (comparatives/symbolize
             {:transaction/transaction-date-after "2015-01-01"}))
        "Namespaced keys that end with -date don't receive -on or -at")
    (is (= {:transaction/transaction-date [:>= "2015-01-01"]}
           (comparatives/symbolize
             {:transaction/transaction-date-on-or-after "2015-01-01"}))
        "-on-or-after translates to >=")
    (is (= {:transaction/transaction-date [:between "2015-01-01" "2015-02-01"]}
           (comparatives/symbolize
             #:transaction{:transaction-date-on-or-after "2015-01-01"
                           :transaction-date-on-or-before "2015-02-01"}))
        "-on-or-after and -on-or-before with the same prefix translate to :between")
    (is (= {:scheduled-transaction/start-on [:between "2015-01-01" "2015-02-01"]}
           (comparatives/symbolize
             #:scheduled-transaction{:start-on-or-after "2015-01-01"
                                     :start-on-or-before "2015-02-01"}))
        "-on-or-after and -on-or-before with the same prefix translate to :between when key ends with -on")
    (is (= {:transaction/transaction-date [:between>
                                           "2015-01-01"
                                           "2015-02-01"]}
           (comparatives/symbolize
             {:transaction/transaction-date-on-or-after "2015-01-01"
              :transaction/transaction-date-before "2015-02-01"}))
        "-on-or-after and -before translates to :between>")
    (is (= {:scheduled-transaction/start-on [:<= "2015-01-01"]
            :scheduled-transaction/end-on [:> "2015-01-01"]}
           (comparatives/symbolize
             {:scheduled-transaction/start-on-or-before "2015-01-01"
              :scheduled-transaction/end-after "2015-01-01"}))
        "Multiple keys can be present in the same map")
    (is (= {:scheduled-transaction/start-date [:<= "2015-01-01"]
            :scheduled-transaction/end-date [:> "2015-01-01"]}
           (comparatives/symbolize
             {:scheduled-transaction/start-date-on-or-before "2015-01-01"
              :scheduled-transaction/end-date-after "2015-01-01"}))
        "Multiple -date keys can be present in the same map")))

(deftest nominalize-a-criteria-map
  (testing "simple keys"
    (is (= {:start-on "2015-01-01"}
           (comparatives/nominalize {:start-on "2015-01-01"}))
        "A scalar value is unchanged when the base has a suffix")
    (is (= {:transaction-date "2015-01-01"}
           (comparatives/nominalize {:transaction-date "2015-01-01"}))
        "A scalar value is unchanged when the base ends with the -date")
    (is (= {:start-after "2015-01-01"}
           (comparatives/nominalize {:start-on [:> "2015-01-01"]}))
        "A greater-than operator translates to the key base with the suffix -after")
    (is (= {:transaction-date-after "2015-01-01"}
           (comparatives/nominalize {:transaction-date [:> "2015-01-01"]}))
        "A greater-than operator translates to the key base with the suffix -after when the base ends with -date")
    (is (= {:start-on-or-after "2015-01-01"}
           (comparatives/nominalize {:start-on [:>= "2015-01-01"]}))
        "A greater-than-or-equal-to operator translates to the key base with the suffix -on-or-after")
    (is (= {:transaction-date-on-or-after "2015-01-01"}
           (comparatives/nominalize {:transaction-date [:>= "2015-01-01"]}))
        "A greater-than-or-equal-to operator translates to the key base with the suffix -on-or-after when the base ends with -date")
    (is (= {:start-before "2015-01-01"}
           (comparatives/nominalize {:start-on [:< "2015-01-01"]}))
        "A less-than operator translates to the key base with the suffix -before")
    (is (= {:transaction-date-before "2015-01-01"}
           (comparatives/nominalize {:transaction-date [:< "2015-01-01"]}))
        "A less-than operator translates to the key base with the suffix -before when the base ends with -date")
    (is (= {:start-on-or-before "2015-01-01"}
           (comparatives/nominalize {:start-on [:<= "2015-01-01"]}))
        "A less-than-or-equal-to operator translates to the key base with the suffix -on-or-before")
    (is (= {:transaction-date-on-or-before "2015-01-01"}
           (comparatives/nominalize {:transaction-date [:<= "2015-01-01"]}))
        "A less-than-or-equal-to operator translates to the key base with the suffix -on-or-before when the base ends with -date"))
  (testing "namespaced keys"
    (is (= {:scheduled-transaction/start-on "2015-01-01"}
           (comparatives/nominalize
             {:scheduled-transaction/start-on "2015-01-01"}))
        "A scalar value is unchanged")
    (is (= {:transaction/transaction-date "2015-01-01"}
           (comparatives/nominalize
             {:transaction/transaction-date "2015-01-01"}))
        "A scalar value is unchanged when the base ends with the -date")
    (is (= {:scheduled-transaction/start-after "2015-01-01"}
           (comparatives/nominalize
             {:scheduled-transaction/start-on [:> "2015-01-01"]}))
        "A greater-than operator translates to the key base with the suffix -after")
    (is (= {:transaction/transaction-date-after "2015-01-01"}
           (comparatives/nominalize
             {:transaction/transaction-date [:> "2015-01-01"]}))
        "A greater-than operator translates to the key base with the suffix -after when the base ends with -date")
    (is (= {:scheduled-transaction/start-on-or-after "2015-01-01"}
           (comparatives/nominalize
             {:scheduled-transaction/start-on [:>= "2015-01-01"]}))
        "A greater-than-or-equal-to operator translates to the key base with the suffix -on-or-after")
    (is (= {:transaction/transaction-date-on-or-after "2015-01-01"}
           (comparatives/nominalize
             {:transaction/transaction-date [:>= "2015-01-01"]}))
        "A greater-than-or-equal-to operator translates to the key base with the suffix -on-or-after when the base ends with -date")
    (is (= {:scheduled-transaction/start-before "2015-01-01"}
           (comparatives/nominalize
             {:scheduled-transaction/start-on [:< "2015-01-01"]}))
        "A less-than operator translates to the key base with the suffix -before")
    (is (= {:transaction/transaction-date-before "2015-01-01"}
           (comparatives/nominalize
             {:transaction/transaction-date [:< "2015-01-01"]}))
        "A less-than operator translates to the key base with the suffix -before when the base ends with -date")
    (is (= {:scheduled-transaction/start-on-or-before "2015-01-01"}
           (comparatives/nominalize
             {:scheduled-transaction/start-on [:<= "2015-01-01"]}))
        "A less-than-or-equal-to operator translates to the key base with the suffix -on-or-before")
    (is (= {:transaction/transaction-date-on-or-before "2015-01-01"}
           (comparatives/nominalize
             {:transaction/transaction-date [:<= "2015-01-01"]}))
        "A less-than-or-equal-to operator translates to the key base with the suffix -on-or-before when the base ends with -date")
    (is (= {:transaction/transaction-date-on-or-after "2015-01-01"
            :transaction/transaction-date-before "2015-02-01"}
           (comparatives/nominalize {:transaction/transaction-date
                                     [:between>
                                      "2015-01-01"
                                      "2015-02-01"]}))
        "A :between> operator translates to two entries with inclusive lower bound and exclusive upper")
    (is (= {:transaction/transaction-date-after "2015-01-01"
            :transaction/transaction-date-on-or-before "2015-02-01"}
           (comparatives/nominalize
             {:transaction/transaction-date [:<between
                                                  "2015-01-01"
                                                  "2015-02-01"]}))
        "A :<between operator translates to two entries with exclusive lower bound and inclusive upper")
    (is (= {:transaction/transaction-date-on-or-after "2015-01-01"
            :transaction/transaction-date-on-or-before "2015-02-01"}
           (comparatives/nominalize
             {:transaction/transaction-date [:between
                                                  "2015-01-01"
                                                  "2015-02-01"]}))
        "A :between operator translates to two entries with inclusive lower bound and inclusive upper")))
