(ns clj-money.commodities-test
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing]])
            [clj-money.commodities :as c]))

(deftest search-for-commodities
  (let [commodities [#:commodity{:name "US Dollar"
                                 :symbol "USD"}
                     #:commodity{:name "British Pound"
                                 :symbol "GBP"}
                     #:commodity{:name "Pineapple Co."
                                 :symbol "DOLE"}
                     #:commodity{:name "Apple, Inc."
                                 :symbol "AAPL"}]
        matches [#:commodity{:name "US Dollar"
                             :symbol "USD"}
                 #:commodity{:name "Pineapple Co."
                             :symbol "DOLE"}]]
    (testing "with two arguments"
      (is (= matches
             (c/search "Dol" commodities))
          "A sequence of matching commodities is returned"))
    (testing "with one argument"
      (is (= matches
             ((c/search commodities) "dol"))
          "A function that takes a search term and returns matching commodities is returned"))))
