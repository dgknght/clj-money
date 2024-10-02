(ns clj-money.util-test
  (:require #?(:clj [clojure.test :refer [deftest is are]]
               :cljs [cljs.test :refer [deftest is are]])
            #?(:clj [java-time.api :as t]
               :cljs [cljs-time.core :as t])
            [clj-money.util :as util]))

(deftest convert-nominal-comparatives-to-symbolic
  (let [date (t/local-date 2015 1 1)
        other-date (t/local-date 2015 1 31)]
    (is (= {:start-on [:> date]}
           (util/symbolic-comparatives {:start-after date}
                                       :start)))
    (is (= {:transaction-date [:> date]}
           (util/symbolic-comparatives {:transaction-date-after date}
                                       :transaction-date))
        "Keys that end with -date don't receive -on or -at")
    (is (= {:transaction-date [:between date other-date]}
           (util/symbolic-comparatives {:transaction-date-on-or-after date
                                        :transaction-date-on-or-before other-date}
                                       :transaction-date)))
    (is (= {:start-on [:between date other-date]}
           (util/symbolic-comparatives {:start-on-or-after date
                                        :start-on-or-before other-date}
                                       :start-on)))))

(deftest convert-symbolic-comparatives-to-nominal
  (let [date (t/local-date 2015 1 1)
        other-date (t/local-date 2015 1 31)]
    (are [m k expected] (= expected (util/nominal-comparatives m k))
         {:start-on date}
         :start
         {:start-on date}

         {:transaction-date date}
         :transaction-date
         {:transaction-date date}

         {:start-on [:> date]}
         :start
         {:start-after date}

         {:transaction-date [:> date]}
         :transaction-date
         {:transaction-date-after date}

         {:transaction-date [:between date other-date]}
         :transaction-date
         {:transaction-date-on-or-after date
          :transaction-date-on-or-before other-date}

         {:start-on [:between date other-date]}
         :start-on
         {:start-on-or-after date
          :start-on-or-before other-date})))

(deftest get-the-earliest-date
  (let [d1 (t/local-date 2020 3 2)
        d2 (t/local-date 2020 2 27)]
    (is (= d2 (util/earliest d1 d2)))))

(deftest extract-a-qualifier
  (is (= "user" (util/qualifier {:user/name "John"}))
      "A single qualifier is taken directly")
  (is (= "user" (util/qualifier {:id 101
                                 :user/name "John"}))
      "Nil namespaces are ignored")
  (is (thrown-with-msg?
        #?(:clj AssertionError
           :cljs js/Error)
        #"more than one keyword namespace"
        (util/qualifier {:user/name "John"
                         :address/line-1 "1234 Main St"}))))

(deftest qualify-map-keys
  (is (= {:entity/name "Personal"}
         (util/qualify-keys {:name "Personal"} :entity))
      "Unqualified keys are qualified with the model type")
  (is (= {:db/id "x"}
         (util/qualify-keys {:db/id "x"} :entity))
      "Qualified keys are left as-is")
  (is (= {:id 101
          :user/name "John"}
         (util/qualify-keys {:id 101 :name "John"}
                           :user
                           :ignore #{:id}))
      "Keys can be explicitly ignored"))
