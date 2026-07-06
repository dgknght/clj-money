(ns clj-money.validation-test
  (:require [cljs.test :refer [deftest testing is]]
            [clj-money.validation :as v]))

(deftest extract-validation-errors-from-an-exception
  (testing "Proper validation errors"
    (is (= "First name is required; Last name cannot be more than 100 characters; Last name cannot contain numbers"
           (v/readable
             (ex-info
               "The model is invalid"
               {:user/first-name ["First name is required"]
                :user/last-name ["Last name cannot be more than 100 characters"
                                 "Last name cannot contain numbers"]})))))
  (testing "A ex-info error that doesn't contain validation errors"
    (is (nil? (v/readable (ex-info "Not a validation error" {:one 1})))))
  (testing "A non ex-info error"
    (is (nil? (v/readable (js/Error. "This is a test"))))))
