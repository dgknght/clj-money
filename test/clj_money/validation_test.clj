(ns clj-money.validation-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clj-money.validation :as validation]))

(defn ^{:clj-money.validation/message "First name is required"
        :clj-money.validation/attribute :first-name}
  first-name-is-present
  [model]
  (:first-name model))

(deftest require-an-attribute
  (testing "success"
    (let [rules [#'first-name-is-present]
          validated (validation/validate rules {:first-name "John"})]
      (is (validation/valid? validated)
          "valid? returns true for the result of a successful validation")))
  (testing "failure"
    (let [rules [#'first-name-is-present]
          validated (validation/validate rules {})]
      (is (not (validation/valid? validated))
          "valid? returns false for the result of a validation failure")
      (is (= ["First name is required"] (validation/error-messages validated :first-name))
          "error-messages returns a list of errors for the specified field when a field is specified")
      (is (= {:first-name ["First name is required"]} (validation/error-messages validated))
          "error-messages returns a map with all error messages when no field is specified"))))
