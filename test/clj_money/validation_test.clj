(ns clj-money.validation-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [clj-money.validation :as validation]))

(s/def ::person (s/keys :req-un [::first-name ::last-name]))

(deftest is-a-model-valid
  (is (nil? (validation/valid? {:first-name "John" :last-name "Doe"}))
      "A map that hasn't been validated cannot be evaluated")
  (is (validation/valid? (validation/validate ::person {:first-name "John" :last-name "Doe"}))
      "A map that has been validated successfully yields true")
  (is (not (validation/valid? (validation/validate ::person {:first-name "John"})))
      "A map that failed validation yields false"))

(deftest get-errors-from-an-invalid-model
  (let [validated (validation/validate ::person {:age 23})]
    (testing "for a specific attribute"
      (is (= ["First name is required"] (validation/error-messages validated :first-name))
          "error-messages returns the errors for the specified attribute"))
    (testing "for the entire model"
      (is (= {:first-name ["First name is required"]
              :last-name ["Last name is required"]}
             (validation/error-messages validated))
          "error-messages returns the errors for the specified attribute"))))
