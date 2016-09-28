(ns clj-money.validation-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clj-money.validation :refer :all]
            [schema.core :as s]))

(def TestThing
  {:name s/Str
   :age s/Int
   :size (s/enum :small :medium :large)})

(deftest validate-with-schema
  (let [model {:name "John Doe" :size :large :age 30}
        rules [(partial apply-schema TestThing)]
        result (validate-model model rules)]
    (is (not (contains? result :clj-money.validation/errors)))))

(deftest find-form-violation-with-schema
  (testing "required attribute"
    (let [model {:size :large :age 30}
          rules [(partial apply-schema TestThing)]
          result (validate-model {} rules)]
      (is (= (get-in result [:clj-money.validation/errors :name])
             ["Name is required"])
          "There should be an error for the missing value."))))
