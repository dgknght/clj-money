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
    (is (not (has-error? model)))))

(deftest form-violation-with-schema
  (testing "required attribute"
    (let [model {:size :large :age 30}
          rules [(partial apply-schema TestThing)]
          result (validate-model model rules)]
      (is (has-error? result) "The model should have at least one error")
      (is (= ["Name is required"]
             (get-errors result :name))
          "There should be an error for the missing value."))))

(deftest validate-against-external
  (let [model {:name "John Doe"}
        rules [(fn [m]
                 (when (= "John Doe" (:name m))
                   [[:name "Name must be a real name"]]))]
        result (validate-model model rules)]
    (is (= ["Name must be a real name"]
           (get-errors result :name)))))
