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
             (get-errors result))
          "The error is returned with all errors for the model")
      (is (= ["Name is required"]
             (get-errors result :name))
          "The error is returned for the specified attribute of the model"))))

(deftest nested-validation-errors-are-made-friendly
  (let [nested-schema {:name s/Str
                       :age s/Int
                       :children [{:name s/Str
                                   :age s/Int}]}
        model {:name "John Doe"
               :age 40
               :children [{:name "Jill Doe"
                           :age 12}
                          {:name "Jack Doe"}]}
        result (validate-model model [(partial apply-schema nested-schema)])]
    (is (= ["Children 2: Age is required"]
           (get-errors result))
        "The error is included in the full list of errors for the model")
    (is (= ["Children 2: Age is required"]
           (get-errors result :children))
        "The error is included in the list of errors for the parent attribute")
    (is (= ["Age is required"]
           (get-errors result :children 1))
        "The error is included in the list of errors for the specified model part")))

(deftest validate-against-external
  (let [model {:name "John Doe"}
        rules [(fn [model]
                 {:model model
                  :errors (if(= "John Doe" (:name model))
                            [[:name "Name must be a real name"]]
                            [])})]
        result (validate-model model rules)]
    (is (= ["Name must be a real name"]
           (get-errors result :name)))))
