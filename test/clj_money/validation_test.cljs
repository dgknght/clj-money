(ns clj-money.validation-test
  (:require [cljs.test :refer [deftest is]]
            [reagent.core :as r]
            [dgknght.app-lib.forms-validation :as v]
            [clj-money.validation :as val]))

(deftest drop-the-rules-for-hidden-fields
  (let [model (r/atom {})]
    (v/add-rules model [:name] #{::v/required})
    (v/add-rules model [:amount] #{::v/required})
    (val/drop-rules model [[:amount]])
    (v/validate model)
    (is (= {[:name] #{::v/required}}
           (v/get-rules model))
        "The rules for the given fields are removed")
    (is (= "Name is required."
           (v/validation-msg model [:name]))
        "The remaining rules are still applied")))

(deftest drop-no-rules
  (let [model (r/atom {})]
    (v/add-rules model [:name] #{::v/required})
    (val/drop-rules model [])
    (is (= {[:name] #{::v/required}}
           (v/get-rules model))
        "The rules are unchanged")))
