(ns clj-money.inflection-test
  (:require [clojure.test :refer :all])
  (:use [clj-money.inflection]))

(deftest humanize-a-keyword
  (let [tests [{:input :test      :expected "Test"}
               {:input :test-word :expected "Test word"}
               {:input :test_word :expected "Test word"}]]
    (doseq [{:keys [input expected]} tests]
      (is (= expected (humanize input))))))
