(ns clj-money.web.shared-test
  (:require [clojure.test :refer :all])
  (:use [clj-money.web.shared]))

(deftest humanize-a-keyword
  (let [tests [{:input :test      :expected "Test"}
               {:input :test-word :expected "Test word"}]]
    (doseq [{:keys [input expected]} tests]
      (is (= expected (humanize input))))))
