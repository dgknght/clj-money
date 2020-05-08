(ns clj-money.inflection-test
  (:require [clojure.test :refer [deftest is]]
            [clj-money.inflection :as inf]))

(deftest humanize-a-keyword
  (let [tests [{:input :test      :expected "Test"}
               {:input :test-word :expected "Test word"}
               {:input :test_word :expected "Test word"}]]
    (doseq [{:keys [input expected]} tests]
      (is (= expected (inf/humanize input))))))

(deftest ordinalize-a-number
  (let [tests [{:input 1  :expected "1st"}
               {:input 2  :expected "2nd"}
               {:input 3  :expected "3rd"}
               {:input 4  :expected "4th"}
               {:input 11 :expected "11th"}
               {:input 12 :expected "12th"}
               {:input 13 :expected "13th"}
               {:input 21 :expected "21st"}
               {:input 42 :expected "42nd"}]]
    (doseq [{:keys [input expected]} tests]
      (is (= expected (inf/ordinal input))))))

(deftest singluraize-a-word
  (let [tests [{:input "words"    :expected "word"}
               {:input "Words"    :expected "Word"}
               {:input "babies"   :expected "baby"}
               {:input "children" :expected "child"}
               {:input "Children" :expected "Child"}
               {:input "CHILDREN" :expected "CHILD"}]]
    (doseq [{:keys [input expected]} tests]
      (is (= expected (inf/singular input))))))

(deftest pluralize-a-word
  (let [tests [{:input "word"  :expected "words"}
               {:input "Word"  :expected "Words"}
               {:input "child" :expected "children"}
               {:input "baby"  :expected "babies"}]]
    (doseq [{:keys [input expected]} tests]
      (is (= expected (inf/plural input))))))

(deftest title-case-a-phrase
  (is (= "My Important Thing" (inf/title-case "my important thing"))))
