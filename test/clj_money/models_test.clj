(ns clj-money.models-test
  (:require [clojure.test :refer [deftest is]]
            [clj-money.models :as models]))

(deftest tag-a-model
  (is (= ::models/entity (-> {}
                             (models/tag ::models/entity)
                             models/tag))
      "The tag can be retrieved after applying it")
  (is (= ::models/entity (-> {}
                             (models/tag :entity)
                             models/tag))
      "The namespace is applied automatically")
  (is (= ::models/entity (-> {}
                             (models/tag :other-ns/entity)
                             models/tag))
      "An incorrect namespace is dismissed"))
