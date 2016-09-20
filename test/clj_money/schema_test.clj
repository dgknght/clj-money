(ns clj-money.schema-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [schema.core :as s])
  (:use [clj-money.schema]))

(def Thing
  "Test model for validating"
  {:name s/Str
   :id (s/pred (partial re-matches #"[A-Z]\d{2}") "invalid format")
   :size s/Int})

(deftest user-friendly-validation-messages
  (try
    (s/validate Thing {:id "invalid" :size "large"})
    (catch clojure.lang.ExceptionInfo e
      (let [data (ex-data e)
            expected {:name "is required"
                      :id "is not valid"
                      :size "must be a number"}
            actual (user-friendify data)]

        (println "")
        (println "diff")
        (pprint (diff expected actual))

        (is (= expected actual))))))
