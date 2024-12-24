(ns clj-money.db-test
  (:require [clojure.test :refer [deftest is testing]]
            [clj-money.db :as db]))

(deftest model-typing
  (testing "Retrieved the type of a model"
    (is (= :user (db/model-type {:user/name "John"}))
        "A model type is derived from the keyword namespace")
    (is (= :user (db/model-type {:id 101
                                 :user/name "John"}))
        "A model type is derived from the keyword namespace if a non-namespace keyword is present")
    (is (= :user (db/model-type ^{::db/type :user} {:id 101}))
        "A model type is read from meta data, if present"))
  (testing "Setting the type of a model"
    (is (= :account (db/model-type (db/model-type {} :account)))
        "A model type can be set excplictly")
    (let [f (db/model-type :account)]
      (is (= :account (db/model-type (f {}))))
      "A fn that sets the model type is returned when given a keyword")
    (let [source-model (db/model-type {}  :account)
          target-model (db/model-type {} source-model)]
      (is (= :account (db/model-type target-model)))
      "The model type can be set from another model"))
  (testing "Testing the type of a model"
    (is (db/model-type? {:entity/name "Personal"} :entity))
    (is (not (db/model-type? {:entity/name "Personal"} :account)))
    (let [is-entity? (db/model-type? :entity)]
      (is (is-entity? {:entity/name "Personal"}))
      (is (not (is-entity? {:account/name "Checking"}))))))
