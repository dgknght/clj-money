(ns clj-money.models.entities-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [clj-money.models.entities :as entities]
            [clj-money.models.users :as users]
            [clj-factory.core :refer [factory]])
  (:use [clj-money.test-helpers :refer :all]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def attributes {:name "Personal"})

(deftest create-an-entity
  (let [user (users/create storage-spec (factory :user))
        actual (entities/create storage-spec
                                (assoc attributes :user-id (:id user)))
        expected {:name "Personal"
                  :user-id (:id user)}]
    (testing "The new entity is returned"
      (is (= expected
             (dissoc actual :id)) "The returned map should have the correct content."))))

(deftest select-entities-for-a-user
  (let [user (users/create storage-spec (factory :user))
        other-user (users/create storage-spec (factory :user))
        other-entity (entities/create storage-spec {:name "Other entity"
                                                    :user-id (:id other-user)})
        _ (dorun (map #(entities/create storage-spec {:name %
                                                 :user-id (:id user)})
                 ["Personal"
                  "Business"]))
        actual (entities/select storage-spec (:id user))
        expected [{:name "Business"
                   :user-id (:id user)}
                  {:name "Personal"
                   :user-id (:id user)}]]
    (is (= expected
           (map #(dissoc % :id) actual)) "The returned list should contain the correct items")
    (is (not-any? #(= "Other entity" (:name %)) actual) "The returned list should not contain other users entities")))
