(ns clj-money.db.sql.types-test
  (:require [clojure.test :refer [deftest testing is]]
            [clj-money.entities :as e]
            [clj-money.db.sql.types :as types]))

(deftest serialize-an-id
  (is (= "user:123" (str (types/->QualifiedID 123 :user)))))

(deftest unserialize-an-id
  (is (types/->QualifiedID 123 :user)
      (types/unserialize-id "user:123")))

(deftest qualify-an-id
  (is (= {:id (types/->QualifiedID 123 :user)
          :user/first-name "John"}
         (types/qualify-id {:id 123
                            :user/first-name "John"}))
      "A map is returned with the :id value updated")
  (is (= (types/->QualifiedID 123 :user)
         (types/qualify-id 123 :user))
      "A id with an entity type is returned as QualifiedID")
  (is (nil? (types/qualify-id nil :user))
      "Given nil, nil is returned")
  (testing "higher order function arity"
    (let [f (types/qualify-id :user)]
      (is (= (types/->QualifiedID 123 :user)
             (f 123))
          "The function returns a qualified id given an id")
      (is (= {:id (types/->QualifiedID 123 :user)
              :user/first-name "John"}
             (f {:id 123
                 :user/first-name "John"}))
          "The function returns an entity map with an updated :id value when given an entity map"))))

(deftest decompose-a-qualified-id
  (is (= {:entity-type :user
          :id 123}
         (e/components (types/->QualifiedID 123 :user)))))

(deftest test-for-equalify-between-qualified-ids
  (is (= (types/->QualifiedID 123 :user)
         (types/->QualifiedID 123 :user))))

(deftest convert-an-entity-for-sql-storage
  (testing "a referenced entity"
    (is (= {:id 201
            :entity/name "Personal"
            :entity/user-id 101}
           (types/sqlize {:id (types/->QualifiedID 201 :entity)
                          :entity/name "Personal"
                          :entity/user {:id (types/->QualifiedID 101 :user)
                                        :user/email "john@doe.com"}}
                         {:ref-keys #{:entity/user}}))))
  (testing "a nested map that is not an entity"
    (is (= {:id "temp"
            :commodity/symbol "USD"
            :commodity/type :currency
            :commodity/name "US Dollar"
            :commodity/entity-id 201
            :commodity/price-config #:price-config{:enabled true}}
           (types/sqlize {:id "temp"
                          :commodity/symbol "USD"
                          :commodity/type :currency
                          :commodity/name "US Dollar"
                          :commodity/entity
                          {:id (types/qid 201 :entity)
                           :entity/name "Personal"
                           :entity/user {:id (types/qid 101 :user)}}
                          :commodity/price-config #:price-config{:enabled true}}
                         {:ref-keys #{:commodity/entity}})))))
