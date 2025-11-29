(ns clj-money.db.sql.types-test
  (:require [clojure.test :refer [deftest testing is]]
            [clj-money.entities :as e]
            [clj-money.db.sql.types :as types]))

(deftest serialize-an-id
  (is (= "123:user" (str (types/qid 123 :user)))))

(deftest unserialize-an-id
  (is (types/qid 123 :user)
      (types/unserialize-qid "123:user")))

; TODO: Do we need both of these?
(deftest parse-a-qualified-id
  (is (types/qid 1 :user)
      (types/parse-qid (str (types/qid 1 :user)))))

(deftest qualify-an-id
  (is (= {:id (types/qid 123 :user)
          :user/first-name "John"}
         (types/qualify-id {:id 123
                            :user/first-name "John"}))
      "A map is returned with the :id value updated")
  (is (= (types/qid 123 :user)
         (types/qualify-id 123 :user))
      "A id with an entity type is returned as QualifiedID")
  (is (nil? (types/qualify-id nil :user))
      "Given nil, nil is returned")
  (testing "higher order function arity"
    (let [f (types/qualify-id :user)]
      (is (= (types/qid 123 :user)
             (f 123))
          "The function returns a qualified id given an id")
      (is (= {:id (types/qid 123 :user)
              :user/first-name "John"}
             (f {:id 123
                 :user/first-name "John"}))
          "The function returns an entity map with an updated :id value when given an entity map"))))

(deftest decompose-a-qualified-id
  (is (= {:entity-type :user
          :id 123}
         (e/components (types/qid 123 :user)))))

(deftest test-for-equalify-between-qualified-ids
  (is (= (types/qid 123 :user)
         (types/qid 123 :user))))

(deftest convert-an-entity-for-sql-storage
  (testing "a referenced entity"
    (is (= {:id 201
            :entity/name "Personal"
            :entity/user-id 101}
           (types/sqlize {:id (types/qid 201 :entity)
                          :entity/name "Personal"
                          :entity/user {:id (types/qid 101 :user)
                                        :user/email "john@doe.com"}}
                         {:ref-keys #{:entity/user}}))))
  (testing "a nested map that is not an entity"
    (is (= {:id "temp"
            :commodity/symbol "USD"
            :commodity/type "currency"
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
                         {:ref-keys #{:commodity/entity}}))))
  (testing "a plural attribute"
    (is (= {:id 101
            :import/user-id 201
            :import/image-ids [301 302]
            :import/name "Personal"}
           (types/sqlize {:id (types/qid 101 :import)
                          :import/user {:id (types/qid 201 :user)}
                          :import/images [{:id (types/qid 301 :image)}
                                          {:id (types/qid 302 :image)}]
                          :import/name "Personal"}
                         {:ref-keys #{:import/user
                                      [:import/images [:image]]}}))))
  (testing "a criterion with operator"
    (is (= {:budget-item/budget-id [:in '(101 102)]}
           (types/sqlize {:budget-item/budget [:in (list
                                                     {:id (types/qid 101 :budget)}
                                                     {:id (types/qid 102 :budget)})]}
                         {:ref-keys #{:budget-item/budget}})))))

(deftest convert-an-entity-from-sql-storage
  (testing "a referenced entity"
    (is (= {:id (types/qid 201 :entity)
            :entity/name "Personal"
            :entity/user {:id (types/qid 101 :user)}}
           (types/generalize {:id 201
                              :entity/name "Personal"
                              :entity/user-id 101}
                             {:sql-ref-keys #{:entity/user-id}}))))
  (testing "a referenced entity with attribute/type mismatch"
    (testing "with implicit reference type"
      (is (= {:id (types/qid 402 :account)
              :account/name "Car",
              :account/type :asset,
              :account/commodity {:id (types/qid 301 :commodity)},
              :account/parent {:id (types/qid 401 :account)},
              :account/entity {:id (types/qid 101 :entity)}}
             (types/generalize {:id 402
                                :account/name "Car",
                                :account/type :asset,
                                :account/commodity-id 301,
                                :account/parent-id 401,
                                :account/entity-id 101}
                               {:sql-ref-keys #{[:account/parent-id :account]
                                                :account/entity-id
                                                :account/commodity-id}}))))
    (testing "with explicit reference type"
      (is (= {:id (types/qid 402 :account)
              :account/name "Car",
              :account/type :asset,
              :account/commodity {:id (types/qid 301 :commodity)},
              :account/parent {:id (types/qid 401 :account)},
              :account/entity {:id (types/qid 101 :entity)}}
             (types/generalize {:id 402
                                :account/name "Car",
                                :account/type :asset,
                                :account/commodity-id 301,
                                :account/parent-id 401,
                                :account/entity-id 101}
                               {:sql-ref-keys {:account/parent-id :account
                                               :account/entity-id :entity
                                               :account/commodity-id :commodity}})))))
  (testing "an id attribute that is not a local reference"
    (is (= {:id (types/qid 101 :identity)
            :identity/user {:id (types/qid 201 :user)}
            :identity/provider-id "abc123"}
           (types/generalize {:id 101
                              :identity/user-id 201
                              :identity/provider-id "abc123"}
                             {:sql-ref-keys #{:identity/user-id}}))))
  (testing "A list of references"
    (is (= {:id (types/qid 101 :import)
            :import/user {:id (types/qid 201 :user)}
            :import/entity-name "Personal"
            :import/images [{:id (types/qid 301 :image)}
                            {:id (types/qid 302 :image)}]}
           (types/generalize {:id 101
                              :import/user-id 201
                              :import/entity-name "Personal"
                              :import/image-ids [301 302]}
                             {:sql-ref-keys #{:import/user-id
                                              [:import/image-ids :image]}})))))
