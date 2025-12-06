(ns clj-money.db.datomic.types-test
  (:require [clojure.test :refer [deftest testing is]]
            [java-time.api :as t]
            [clj-money.db.datomic.types :as types]))

(deftest coerce-an-id-value
  (is (= 1 (types/coerce-id "1"))
      "A String is parsed as an integer")
  (is (= 1 (types/coerce-id 1))
      "An integer is returned as-is")
  (is (= [1 2] (types/coerce-id ["1" 2]))
      "A vector is returne with coerc-id applied to the members"))

(deftest convert-a-java-time-date-to-a-java-date
  (is (= #inst "2020-01-01T00:00:00.000Z"
         (types/->java-date (t/local-date 2020 1 1)))
      "A LocalDate is converted to a java Date at UTC midnight"))

(deftest convert-date-values-in-a-map
  (is (= {:transaction/transaction-date #inst "2020-01-01T00:00:00.000Z"
          :transaction/description "Paycheck"}
         (types/->java-dates
           {:transaction/transaction-date (t/local-date 2020 1 1)
            :transaction/description "Paycheck"}))))

(deftest convert-an-entity-for-datomic-storage
  (testing "a referenced entity"
    (is (= {:id 201
            :entity/name "Personal"
            :entity/user 101}
           (types/datomize {:id 201
                            :entity/name "Personal"
                            :entity/user {:id 101
                                          :user/email "john@doe.com"}}
                           {:ref-keys #{:entity/user}}))))
  (testing "a nested map that is not an entity"
    (is (= {:id "temp"
            :commodity/symbol "USD"
            :commodity/type :currency
            :commodity/name "US Dollar"
            :commodity/entity 201
            :commodity/price-config #:price-config{:enabled true}}
           (types/datomize {:id "temp"
                            :commodity/symbol "USD"
                            :commodity/type :currency
                            :commodity/name "US Dollar"
                            :commodity/entity
                            {:id 201
                             :entity/name "Personal"
                             :entity/user {:id 101}}
                            :commodity/price-config #:price-config{:enabled true}}
                           {:ref-keys #{:commodity/entity}}))))
  (testing "a plural attribute"
    (is (= {:id 101
            :import/user 201
            :import/images [301 302]
            :import/name "Personal"}
           (types/datomize {:id 101
                            :import/user {:id 201}
                            :import/images [{:id 301} {:id 302}]
                            :import/name "Personal"}
                           {:ref-keys #{:import/user
                                        :import/images}}))))
  (testing "a criterion with operator"
    (is (= {:budget-item/budget [:in '(101 102)]}
           (types/datomize {:budget-item/budget [:in (list
                                                       {:id 101}
                                                       {:id 102})]}
                           {:ref-keys #{:budget-item/budget}})))))
