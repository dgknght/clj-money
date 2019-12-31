(ns clj-money.api.lots-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-time.core :as t]
            [clj-money.api.test-helper :refer [deftest-create
                                               deftest-delete
                                               deftest-update
                                               deftest-list
                                               deftest-get-one]]
            [clj-money.serialization :as serialization]
            [clj-money.test-helpers :as h]
            [clj-money.api.lots :as api]))

(def storage-spec (env :db))

(use-fixtures :each (partial h/reset-db storage-spec))

(defn- find-user       [ctx] (h/find-user ctx "john@doe.com"))
(defn- find-other-user [ctx] (h/find-user ctx "jane@doe.com"))

(def ^:private list-context
  {:users [(factory :user {:email "john@doe.com"})
           (factory :user {:email "jane@doe.com"})]
   :entities [{:name "Personal"
               :user-id "john@doe.com"}]
   :commodities [{:name "US Dollar"
                  :entity-id "Personal"
                  :symbol "USD"
                  :type :currency}
                 {:name "Some Fund"
                  :entity-id "Personal"
                  :symbol "FND"
                  :type :fund}]
   :accounts [{:name "IRA"
               :entity-id "Personal"
               :commodity-id "USD"
               :type :asset}
              {:name "Opening Balances"
               :entity-id "Personal"
               :commodity-id "USD"
               :type :equity}]
   :transactions [{:transaction-date (t/local-date 2016 1 1)
                   :entity-id "Personal"
                   :description "Opening Balances"
                   :debit-account-id "IRA"
                   :credit-account-id "Opening Balances"
                   :quantity 1000M}]
   :trades [{:type :purchase
             :entity-id "Personal"
             :trade-date (t/local-date 2016 2 1)
             :account-id "IRA"
             :commodity-id "FND"
             :shares 10M
             :value 50M}
            {:type :purchase
             :entity-id "Personal"
             :trade-date (t/local-date 2016 3 1)
             :account-id "IRA"
             :commodity-id "FND"
             :shares 10M
             :value 60M}
            {:type :sale
             :entity-id "Personal"
             :trade-date (t/local-date 2016 3 15)
             :account-id "IRA"
             :commodity-id "FND"
             :shares 5M
             :value 35M}]})

(deftest-list list-lots
  {:resource-name "lot"
   :context list-context
   :list-fn api/index
   :params-fn (fn [ctx]
                {:account-id (str (:id (h/find-account ctx "IRA")))
                 :commodity-id (str (:id (h/find-commodity ctx "FND")))})
   :expectation-fn (fn [actual]
                     (let [expected [{:purchase-date (t/local-date 2016 2 1)
                                      :shares-purchased 10M
                                      :purchase-price 5M
                                      :shares-owned 5M}
                                     {:purchase-date (t/local-date 2016 3 1)
                                      :shares-purchased 10M
                                      :purchase-price 6M
                                      :shares-owned 10M}]
                           actual (map #(select-keys % [:purchase-date
                                                        :shares-purchased
                                                        :shares-owned
                                                        :purchase-price])
                                       actual)]
                       (h/pprint-diff expected actual)
                       (= expected actual)))})
