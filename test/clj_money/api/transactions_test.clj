(ns clj-money.api.transactions-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clojure.set :refer [rename-keys]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-factory.core :refer [factory]]
            [clj-money.api.test-helper :refer [deftest-create
                                               deftest-delete
                                               deftest-update
                                               deftest-list
                                               deftest-get-one]]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.validation :as validation]
            [clj-money.test-helpers :as h]
            [clj-money.api.transactions :as api]
            [clj-money.models.transactions :as transactions]))

(def storage-spec (env :db))

(use-fixtures :each (partial h/reset-db storage-spec))

(def ^:private context
  {:users (->> ["john@doe.com" "jane@doe.com"]
               (mapv #(factory :user {:email %})))
   :entities [{:name "Personal"
               :user-id "john@doe.com"}
              {:name "Business"
               :user-id "jane@doe.com"}]
   :commodities [{:name "US Dollar"
                  :symbol "USD"
                  :type :currency}]
   :accounts [{:name "Checking"
               :type :asset}
              {:name "Salary"
               :type :income}]
   :transactions [{:description "Paycheck"
                   :transaction-date (t/local-date 2016 2 1)
                   :memo "Pre-existing transaction"
                   :items [{:account-id "Checking"
                            :action :debit
                            :quantity 1000M
                            :memo "checking item"}
                           {:account-id "Salary"
                            :action :credit
                            :quantity 1000M
                            :memo "salary item"}]}]})

(defn- find-user        [ctx] (h/find-user ctx "john@doe.com"))
(defn- find-other-user  [ctx] (h/find-user ctx "jane@doe.com"))
(defn- find-entity      [ctx] (h/find-entity ctx "Personal"))
(defn- find-resource    [ctx] (h/find-transaction ctx (t/local-date 2016 2 1) "Paycheck"))
(defn- select-resources [ctx]
  (transactions/search storage-spec {:entity-id (:id (find-entity ctx))} {:include-items? true}))

(deftest-list list-transactions
  {:resource-name "transaction"
   :list-fn api/index
   :params-fn (fn [ctx]
                {:entity-id (-> ctx :entities first :id)
                 :transaction-date [:between
                                    (t/local-date 2016 1 1)
                                    (t/local-date 2016 12 31)]})
   :expectation-fn (fn [actual]
                     (let [expected [{:transaction-date (t/local-date 2016 2 1)
                                      :description "Paycheck"
                                      :memo "Pre-existing transaction"
                                      :value 1000M}]
                           actual (map (fn [a]
                                         (dissoc a :id :entity-id :created-at :updated-at))
                                       actual)]
                       (h/pprint-diff expected actual)
                       (= expected actual)))})

(deftest-get-one get-a-transaction
  {:resource-name "transaction"
   :get-one-fn api/get-one
   :params-fn (fn [ctx]
                (-> ctx
                    :transactions
                    first
                    (update-in [:transaction-date]
                               #(f/unparse-local (:date f/formatters) %))
                    (select-keys [:id :transaction-date])))
   :expectation-fn (fn [actual]
                     (let [expected (-> context :transactions first)]
                       (= "Paycheck" (:description actual))))})

(deftest-create create-a-transaction
  {:resource-name "transaction"
   :create-fn api/create
   :create-params-fn (fn [ctx]
                       {:entity-id (:id (find-entity ctx))
                        :description "Paycheck"
                        :transaction-date (t/local-date 2016 3 2)
                        :memo "Seems like there should be more"
                        :items [{:account-id (:id (h/find-account ctx "Checking"))
                                 :action :debit
                                 :quantity 1000M
                                 :memo "checking item"}
                                {:account-id (:id (h/find-account ctx "Salary"))
                                 :action :credit
                                 :quantity 1000M
                                 :memo "salary item"}]})
   :compare-fn (fn [trx]
                 (let [actual (-> trx
                                  (select-keys [:description
                                                :transaction-date
                                                :memo
                                                :items])
                                  (update-in [:items] (fn [items]
                                                        (map #(select-keys % [:action :quantity :memo])
                                                             items))))
                       expected {:description "Paycheck"
                                 :transaction-date (t/local-date 2016 3 2)
                                 :memo "Seems like there should be more"
                                 :items [{:action :debit
                                          :quantity 1000M
                                          :memo "checking item"}
                                         {:action :credit
                                          :quantity 1000M
                                          :memo "salary item"}]}]
                   (= actual expected)))})

(deftest-update update-a-transaction
  {:resource-name "transaction"
   :find-updated-resource-fn #(transactions/find-by-id storage-spec (:id %) (:transaction-date %))
   :update-fn api/update
   :comparison-fn #(= "updated memo" (:memo %))
   :prepare-update-fn #(-> %
                           (assoc :memo "updated memo")
                           (select-keys [:id :memo :transaction-date])
                           (rename-keys {:transaction-date :original-transaction-date}))})

(deftest-delete delete-a-transaction
  {:resource-name "transaction"
   :delete-fn api/delete
   :delete-keys [:id :transaction-date]})
