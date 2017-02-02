(ns clj-money.models.commodities-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.validation :as validation]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models.commodities :as commodities]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def ^:private commodity-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}]})

(defn- attributes
  [context]
  {:entity-id (-> context :entities first :id)
   :exchange :nasdaq
   :name "Apple"
   :symbol "APPL"})

(deftest create-a-commodity
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (attributes context)
        result (commodities/create storage-spec commodity)
        commodities (commodities/select-by-entity-id storage-spec entity-id)]
    (is (empty? (validation/error-messages result))
        "The result has no error messages")
    (is (= [{:name "Apple"
             :symbol "APPL"
             :exchange :nasdaq}]
           (map #(select-keys % [:name :symbol :exchange]) commodities))
        "The commodity can be retrieved after create")))

(deftest entity-id-can-be-a-string
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (assoc (attributes context) :entity-id (str entity-id))
        result (commodities/create storage-spec commodity)
        commodities (commodities/select-by-entity-id storage-spec entity-id)]
    (is (empty? (validation/error-messages result))
        "The result has no error messages")
    (is (= [{:name "Apple"
             :symbol "APPL"
             :exchange :nasdaq}]
           (map #(select-keys % [:name :symbol :exchange]) commodities))
        "The commodity can be retrieved after create")))

(deftest entity-id-is-required
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (dissoc (attributes context) :entity-id)
        result (commodities/create storage-spec commodity)
        commodities (commodities/select-by-entity-id storage-spec entity-id)]
    (is (= ["Entity id is required"]
           (validation/error-messages result :entity-id))
        "The result has an error messages")
    (is (empty? (->> commodities
                     (filter #(= "APPL" (:symbol %)))))
        "The commodity is not retrieved after create")))

(deftest name-is-required
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (dissoc (attributes context) :name)
        result (commodities/create storage-spec commodity)
        commodities (commodities/select-by-entity-id storage-spec entity-id)]
    (is (= ["Name is required"]
           (validation/error-messages result :name))
        "The result has an error messages")
    (is (empty? (->> commodities
                     (filter #(= "APPL" (:symbol %)))))
        "The commodity is not retrieved after create")))

(deftest name-is-unique-for-an-entity-and-exchange
  (is false "need to write the test"))

(deftest name-can-be-duplicated-between-exchanges
  (is false "need to write the test"))

(deftest name-can-be-duplicated-between-entities
  (is false "need to write the test"))

(deftest symbol-is-required
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (dissoc (attributes context) :symbol)
        result (commodities/create storage-spec commodity)
        commodities (commodities/select-by-entity-id storage-spec entity-id)]
    (is (= ["Symbol is required"]
           (validation/error-messages result :symbol))
        "The result has an error message")
    (is (empty? (->> commodities
                     (filter #(= "APPL" (:symbol %)))))
        "The commodity is not retrieved after create")))

(deftest symbol-is-unique-for-an-entity-and-exchange
  (is false "need to write the test"))

(deftest symbol-can-be-duplicated-between-exchanges
  (is false "need to write the test"))

(deftest symbol-can-be-duplicated-between-entities
  (is false "need to write the test"))

(deftest exchange-can-be-a-string
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (assoc (attributes context) :entity-id (str entity-id))
        result (commodities/create storage-spec commodity)
        commodities (commodities/select-by-entity-id storage-spec entity-id)]
    (is (empty? (validation/error-messages result))
        "The result has no error messages")
    (is (= [{:name "Apple"
             :symbol "APPL"
             :exchange :nasdaq}]
           (map #(select-keys % [:name :symbol :exchange]) commodities))
        "The commodity can be retrieved after create")))

(deftest exchange-is-required
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (dissoc (attributes context) :exchange)
        result (commodities/create storage-spec commodity)
        commodities (commodities/select-by-entity-id storage-spec entity-id)]
    (is (= ["Symbol is required"]
           (validation/error-messages result :exchange))
        "The result has an error message")
    (is (empty? (->> commodities
                     (filter #(= "APPL" (:symbol %)))))
        "The commodity is not retrieved after create")))

(deftest exchange-must-be-a-valid-exchange
  (testing "it can be :nyse"
    (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (attributes context)
        result (commodities/create storage-spec commodity)
        commodities (commodities/select-by-entity-id storage-spec entity-id)]
    (is (empty? (validation/error-messages result))
        "The result has no error messages")))
  (testing "it can be :nasdaq"
    (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (assoc (attributes context) :exchange :nasdaq)
        result (commodities/create storage-spec commodity)
        commodities (commodities/select-by-entity-id storage-spec entity-id)]
    (is (empty? (validation/error-messages result))
        "The result has no error messages")))
  (testing "it can be :fund"
    (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (assoc (attributes context) :exchange :fund)
        result (commodities/create storage-spec commodity)
        commodities (commodities/select-by-entity-id storage-spec entity-id)]
    (is (empty? (validation/error-messages result))
        "The result has no error messages")))
  (testing "it cannot be anything other than :nyse, :nasdaq, or :fund"
    (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (assoc (attributes context) :exchange :not-a-valid-exchange)
        result (commodities/create storage-spec commodity)
        commodities (commodities/select-by-entity-id storage-spec entity-id)]
    (is (= ["Exchange must be one of: nasdaq, fund, nyse"]
           (validation/error-messages result :exchange))
        "The result has an error messages"))))
