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
        "The result has no error messages")
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
           (validation/error-messages result :entity-id))
        "The result has no error messages")
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
  (is false "need to write the test"))

(deftest symbol-is-unique-for-an-entity-and-exchange
  (is false "need to write the test"))

(deftest symbol-can-be-duplicated-between-exchanges
  (is false "need to write the test"))

(deftest symbol-can-be-duplicated-between-entities
  (is false "need to write the test"))

(deftest exchange-is-required
  (is false "need to write the test"))

(deftest exchange-must-be-a-valid-exchange
  (testing "it can be :nyse"
    (is false "need to write the test"))
  (testing "it can be :nasdaq"
    (is false "need to write the test"))
  (testing "it can be :fund"
    (is false "need to write the test"))
  (testing "it cannot be anything other than :nyse, :nasdaq, or :fund"
    (is false "need to write the test")))
