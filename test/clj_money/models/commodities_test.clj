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
   :entities [{:name "Personal"}
              {:name "Business"}]})

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
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (attributes context) 
        result-1 (commodities/create storage-spec commodity)
        result-2 (commodities/create storage-spec commodity)
        commodities (commodities/select-by-entity-id storage-spec entity-id)]
    (is (empty? (validation/error-messages result-1))
        "The first is created successfully")
    (is (= ["Name must be unique for a given exchange"]
           (validation/error-messages result-2 :name))
        "The result has an error messages")
    (is (= 1 (->> commodities
                  (filter #(= "Apple" (:name %)))
                  count))
        "The commodity exists only once after both calls to create")))

(deftest name-can-be-duplicated-between-exchanges
  (let [context (serialization/realize storage-spec commodity-context)
        entity-1-id (-> context :entities first :id)
        entity-2-id (-> context :entities second :id)
        commodity-1 (assoc (attributes context) :entity-id entity-1-id)
        commodity-2 (assoc (attributes context) :entity-id entity-2-id)
        result-1 (commodities/create storage-spec commodity-1)
        result-2 (commodities/create storage-spec commodity-2)
        commodities-1 (commodities/select-by-entity-id storage-spec entity-1-id)
        commodities-2 (commodities/select-by-entity-id storage-spec entity-2-id)]
    (is (empty? (validation/error-messages result-1))
        "The first is created successfully")
    (is (empty? (validation/error-messages result-2))
        "The second is created successfully")
    (is (->> commodities-1
             (filter #(= "Apple" (:name %)))
             first)
        "The first commodity can be retrieved after create")
    (is (->> commodities-2
             (filter #(= "Apple" (:name %)))
             first)
        "The second commodity can be retrieved after create")))

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
    (is (= ["Exchange is required"]
           (validation/error-messages result :exchange))
        "The result has an error message")
    (is (empty? (->> commodities
                     (filter #(= "APPL" (:symbol %)))))
        "The commodity is not retrieved after create")))

(deftest exchange-must-be-a-valid-exchange
  (let [context (serialization/realize storage-spec commodity-context)]
    (testing "it can be :nyse"
      (let [entity-id (-> context :entities first :id)
            commodity (assoc (attributes context)
                             :exchange :nyse
                             :symbol "HD"
                             :name "Home Depot")
            result (commodities/create storage-spec commodity)
            commodities (commodities/select-by-entity-id storage-spec entity-id)]
        (is (empty? (validation/error-messages result))
            "The result has no error messages")))
    (testing "it can be :nasdaq"
      (let [ entity-id (-> context :entities first :id)
            commodity (assoc (attributes context)
                             :exchange :nasdaq
                             :symbol "APPL"
                             :name "Apple")
            result (commodities/create storage-spec commodity)
            commodities (commodities/select-by-entity-id storage-spec entity-id)]
        (is (empty? (validation/error-messages result))
            "The result has no error messages")))
    (testing "it can be :fund"
      (let [entity-id (-> context :entities first :id)
            commodity (assoc (attributes context)
                             :exchange :fund
                             :symbol "VFINX"
                             :name "Vanguard 500 Index")
            result (commodities/create storage-spec commodity)
            commodities (commodities/select-by-entity-id storage-spec entity-id)]
        (is (empty? (validation/error-messages result))
            "The result has no error messages")))
    (testing "it cannot be anything other than :nyse, :nasdaq, or :fund"
      (let [entity-id (-> context :entities first :id)
            commodity (assoc (attributes context)
                             :exchange :not-a-valid-exchange
                             :symbol "NUNYA"
                             :name "None of your business")
            result (commodities/create storage-spec commodity)
            commodities (commodities/select-by-entity-id storage-spec entity-id)]
        (is (= ["Exchange must be one of: nasdaq, fund, nyse"]
               (validation/error-messages result :exchange))
            "The result has an error messages")))))
