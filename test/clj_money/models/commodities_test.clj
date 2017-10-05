(ns clj-money.models.commodities-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.validation :as validation]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.prices :as prices]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def ^:private commodity-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}
              {:name "Business"}]})

(defn- attributes
  [context]
  {:entity-id (-> context :entities first :id)
   :type :stock
   :exchange :nasdaq
   :name "Apple"
   :symbol "APPL"})

(deftest create-a-commodity
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (attributes context)
        result (commodities/create storage-spec commodity)
        commodities (commodities/search storage-spec {:entity-id entity-id})
        expected [{:name "Apple"
                   :type :stock
                   :symbol "APPL"
                   :exchange :nasdaq}]
        actual (map #(dissoc % :id :entity-id :created-at :updated-at)
                    commodities)]
    (is (empty? (validation/error-messages result))
        "The result has no error messages")
    (is (= expected actual)
        "The commodity can be retrieved after create")))

(deftest entity-id-can-be-a-string
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (assoc (attributes context) :entity-id (str entity-id))
        result (commodities/create storage-spec commodity)
        commodities (commodities/search storage-spec {:entity-id entity-id})]
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
        commodities (commodities/search storage-spec {:entity-id entity-id})]
    (is (= ["Entity id is required"]
           (validation/error-messages result :entity-id))
        "The result has an error messages")
    (is (empty? (->> commodities
                     (filter #(= "APPL" (:symbol %)))))
        "The commodity is not retrieved after create")))

(deftest type-is-required
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (dissoc (attributes context) :type)
        result (commodities/create storage-spec commodity)
        commodities (commodities/search storage-spec {:entity-id entity-id})]
    (is (= ["Type is required"]
           (validation/error-messages result :type))
        "The result has an error messages")
    (is (empty? (->> commodities
                     (filter #(= "APPL" (:symbol %)))))
        "The commodity is not retrieved after create")))

(deftest type-can-be-currency
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (assoc (attributes context) :type :currency)
        result (commodities/create storage-spec commodity)
        commodities (commodities/search storage-spec {:entity-id entity-id})]
    (is (empty?  (validation/error-messages result :type))
        "The result has no error messages")
    (is (seq (->> commodities
                  (filter #(= "APPL" (:symbol %)))))
        "The commodity is retrieved after create")))

(deftest type-can-be-stock
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (assoc (attributes context) :type :stock)
        result (commodities/create storage-spec commodity)
        commodities (commodities/search storage-spec {:entity-id entity-id})]
    (is (empty?  (validation/error-messages result :type))
        "The result has no error messages")
    (is (seq (->> commodities
                  (filter #(= "APPL" (:symbol %)))))
        "The commodity is retrieved after create")))

(deftest type-can-be-fund
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (assoc (attributes context) :type :fund)
        result (commodities/create storage-spec commodity)
        commodities (commodities/search storage-spec {:entity-id entity-id})]
    (is (empty?  (validation/error-messages result :type))
        "The result has no error messages")
    (is (seq (->> commodities
                  (filter #(= "APPL" (:symbol %)))))
        "The commodity is retrieved after create")))

(deftest type-cannot-be-invalid
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (assoc (attributes context) :type :not-a-valid-type)
        result (commodities/create storage-spec commodity)
        commodities (commodities/search storage-spec {:entity-id entity-id})]
    (is (= ["Type must be one of: fund, currency, stock"]
           (validation/error-messages result :type))
        "The result has an error messages")
    (is (empty? (->> commodities
                     (filter #(= "APPL" (:symbol %)))))
        "The commodity is not retrieved after create")))

(deftest name-is-required
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (dissoc (attributes context) :name)
        result (commodities/create storage-spec commodity)
        commodities (commodities/search storage-spec {:entity-id entity-id})]
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
        commodities (commodities/search storage-spec {:entity-id entity-id})]
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
        commodities-1 (commodities/search storage-spec {:entity-id entity-1-id})
        commodities-2 (commodities/search storage-spec {:entity-id entity-2-id})]
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
  (let [context (serialization/realize storage-spec commodity-context)
        entity-1 (-> context :entities first)
        entity-2 (-> context :entities second)
        c1 (commodities/create storage-spec (assoc (attributes context) :entity-id (:id entity-1)))
        c2 (commodities/create storage-spec (assoc (attributes context) :entity-id (:id entity-2)))]
    (is (empty? (validation/error-messages c2)))))

(deftest name-can-be-duplicated-between-exchanges
  (let [context (serialization/realize storage-spec commodity-context)
        c1 (commodities/create storage-spec (assoc (attributes context) :exchange :nasdaq))
        c2 (commodities/create storage-spec (assoc (attributes context) :exchange :nyse))]
    (is (empty? (validation/error-messages c2)))))

(deftest symbol-is-required
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (dissoc (attributes context) :symbol)
        result (commodities/create storage-spec commodity)
        commodities (commodities/search storage-spec {:entity-id entity-id})]
    (is (= ["Symbol is required"]
           (validation/error-messages result :symbol))
        "The result has an error message")
    (is (empty? (->> commodities
                     (filter #(= "APPL" (:symbol %)))))
        "The commodity is not retrieved after create")))

(deftest symbol-is-unique-for-an-entity-and-exchange
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (attributes context) 
        result-1 (commodities/create storage-spec commodity)
        result-2 (commodities/create storage-spec commodity)
        commodities (commodities/search storage-spec {:entity-id entity-id})]
    (is (empty? (validation/error-messages result-1))
        "The first is created successfully")
    (is (= ["Symbol must be unique for a given exchange"]
           (validation/error-messages result-2 :symbol))
        "The result has an error messages")
    (is (= 1 (->> commodities
                  (filter #(= "Apple" (:name %)))
                  count))
        "The commodity exists only once after both calls to create")))

(deftest symbol-can-be-duplicated-between-exchanges
  (let [context (serialization/realize storage-spec commodity-context)
        c1 (commodities/create storage-spec (assoc (attributes context) :exchange :nasdaq))
        c2 (commodities/create storage-spec (assoc (attributes context) :exchange :nyse))]
    (is (empty? (validation/error-messages c2)))))

(deftest symbol-can-be-duplicated-between-entities
  (let [context (serialization/realize storage-spec commodity-context)
        entity-1 (-> context :entities first)
        entity-2 (-> context :entities second)
        c1 (commodities/create storage-spec (assoc (attributes context) :entity-id (:id entity-1)))
        c2 (commodities/create storage-spec (assoc (attributes context) :entity-id (:id entity-2)))]
    (is (empty? (validation/error-messages c2)))))

(deftest exchange-can-be-a-string
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (assoc (attributes context) :entity-id (str entity-id))
        result (commodities/create storage-spec commodity)
        commodities (commodities/search storage-spec {:entity-id entity-id})
        expected [{:entity-id entity-id
                   :name "Apple"
                   :symbol "APPL"
                   :type :stock
                   :exchange :nasdaq}]
        actual (map #(dissoc % :updated-at :created-at :id) commodities)]
    (is (empty? (validation/error-messages result))
        "The result has no error messages")
    (is (= expected actual) "The commodity can be retrieved after create")))

(deftest exchange-is-required-for-stocks
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (dissoc (attributes context) :exchange)
        result (commodities/create storage-spec commodity)
        commodities (commodities/search storage-spec {:entity-id entity-id})]
    (is (= ["Exchange is required"]
           (validation/error-messages result :exchange))
        "The result has an error message")
    (is (empty? (->> commodities
                     (filter #(= "APPL" (:symbol %)))))
        "The commodity is not retrieved after create")))

(deftest exchange-is-not-required-for-currencies
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity {:entity-id entity-id
                   :name "US Dollar"
                   :symbol "USD"
                   :type :currency}
        result (commodities/create storage-spec commodity)
        commodities (commodities/search storage-spec {:entity-id entity-id})]
    (is (empty?  (validation/error-messages result :exchange))
        "The result has no error messages")
    (is (seq (->> commodities
                  (filter #(= "USD" (:symbol %)))))
        "The commodity is retrieved after create")))

(deftest exchange-can-be-nasdaq
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (assoc (attributes context)
                         :exchange :nasdaq
                         :symbol "APPL"
                         :name "Apple")
        result (commodities/create storage-spec commodity)
        commodities (commodities/search storage-spec {:entity-id entity-id})]
    (is (empty? (validation/error-messages result))
        "The result has no error messages")))

(deftest exchange-can-be-nyse
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (assoc (attributes context)
                         :exchange :nyse
                         :symbol "HD"
                         :name "Home Depot")
        result (commodities/create storage-spec commodity)
        commodities (commodities/search storage-spec {:entity-id entity-id})]
    (is (empty? (validation/error-messages result))
        "The result has no error messages")))

(deftest exchange-must-be-valid
  (let [context (serialization/realize storage-spec commodity-context)
        entity-id (-> context :entities first :id)
        commodity (assoc (attributes context)
                         :exchange :not-a-valid-exchange
                         :symbol "NUNYA"
                         :name "None of your business")
        result (commodities/create storage-spec commodity)
        commodities (commodities/search storage-spec {:entity-id entity-id})]
    (is (= ["Exchange must be one of: nasdaq, nyse"]
           (validation/error-messages result :exchange))
        "The result has an error messages")))

(def ^:private existing-commodity-context
  (assoc commodity-context :commodities [{:name "Apple"
                                          :type :stock
                                          :symbol "APPL"
                                          :exchange :nasdaq}]))

(deftest a-commodity-can-be-updated
  (let [context (serialization/realize storage-spec existing-commodity-context)
        commodity (-> context :commodities first)
        result (commodities/update storage-spec (assoc commodity :name "New name"))
        retrieved (commodities/find-by-id storage-spec (:id commodity))]
    (is (empty? (validation/error-messages result))
        "The result has no validation errors")
    (is (= "New name" (:name result)) "The return value has the correct name")
    (is (= "New name" (:name retrieved)) "The retrieved value has the correct name")))

(deftest a-commodity-can-be-deleted
  (let [context (serialization/realize storage-spec existing-commodity-context)
        commodity (-> context :commodities first)
        _ (commodities/delete storage-spec (:id commodity))
        retrieved (commodities/find-by-id storage-spec (:id commodity))]
    (is (not retrieved)
        "The commodity cannot be retrieved after delete")))

(def ^:private commodity-with-prices-context
  (assoc existing-commodity-context :prices [{:commodity-id "APPL"
                                              :trade-date (t/local-date 2016 2 27)
                                              :price 10.00M}
                                             {:commodity-id "APPL"
                                              :trade-date (t/local-date 2016 3 2)
                                              :price 10.50M}]))

(deftest deleting-a-commodity-deletes-the-prices
  (let [context (serialization/realize storage-spec commodity-with-prices-context)
        commodity (-> context :commodities first)
        prices-before (prices/search storage-spec {:commodity-id (:id commodity)})
        _ (commodities/delete storage-spec (:id commodity))
        prices-after (prices/search storage-spec {:commodity-id (:id commodity)})]
    (is (not (empty? prices-before))
        "The commodity prices exist before delete")
    (is (empty? prices-after)
        "The commodity prices are absent after delete")))
