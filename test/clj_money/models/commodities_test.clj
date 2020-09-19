(ns clj-money.models.commodities-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [realize
                                            find-entity]]
            [clj-money.validation :as validation]
            [clj-money.test-helpers :refer [reset-db
                                            pprint-diff]]
            [clj-money.models.commodities :as commodities]))

(use-fixtures :each reset-db)

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
   :symbol "AAPL"})

(deftest create-a-commodity
  (let [context (realize commodity-context)
        entity-id (-> context :entities first :id)
        commodity (attributes context)
        result (commodities/create commodity)
        commodities (commodities/search {:entity-id entity-id})
        expected [{:name "Apple"
                   :type :stock
                   :symbol "AAPL"
                   :earliest-price nil
                   :latest-price nil
                   :exchange :nasdaq}]
        actual (map #(dissoc % :id :entity-id :created-at :updated-at)
                    commodities)]
    (is (empty? (validation/error-messages result))
        "The result has no error messages")
    (pprint-diff expected actual)
    (is (= expected actual)
        "The commodity can be retrieved after create")))

(deftest entity-id-is-required
  (let [context (realize commodity-context)
        entity-id (-> context :entities first :id)
        commodity (dissoc (attributes context) :entity-id)
        result (commodities/create commodity)
        commodities (commodities/search {:entity-id entity-id})]
    (is (= ["Entity id is required"]
           (validation/error-messages result :entity-id))
        "The result has an error messages")
    (is (empty? (->> commodities
                     (filter #(= "AAPL" (:symbol %)))))
        "The commodity is not retrieved after create")))

(deftest type-is-required
  (let [context (realize commodity-context)
        entity-id (-> context :entities first :id)
        commodity (dissoc (attributes context) :type)
        result (commodities/create commodity)
        commodities (commodities/search {:entity-id entity-id})]
    (is (= ["Type is required"]
           (validation/error-messages result :type))
        "The result has an error messages")
    (is (empty? (->> commodities
                     (filter #(= "AAPL" (:symbol %)))))
        "The commodity is not retrieved after create")))

(deftest type-can-be-currency
  (let [context (realize commodity-context)
        entity-id (-> context :entities first :id)
        commodity (assoc (attributes context) :type :currency)
        result (commodities/create commodity)
        commodities (commodities/search {:entity-id entity-id})]
    (is (empty?  (validation/error-messages result :type))
        "The result has no error messages")
    (is (seq (->> commodities
                  (filter #(= "AAPL" (:symbol %)))))
        "The commodity is retrieved after create")))

(deftest type-can-be-stock
  (let [context (realize commodity-context)
        entity-id (-> context :entities first :id)
        commodity (assoc (attributes context) :type :stock)
        result (commodities/create commodity)
        commodities (commodities/search {:entity-id entity-id})]
    (is (empty?  (validation/error-messages result :type))
        "The result has no error messages")
    (is (seq (->> commodities
                  (filter #(= "AAPL" (:symbol %)))))
        "The commodity is retrieved after create")))

(deftest type-can-be-fund
  (let [context (realize commodity-context)
        entity-id (-> context :entities first :id)
        commodity (assoc (attributes context) :type :fund)
        result (commodities/create commodity)
        commodities (commodities/search {:entity-id entity-id})]
    (is (empty?  (validation/error-messages result :type))
        "The result has no error messages")
    (is (seq (->> commodities
                  (filter #(= "AAPL" (:symbol %)))))
        "The commodity is retrieved after create")))

(deftest type-cannot-be-invalid
  (let [context (realize commodity-context)
        entity-id (-> context :entities first :id)
        commodity (assoc (attributes context) :type :not-a-valid-type)
        result (commodities/create commodity)
        commodities (commodities/search {:entity-id entity-id})]
    (is (= ["Type must be one of: fund, currency, stock"]
           (validation/error-messages result :type))
        "The result has an error messages")
    (is (empty? (->> commodities
                     (filter #(= "AAPL" (:symbol %)))))
        "The commodity is not retrieved after create")))

(deftest name-is-required
  (let [context (realize commodity-context)
        entity-id (-> context :entities first :id)
        commodity (dissoc (attributes context) :name)
        result (commodities/create commodity)
        commodities (commodities/search {:entity-id entity-id})]
    (is (= ["Name is required"]
           (validation/error-messages result :name))
        "The result has an error messages")
    (is (empty? (->> commodities
                     (filter #(= "AAPL" (:symbol %)))))
        "The commodity is not retrieved after create")))

(deftest name-is-unique-for-an-entity-and-exchange
  (let [context (realize commodity-context)
        entity-id (-> context :entities first :id)
        commodity (attributes context) 
        result-1 (commodities/create commodity)
        result-2 (commodities/create commodity)
        commodities (commodities/search {:entity-id entity-id})]
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
  (let [context (realize commodity-context)
        entity-1-id (-> context :entities first :id)
        entity-2-id (-> context :entities second :id)
        commodity-1 (assoc (attributes context) :entity-id entity-1-id)
        commodity-2 (assoc (attributes context) :entity-id entity-2-id)
        result-1 (commodities/create commodity-1)
        result-2 (commodities/create commodity-2)
        commodities-1 (commodities/search {:entity-id entity-1-id})
        commodities-2 (commodities/search {:entity-id entity-2-id})]
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
  (let [context (realize commodity-context)
        entity-1 (-> context :entities first)
        entity-2 (-> context :entities second)
        _ (commodities/create (assoc (attributes context) :entity-id (:id entity-1)))
        c2 (commodities/create (assoc (attributes context) :entity-id (:id entity-2)))]
    (is (empty? (validation/error-messages c2)))))

(deftest symbol-is-required
  (let [context (realize commodity-context)
        entity-id (-> context :entities first :id)
        commodity (dissoc (attributes context) :symbol)
        result (commodities/create commodity)
        commodities (commodities/search {:entity-id entity-id})]
    (is (= ["Symbol is required"]
           (validation/error-messages result :symbol))
        "The result has an error message")
    (is (empty? (->> commodities
                     (filter #(= "AAPL" (:symbol %)))))
        "The commodity is not retrieved after create")))

(deftest symbol-is-unique-for-an-entity-and-exchange
  (let [context (realize commodity-context)
        entity-id (-> context :entities first :id)
        commodity (attributes context) 
        result-1 (commodities/create commodity)
        result-2 (commodities/create commodity)
        commodities (commodities/search {:entity-id entity-id})]
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
  (let [context (realize commodity-context)
        _ (commodities/create (assoc (attributes context) :exchange :nasdaq))
        c2 (commodities/create (assoc (attributes context) :exchange :nyse))]
    (is (empty? (validation/error-messages c2)))))

(deftest symbol-can-be-duplicated-between-entities
  (let [context (realize commodity-context)
        entity-1 (-> context :entities first)
        entity-2 (-> context :entities second)
        _ (commodities/create (assoc (attributes context) :entity-id (:id entity-1)))
        c2 (commodities/create (assoc (attributes context) :entity-id (:id entity-2)))]
    (is (empty? (validation/error-messages c2)))))

(deftest exchange-is-required-for-stocks
  (let [context (realize commodity-context)
        entity-id (-> context :entities first :id)
        commodity (dissoc (attributes context) :exchange)
        result (commodities/create commodity)
        commodities (commodities/search {:entity-id entity-id})]
    (is (= ["Exchange is required"]
           (validation/error-messages result :exchange))
        "The result has an error message")
    (is (empty? (->> commodities
                     (filter #(= "AAPL" (:symbol %)))))
        "The commodity is not retrieved after create")))

(deftest exchange-is-not-required-for-currencies
  (let [context (realize commodity-context)
        entity-id (-> context :entities first :id)
        commodity {:entity-id entity-id
                   :name "US Dollar"
                   :symbol "USD"
                   :type :currency}
        result (commodities/create commodity)
        retrieved (commodities/find-by {:entity-id entity-id
                                        :symbol "USD"})]
    (is (empty?  (validation/error-messages result :exchange))
        "The result has no error messages")
    (is retrieved
        "The commodity is retrieved after create")))

(deftest exchange-can-be-nasdaq
  (let [context (realize commodity-context)
        commodity (assoc (attributes context)
                         :exchange :nasdaq
                         :symbol "AAPL"
                         :name "Apple")
        result (commodities/create commodity)]
    (is (empty? (validation/error-messages result))
        "The result has no error messages")))

(deftest exchange-can-be-nyse
  (let [context (realize commodity-context)
        commodity (assoc (attributes context)
                         :exchange :nyse
                         :symbol "HD"
                         :name "Home Depot")
        result (commodities/create commodity)]
    (is (empty? (validation/error-messages result))
        "The result has no error messages")))

(deftest exchange-must-be-valid
  (let [context (realize commodity-context)
        commodity (assoc (attributes context)
                         :exchange :not-a-valid-exchange
                         :symbol "NUNYA"
                         :name "None of your business")
        result (commodities/create commodity)]
    (is (= ["Exchange must be one of: amex, nasdaq, nyse"]
           (validation/error-messages result :exchange))
        "The result has an error messages")))

(def ^:private existing-commodity-context
  (assoc commodity-context :commodities [{:name "Apple"
                                          :type :stock
                                          :symbol "AAPL"
                                          :exchange :nasdaq}]))

(deftest a-commodity-can-be-updated
  (let [context (realize existing-commodity-context)
        commodity (-> context :commodities first)
        result (commodities/update (assoc commodity :name "New name"))
        retrieved (commodities/find commodity)]
    (is (empty? (validation/error-messages result))
        "The result has no validation errors")
    (is (= "New name" (:name result)) "The return value has the correct name")
    (is (= "New name" (:name retrieved)) "The retrieved value has the correct name")))

(deftest a-commodity-can-be-deleted
  (let [context (realize existing-commodity-context)
        commodity (-> context :commodities first)
        _ (commodities/delete commodity)
        retrieved (commodities/find commodity)]
    (is (not retrieved)
        "The commodity cannot be retrieved after delete")))

(def ^:private count-context
  (assoc commodity-context :commodities [{:name "Microsoft, Inc."
                                          :entity-id "Personal"
                                          :symbol "MSFT"
                                          :type :stock
                                          :exchange :nasdaq}
                                         {:name "Apple, Inc."
                                          :entity-id "Personal"
                                          :symbol "AAPL"
                                          :type :stock
                                          :exchange :nasdaq}
                                         {:name "United States Dollar"
                                          :entity-id "Personal"
                                          :symbol "USD"
                                          :type :currency}
                                         {:name "British Pound"
                                          :entity-id "Business"
                                          :symbol "GBP"
                                          :type :currency}]))

(deftest get-a-count-of-commodities
  (let [ctx (realize count-context)
        entity (find-entity ctx "Personal")
        result (commodities/count {:entity-id (:id entity)})]
    (is  (= 3 result))))
