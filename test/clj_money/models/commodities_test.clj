(ns clj-money.models.commodities-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [realize
                                            find-entity]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models.commodities :as commodities]))

(use-fixtures :each reset-db)

(def ^:private commodity-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}
              {:name "Business"}]})

(defn- attributes
  [context]
  {:entity-id (:id (find-entity context "Personal"))
   :type :stock
   :exchange :nasdaq
   :name "Apple, Inc."
   :symbol "AAPL"})

(deftest create-a-commodity
  (let [context (realize commodity-context)
        commodity (attributes context)
        result (commodities/create commodity)
        retrieved (commodities/find (:id result))]
    (is (valid? result))
    (is (comparable? commodity result) "The result matches the input")
    (is (comparable? commodity retrieved) "The retrieved matches the input")))

(deftest entity-id-is-required
  (let [context (realize commodity-context)
        entity-id (-> context :entities first :id)
        commodity (dissoc (attributes context) :entity-id)
        result (commodities/create commodity)
        commodities (commodities/search {:entity-id entity-id})]
    (is (invalid? result [:entity-id] "Entity is required"))
    (is (empty? (->> commodities
                     (filter #(= "AAPL" (:symbol %)))))
        "The commodity is not retrieved after create")))

(deftest type-is-required
  (let [context (realize commodity-context)
        entity-id (-> context :entities first :id)
        commodity (dissoc (attributes context) :type)
        result (commodities/create commodity)
        commodities (commodities/search {:entity-id entity-id})]
    (is (invalid? result [:type] "Type is required"))
    (is (empty? (->> commodities
                     (filter #(= "AAPL" (:symbol %)))))
        "The commodity is not retrieved after create")))

(deftest type-can-be-currency
  (let [context (realize commodity-context)
        entity-id (-> context :entities first :id)
        commodity (assoc (attributes context) :type :currency)
        result (commodities/create commodity)
        commodities (commodities/search {:entity-id entity-id})]
    (is (valid? result))
    (is (seq (->> commodities
                  (filter #(= "AAPL" (:symbol %)))))
        "The commodity is retrieved after create")))

(deftest type-can-be-stock
  (let [context (realize commodity-context)
        entity-id (-> context :entities first :id)
        commodity (assoc (attributes context) :type :stock)
        result (commodities/create commodity)
        commodities (commodities/search {:entity-id entity-id})]
    (is (valid? result))
    (is (seq (->> commodities
                  (filter #(= "AAPL" (:symbol %)))))
        "The commodity is retrieved after create")))

(deftest type-can-be-fund
  (let [context (realize commodity-context)
        entity-id (-> context :entities first :id)
        commodity (assoc (attributes context) :type :fund)
        result (commodities/create commodity)
        commodities (commodities/search {:entity-id entity-id})]
    (is (valid? result))
    (is (seq (->> commodities
                  (filter #(= "AAPL" (:symbol %)))))
        "The commodity is retrieved after create")))

(deftest type-cannot-be-invalid
  (let [context (realize commodity-context)
        commodity (assoc (attributes context) :type :not-a-valid-type)
        result (commodities/create commodity)
        retrieved (commodities/find-by (select-keys commodity [:entity-id :symbol :name]))]
    (is (invalid? result [:type] "Type must be fund, currency, or stock"))
    (is (nil? retrieved)
        "The commodity is not retrieved after create")))

(deftest name-is-required
  (let [context (realize commodity-context)
        entity (find-entity context "Personal")
        commodity (dissoc (attributes context) :name)
        result (commodities/create commodity)
        retrieved (commodities/find-by {:entity-id (:id entity)
                                        :symbol (:symbol commodity)})]
    (is (invalid? result [:name] "Name is required"))
    (is (nil? retrieved) "The commodity is not retrieved after create")))

(def existing-context
  (assoc commodity-context
         :commodities [{:name "Apple, Inc."
                        :symbol "AAPL"
                        :exchange :nasdaq
                        :type :stock
                        :entity-id "Personal"}]))

(deftest name-is-unique-for-an-entity-and-exchange
  (let [ctx (realize existing-context)
        commodity (attributes ctx)
        result (commodities/create commodity)
        retrieved (commodities/search (select-keys commodity [:entity-id :name]))]
    (is (invalid? result [:name] "Name is already in use"))
    (is (= 1 (count retrieved))
        "The commodity exists only once after both calls to create")))

(deftest name-can-be-duplicated-between-exchanges
  (let [context (realize existing-context)
        entity (find-entity context "Personal")
        commodity (assoc (attributes context)
                         :exchange :nyse
                         :symbol "APLL")
        result (commodities/create commodity)
        retrieved (commodities/search {:name "Apple, Inc."
                                       :entity-id (:id entity)})]
    (is (valid? result))
    (is (= {:nyse 1
            :nasdaq 1}
           (->> retrieved
                (map :exchange)
                frequencies))
        "The commodity can be retrieved after create")))

(deftest name-can-be-duplicated-between-entities
  (let [context (realize existing-context)
        entity (find-entity context "Business")
        commodity (assoc (attributes context)
                         :entity-id (:id entity)
                         :symbol "APLL")
        result (commodities/create commodity)
        retrieved (commodities/search {:name "Apple, Inc."})]
    (is (valid? result))
    (is (= 2 (count retrieved))
        "The commodity can be retrieved after create")))

(deftest symbol-is-required
  (let [context (realize commodity-context)
        entity (find-entity context "Personal")
        commodity {:entity-id (:id entity)
                   :name "Test"
                   :type :currency}
        result (commodities/create commodity)
        retrieved (commodities/find-by {:entity-id (:id entity)
                                        :name "Test"})]
    (is (invalid? result [:symbol] "Symbol is required"))
    (is (nil? retrieved) "The commodity is not retrieved after create")))

(deftest symbol-is-unique-for-an-entity-and-exchange
  (let [context (realize existing-context)
        commodity (assoc (attributes context) :name "New Name")
        result (commodities/create commodity)
        retrieved (commodities/search (select-keys commodity [:entity-id :symbol]))]
    (is (invalid? result [:symbol] "Symbol is already in use"))
    (is (= 1 (count retrieved))
        "The commodity exists only once after both calls to create")))

(deftest symbol-can-be-duplicated-between-exchanges
  (let [context (realize existing-context)
        result (commodities/create (assoc (attributes context)
                                          :name "Apply"
                                          :exchange :nyse))
        retrieved (commodities/search {:symbol (:symbol result)})]
    (is (valid? result))
    (is (= {:nyse 1
            :nasdaq 1}
           (->> retrieved
                (map :exchange)
                frequencies))
        "The value can be retrieved after create")))

(deftest symbol-can-be-duplicated-between-entities
  (let [context (realize existing-context)
        entity (find-entity context "Business")
        result (commodities/create (assoc (attributes context)
                                          :entity-id (:id entity)))]
    (is (valid? result))))

(deftest exchange-is-required-for-stocks
  (let [context (realize commodity-context)
        entity-id (-> context :entities first :id)
        commodity (dissoc (attributes context) :exchange)
        result (commodities/create commodity)
        retrieved (commodities/find-by {:entity-id entity-id
                                        :symbol (:symbol commodity)})]
    (is (invalid? result [:exchange] "Exchange is required"))
    (is (nil? retrieved)
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
    (is (valid? result))
    (is retrieved "The commodity is retrieved after create")))

(deftest exchange-can-be-nasdaq
  (let [context (realize commodity-context)
        commodity (assoc (attributes context)
                         :exchange :nasdaq
                         :symbol "AAPL"
                         :name "Apple")
        result (commodities/create commodity)]
    (is (valid? result))))

(deftest exchange-can-be-nyse
  (let [context (realize commodity-context)
        commodity (assoc (attributes context)
                         :exchange :nyse
                         :symbol "HD"
                         :name "Home Depot")
        result (commodities/create commodity)]
    (is (valid? result))))

(deftest exchange-must-be-valid
  (let [context (realize commodity-context)
        commodity (assoc (attributes context)
                         :exchange :not-valid
                         :symbol "NUNYA"
                         :name "None of your business")
        result (commodities/create commodity)]
    (is (invalid? result [:exchange] "Exchange must be amex, nasdaq, or nyse"))))

(deftest a-commodity-can-be-updated
  (let [context (realize existing-context)
        commodity (-> context :commodities first)
        result (commodities/update (assoc commodity :name "New name"))
        retrieved (commodities/find commodity)]
    (is (valid? result))
    (is (= "New name" (:name result)) "The return value has the correct name")
    (is (= "New name" (:name retrieved)) "The retrieved value has the correct name")))

(deftest a-commodity-can-be-deleted
  (let [context (realize existing-context)
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
