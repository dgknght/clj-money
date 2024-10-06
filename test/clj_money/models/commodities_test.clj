(ns clj-money.models.commodities-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.validation :as v]
            [clj-money.db.sql.ref]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            find-entity
                                            find-commodity]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models.commodities :as commodities]))

(use-fixtures :each reset-db)

(def ^:private commodity-context
  {:users [(factory :user {:user/email "john@doe.com"})]
   :entities [{:entity/name "Personal"
               :entity/user "john@doe.com"}
              {:entity/name "Business"
               :entity/user "john@doe.com"}]})

(defn- attributes []
  #:commodity{:entity (find-entity "Personal")
              :type :stock
              :exchange :nasdaq
              :name "Apple, Inc."
              :symbol "AAPL"
              :price-config {:price-config/enabled true}})

(deftest create-a-commodity
  (with-context commodity-context
    (let [attr (attributes)
          result (commodities/put attr)
          expected (update-in attr [:commodity/entity] select-keys [:id])]
      (is (comparable? expected result)
          "The result matches the input")
      (is (comparable? expected (commodities/find (:id result)))
          "The retrieved matches the input"))))

(deftest entity-id-is-required
  (with-context commodity-context
    (is (thrown-with-ex-data?
          "Validation failed"
          {::v/errors {:commodity/entity ["Entity is required"]}}
          (commodities/put (dissoc (attributes) :commodity/entity))))))

(deftest type-is-required
  (with-context commodity-context
    (is (thrown-with-ex-data?
          "Validation failed"
          {::v/errors {:commodity/type ["Type is required"]}}
          (commodities/put (dissoc (attributes) :commodity/type))))))

(deftest type-can-be-currency
  (with-context commodity-context
    (let [commodity (commodities/put
                      (merge (attributes)
                             #:commodity{:type :currency
                                         :name "US Dollar"
                                         :symbol "USD"}))]
      (is (comparable? #:commodity{:type :currency
                                   :name "US Dollar"
                                   :symbol "USD"}
                       commodity)
          "The commodity is created with the specified attributes"))))

(deftest type-can-be-stock
  (with-context commodity-context
    (let [commodity (commodities/put
                      (merge (attributes)
                             #:commodity{:type :stock
                                         :name "Apple Inc."
                                         :symbol "AAPL"}))]
      (is (comparable? #:commodity{:type :stock
                                   :name "Apple Inc."
                                   :symbol "AAPL"}
                       commodity)
          "The commodity is created with the specified attributes"))))

(deftest type-can-be-fund
  (with-context commodity-context
    (let [commodity (commodities/put
                      (merge (attributes)
                             #:commodity{:type :fund
                                         :name "Vanguard S&P 500 Index Fund"
                                         :symbol "VFIAX"}))]
      (is (comparable? #:commodity{:type :fund
                                   :name "Vanguard S&P 500 Index Fund"
                                   :symbol "VFIAX"}
                       commodity)
          "The commodity is created with the specified attributes"))))

(deftest type-cannot-be-invalid
  (with-context commodity-context
    (is (thrown-with-ex-data?
          "Validation failed"
          {::v/errors {:commodity/type ["Type must be one of :currency, :stock, or :fund"]}}
          (commodities/put (assoc (attributes)
                                     :commodity/type :not-a-valid-type))))))

(deftest name-is-required
  (with-context commodity-context
    (is (thrown-with-ex-data?
          "Validation failed"
          {::v/errors {:commodity/name ["Name is required"]}}
          (commodities/put (dissoc (attributes) :commodity/name))))))

(def existing-context
  (assoc commodity-context
         :commodities [#:commodity{:name "Apple, Inc."
                                   :symbol "AAPL"
                                   :exchange :nasdaq
                                   :type :stock
                                   :entity "Personal"}]))

(deftest name-is-unique-for-an-entity-and-exchange
  (with-context existing-context
    (is (thrown-with-ex-data?
          "Validation failed"
          {::v/errors {:commodity/name ["Name is already in use"]}}
          (commodities/put (attributes))))))

(deftest name-can-be-duplicated-between-exchanges
  (with-context existing-context
    (let [commodity (commodities/put
                      (assoc (attributes)
                             :commodity/exchange :nyse
                             :commodity/symbol "AAPL"))]
      (is (comparable? #:commodity{:name "Apple, Inc."
                                   :symbol "AAPL"}
                       commodity)))))

(deftest name-can-be-duplicated-between-entities
  (with-context existing-context
    (let [commodity (commodities/put
                      (assoc (attributes)
                             :commodity/entity (find-entity "Business")))]
      (is (comparable? #:commodity{:name "Apple, Inc."
                                   :symbol "AAPL"}
                       commodity)))))

(deftest symbol-is-required
  (with-context commodity-context
    (is (thrown-with-ex-data?
          "Validation failed"
          {::v/errors {:commodity/name ["Name is required"]}}
          (commodities/put (dissoc (attributes) :commodity/symbol))))))

(deftest symbol-is-unique-for-an-entity-and-exchange
  (with-context existing-context
    (let [commodity (assoc (attributes) :name "New Name")
          result (commodities/put commodity)
          retrieved (commodities/search (select-keys commodity [:entity-id :symbol]))]
      (is (invalid? result [:symbol] "Symbol is already in use"))
      (is (= 1 (count retrieved))
          "The commodity exists only once after both calls to create"))))

(deftest symbol-can-be-duplicated-between-exchanges
  (with-context existing-context
    (let [result (commodities/put (assoc (attributes)
                                            :name "Apply"
                                            :exchange :nyse))
          retrieved (commodities/search {:symbol (:symbol result)})]
      (is (valid? result))
      (is (= {:nyse 1
              :nasdaq 1}
             (->> retrieved
                  (map :exchange)
                  frequencies))
          "The value can be retrieved after create"))))

(deftest symbol-can-be-duplicated-between-entities
  (with-context existing-context
    (let [entity (find-entity "Business")
          result (commodities/put (assoc (attributes)
                                            :entity-id (:id entity)))]
      (is (valid? result)))))

(deftest exchange-is-required-for-stocks
  (with-context commodity-context
    (let [entity (find-entity "Personal")
          commodity (dissoc (attributes) :exchange)
          result (commodities/put commodity)
          retrieved (commodities/find-by {:entity-id (:id entity)
                                          :symbol (:symbol commodity)})]
      (is (invalid? result [:exchange] "Exchange is required"))
      (is (nil? retrieved)
          "The commodity is not retrieved after create"))))

(deftest exchange-is-required-for-funds
  (with-context commodity-context
    (let [entity (find-entity "Personal")
          commodity (-> (attributes)
                        (assoc :type :fund)
                        (dissoc :exchange))
          result (commodities/put commodity)
          retrieved (commodities/find-by {:entity-id (:id entity)
                                          :symbol (:symbol commodity)})]
      (is (invalid? result [:exchange] "Exchange is required"))
      (is (nil? retrieved)
          "The commodity is not retrieved after create"))))

(deftest exchange-is-not-required-for-currencies
  (with-context commodity-context
    (let [entity (find-entity "Personal")
          commodity {:entity-id (:id entity)
                     :name "US Dollar"
                     :symbol "USD"
                     :type :currency
                     :price-config {:enabled false}}
          result (commodities/put commodity)
          retrieved (commodities/find-by {:entity-id (:id entity)
                                          :symbol "USD"})]
      (is (valid? result))
      (is retrieved "The commodity is retrieved after create"))))

(deftest exchange-can-be-nasdaq
  (with-context commodity-context
    (let [commodity (assoc (attributes)
                           :exchange :nasdaq
                           :symbol "AAPL"
                           :name "Apple")
          result (commodities/put commodity)]
      (is (valid? result)))))

(deftest exchange-can-be-nyse
  (with-context commodity-context
    (let [commodity (assoc (attributes)
                           :exchange :nyse
                           :symbol "HD"
                           :name "Home Depot")
          result (commodities/put commodity)]
      (is (valid? result)))))

(deftest exchange-can-be-otc
  (with-context commodity-context
    (let [commodity (assoc (attributes)
                           :exchange :otc
                           :symbol "GBTC"
                           :name "Grayscale Bitcoin Trust")
          result (commodities/put commodity)]
      (is (valid? result)))))

(deftest exchange-must-be-valid
  (with-context commodity-context
    (let [commodity (assoc (attributes)
                           :exchange :not-valid
                           :symbol "NUNYA"
                           :name "None of your business")
          result (commodities/put commodity)]
      (is (invalid? result [:exchange] "Exchange must be amex, nasdaq, nyse, or otc")))))

(deftest a-commodity-can-be-updated
  (with-context existing-context
    (let [commodity (find-commodity "AAPL")
          result (commodities/put (-> commodity
                                         (assoc :name "New name")
                                         (assoc-in [:price-config :enabled] false)))
          retrieved (commodities/find commodity)]
      (is (valid? result))
      (is (= "New name" (:name result)) "The return value has the correct name")
      (is (not (get-in result [:price-config :enabled]))
          "The return value contains the updated price config")
      (is (= "New name" (:name retrieved)) "The retrieved value has the correct name")
      (is (not (get-in retrieved[:price-config :enabled]))
          "The retrieved value contains the updated price config"))))

(deftest a-commodity-can-be-deleted
  (with-context existing-context
    (let [commodity (find-commodity "AAPL")]
      (commodities/delete commodity)
      (is (nil? (commodities/find commodity))
          "The commodity cannot be retrieved after delete."))))

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
  (with-context count-context
    (let [entity (find-entity "Personal")
          result (commodities/count {:entity-id (:id entity)})]
      (is  (= 3 result)))))
