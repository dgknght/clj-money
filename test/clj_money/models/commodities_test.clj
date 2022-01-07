(ns clj-money.models.commodities-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            find-entity
                                            find-commodity]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models.commodities :as commodities]))

(use-fixtures :each reset-db)

(def ^:private commodity-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}
              {:name "Business"}]})

(defn- attributes []
  {:entity-id (:id (find-entity "Personal"))
   :type :stock
   :exchange :nasdaq
   :name "Apple, Inc."
   :symbol "AAPL"
   :price-config {:enabled true}})

(deftest create-a-commodity
  (with-context commodity-context
    (let [commodity (attributes)
          result (commodities/create commodity)
          retrieved (commodities/find (:id result))]
      (is (valid? result))
      (is (comparable? commodity result) "The result matches the input")
      (is (comparable? commodity retrieved) "The retrieved matches the input"))))

(deftest entity-id-is-required
  (with-context commodity-context
    (let [entity (find-entity "Personal")
          commodity (dissoc (attributes) :entity-id)
          result (commodities/create commodity)
          commodities (commodities/search {:entity-id (:id entity)})]
      (is (invalid? result [:entity-id] "Entity is required"))
      (is (empty? (->> commodities
                       (filter #(= "AAPL" (:symbol %)))))
          "The commodity is not retrieved after create"))))

(deftest type-is-required
  (with-context commodity-context
    (let [entity (find-entity "Personal")
          commodity (dissoc (attributes) :type)
          result (commodities/create commodity)
          commodities (commodities/search {:entity-id (:id entity)})]
      (is (invalid? result [:type] "Type is required"))
      (is (empty? (->> commodities
                       (filter #(= "AAPL" (:symbol %)))))
          "The commodity is not retrieved after create"))))

(deftest type-can-be-currency
  (with-context commodity-context
  (let [entity (find-entity "Personal")
        commodity (assoc (attributes) :type :currency)
        result (commodities/create commodity)
        commodities (commodities/search {:entity-id (:id entity)})]
    (is (valid? result))
    (is (seq (->> commodities
                  (filter #(= "AAPL" (:symbol %)))))
        "The commodity is retrieved after create"))))

(deftest type-can-be-stock
  (with-context commodity-context
    (let [entity (find-entity "Personal")
          commodity (assoc (attributes) :type :stock)
          result (commodities/create commodity)
          commodities (commodities/search {:entity-id (:id entity)})]
      (is (valid? result))
      (is (seq (->> commodities
                    (filter #(= "AAPL" (:symbol %)))))
          "The commodity is retrieved after create"))))

(deftest type-can-be-fund
  (with-context commodity-context
    (let [entity (find-entity "Personal")
          commodity (assoc (attributes) :type :fund)
          result (commodities/create commodity)
          commodities (commodities/search {:entity-id (:id entity)})]
      (is (valid? result))
      (is (seq (->> commodities
                    (filter #(= "AAPL" (:symbol %)))))
          "The commodity is retrieved after create"))))

(deftest type-cannot-be-invalid
  (with-context commodity-context
    (let [commodity (assoc (attributes) :type :not-a-valid-type)
          result (commodities/create commodity)
          retrieved (commodities/find-by (select-keys commodity [:entity-id :symbol :name]))]
      (is (invalid? result [:type] "Type must be fund, currency, or stock"))
      (is (nil? retrieved)
          "The commodity is not retrieved after create"))))

(deftest name-is-required
  (with-context commodity-context
    (let [entity (find-entity "Personal")
          commodity (dissoc (attributes) :name)
          result (commodities/create commodity)
          retrieved (commodities/find-by {:entity-id (:id entity)
                                          :symbol (:symbol commodity)})]
      (is (invalid? result [:name] "Name is required"))
      (is (nil? retrieved) "The commodity is not retrieved after create"))))

(def existing-context
  (assoc commodity-context
         :commodities [{:name "Apple, Inc."
                        :symbol "AAPL"
                        :exchange :nasdaq
                        :type :stock
                        :entity-id "Personal"}]))

(deftest name-is-unique-for-an-entity-and-exchange
  (with-context existing-context
    (let [commodity (attributes)
          result (commodities/create commodity)
          retrieved (commodities/search (select-keys commodity [:entity-id :name]))]
      (is (invalid? result [:name] "Name is already in use"))
      (is (= 1 (count retrieved))
          "The commodity exists only once after both calls to create"))))

(deftest name-can-be-duplicated-between-exchanges
  (with-context existing-context
    (let [entity (find-entity "Personal")
          commodity (assoc (attributes)
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
          "The commodity can be retrieved after create"))))

(deftest name-can-be-duplicated-between-entities
  (with-context existing-context
    (let [entity (find-entity "Business")
          commodity (assoc (attributes)
                           :entity-id (:id entity)
                           :symbol "APLL")
          result (commodities/create commodity)
          retrieved (commodities/search {:name "Apple, Inc."})]
      (is (valid? result))
      (is (= 2 (count retrieved))
          "The commodity can be retrieved after create"))))

(deftest symbol-is-required
  (with-context commodity-context
    (let [entity (find-entity "Personal")
          commodity {:entity-id (:id entity)
                     :name "Test"
                     :type :currency
                     :price-config {:enabled false}}
          result (commodities/create commodity)
          retrieved (commodities/find-by {:entity-id (:id entity)
                                          :name "Test"})]
      (is (invalid? result [:symbol] "Symbol is required"))
      (is (nil? retrieved) "The commodity is not retrieved after create"))))

(deftest symbol-is-unique-for-an-entity-and-exchange
  (with-context existing-context
    (let [commodity (assoc (attributes) :name "New Name")
          result (commodities/create commodity)
          retrieved (commodities/search (select-keys commodity [:entity-id :symbol]))]
      (is (invalid? result [:symbol] "Symbol is already in use"))
      (is (= 1 (count retrieved))
          "The commodity exists only once after both calls to create"))))

(deftest symbol-can-be-duplicated-between-exchanges
  (with-context existing-context
    (let [result (commodities/create (assoc (attributes)
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
          result (commodities/create (assoc (attributes)
                                            :entity-id (:id entity)))]
      (is (valid? result)))))

(deftest exchange-is-required-for-stocks
  (with-context commodity-context
    (let [entity (find-entity "Personal")
          commodity (dissoc (attributes) :exchange)
          result (commodities/create commodity)
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
          result (commodities/create commodity)
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
          result (commodities/create commodity)
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
          result (commodities/create commodity)]
      (is (valid? result)))))

(deftest exchange-can-be-nyse
  (with-context commodity-context
    (let [commodity (assoc (attributes)
                           :exchange :nyse
                           :symbol "HD"
                           :name "Home Depot")
          result (commodities/create commodity)]
      (is (valid? result)))))

(deftest exchange-can-be-otc
  (with-context commodity-context
    (let [commodity (assoc (attributes)
                           :exchange :otc
                           :symbol "GBTC"
                           :name "Grayscale Bitcoin Trust")
          result (commodities/create commodity)]
      (is (valid? result)))))

(deftest exchange-must-be-valid
  (with-context commodity-context
    (let [commodity (assoc (attributes)
                           :exchange :not-valid
                           :symbol "NUNYA"
                           :name "None of your business")
          result (commodities/create commodity)]
      (is (invalid? result [:exchange] "Exchange must be amex, nasdaq, nyse, or otc")))))

(deftest a-commodity-can-be-updated
  (with-context existing-context
    (let [commodity (find-commodity "AAPL")
          result (commodities/update (-> commodity
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
    (let [commodity (find-commodity "AAPL")
          _ (commodities/delete commodity)
          retrieved (commodities/find commodity)]
      (is (not retrieved)
          "The commodity cannot be retrieved after delete"))))

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
