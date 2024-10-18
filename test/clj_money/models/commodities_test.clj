(ns clj-money.models.commodities-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [clojure.pprint :refer [pprint]]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.validation :as v]
            [clj-money.model-helpers :as helpers :refer [assert-invalid
                                                         assert-deleted]]
            [clj-money.models :as models]
            [clj-money.db.sql.ref]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            find-entity
                                            find-commodity]]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(def ^:private commodity-context
  [(factory :user {:user/email "john@doe.com"})
   #:entity{:name "Personal"
            :user "john@doe.com"}
   #:entity{:name "Business"
            :user "john@doe.com"}])

(defn- attributes []
  #:commodity{:entity (find-entity "Personal")
              :type :stock
              :exchange :nasdaq
              :name "Apple, Inc."
              :symbol "AAPL"
              :price-config {:price-config/enabled true}})

(defn- assert-created
  [attr]
  (helpers/assert-created attr :refs [:commodity/entity]))

(deftest create-a-commodity
  (with-context commodity-context
    (assert-created (attributes))))

(deftest entity-id-is-required
  (with-context commodity-context
    (assert-invalid (dissoc (attributes) :commodity/entity)
                    {:commodity/entity ["Entity is required"]})))

(deftest type-is-required
  (with-context commodity-context
    (assert-invalid (dissoc (attributes) :commodity/type)
                    {:commodity/type ["Type is required"]})))

(deftest type-can-be-currency
  (with-context commodity-context
    (assert-created (merge (attributes)
                            #:commodity{:type :currency
                                        :name "US Dollar"
                                        :symbol "USD"}))))

(deftest type-can-be-stock
  (with-context commodity-context
    (assert-created (merge (attributes)
                            #:commodity{:type :stock
                                        :name "Apple Inc."
                                        :symbol "AAPL"}))))

(deftest type-can-be-fund
  (with-context commodity-context
    (assert-created (merge (attributes)
                            #:commodity{:type :fund
                                        :name "Vanguard S&P 500 Index Fund"
                                        :symbol "VFIAX"}))))

(deftest type-cannot-be-invalid
  (with-context commodity-context
    (assert-invalid (assoc (attributes) :commodity/type :not-a-valid-type)
                    {:commodity/type ["Type must be fund, currency, or stock"]})))

(deftest name-is-required
  (with-context commodity-context
    (assert-invalid (dissoc (attributes) :commodity/name)
                    {:commodity/name ["Name is required"]})))

(def ^:private existing-context
  (concat commodity-context
          [#:commodity{:name "Apple, Inc."
                       :symbol "AAPL"
                       :exchange :nasdaq
                       :type :stock
                       :entity "Personal"}]))

(deftest name-is-unique-for-an-entity-and-exchange
  (with-context existing-context
    (assert-invalid (attributes)
                    {:commodity/name ["Name is already in use"]})))

(deftest name-can-be-duplicated-between-exchanges
  (with-context existing-context
    (let [commodity (models/put
                      (assoc (attributes)
                             :commodity/exchange :nyse
                             :commodity/symbol "AAPL"))]
      (is (comparable? #:commodity{:name "Apple, Inc."
                                   :symbol "AAPL"}
                       commodity)))))

(deftest name-can-be-duplicated-between-entities
  (with-context existing-context
    (let [commodity (models/put
                      (assoc (attributes)
                             :commodity/entity (find-entity "Business")))]
      (is (comparable? #:commodity{:name "Apple, Inc."
                                   :symbol "AAPL"}
                       commodity)))))

(deftest symbol-is-required
  (with-context commodity-context
    (is (thrown-with-ex-data?
          "Validation failed"
          {::v/errors {:commodity/symbol ["Symbol is required"]}}
          (models/put (dissoc (attributes) :commodity/symbol))))))

(deftest symbol-is-unique-for-an-entity-and-exchange
  (with-context existing-context
    (assert-invalid (assoc (attributes) :commodity/name "New Name")
                    {:commodity/symbol ["Symbol is already in use"]})))

(deftest symbol-can-be-duplicated-between-exchanges
  (with-context existing-context
    (assert-created (assoc (attributes)
                           :commodity/name "Apply"
                           :commodity/exchange :nyse))))

(deftest symbol-can-be-duplicated-between-entities
  (with-context existing-context
    (assert-created (assoc (attributes)
                           :commodity/entity (find-entity "Business")))))

(deftest exchange-is-required-for-stocks
  (with-context commodity-context
    (assert-invalid (dissoc (attributes) :commodity/exchange)
                    {:commodity/exchange ["Exchange is required"]})))

(deftest exchange-is-required-for-funds
  (with-context commodity-context
    (assert-invalid (-> (attributes)
                        (assoc :commodity/type :fund)
                        (dissoc :commodity/exchange))
                    {:commodity/exchange ["Exchange is required"]})))

(deftest exchange-is-not-required-for-currencies
  (with-context commodity-context
    (assert-created #:commodity{:entity (find-entity "Personal")
                                 :name "US Dollar"
                                 :symbol "USD"
                                 :type :currency
                                 :price-config {:price-config/enabled false}})))

(deftest exchange-can-be-nasdaq
  (with-context commodity-context
    (assert-created (assoc (attributes)
                            :commodity/exchange :nasdaq
                            :commodity/symbol "AAPL"
                            :commodity/name "Apple"))))

(deftest exchange-can-be-nyse
  (with-context commodity-context
    (assert-created (assoc (attributes)
                            :commodity/exchange :nyse
                            :commodity/symbol "HD"
                            :commodity/name "Home Depot"))))

(deftest exchange-can-be-otc
  (with-context commodity-context
    (assert-created (assoc (attributes)
                           :commodity/exchange :otc
                           :commodity/symbol "GBTC"
                           :commodity/name "Grayscale Bitcoin Trust"))))

(deftest exchange-must-be-valid
  (with-context commodity-context
    (assert-invalid (assoc (attributes)
                           :commodity/exchange :not-valid
                           :commodity/symbol "NUNYA"
                           :commodity/name "None of your business")
                    {:commodity/exchange ["Exchange must be nasdaq, nyse, or otc"]})))

(deftest a-commodity-can-be-updated
  (with-context existing-context
    (let [commodity (find-commodity "AAPL")
          result (models/put (-> commodity
                                         (assoc :commodity/name "New name")
                                         (assoc-in [:commodity/price-config :price-config/enabled] false)))]
      (is (comparable? {:commodity/name "New name"
                        :commodity/price-config {:price-config/enabled false}}
                       result)
          "The return value has the updated attributes")
      (is (comparable? {:commodity/name "New name"
                        :commodity/price-config {:price-config/enabled false}}
                       (models/find commodity))
          "The retrieved value has the updated attributes"))))

(deftest a-commodity-can-be-deleted
  (with-context existing-context
    (assert-deleted (find-commodity "AAPL"))))

(def ^:private count-context
  (concat commodity-context
          [#:commodity{:name "Microsoft, Inc."
                       :entity "Personal"
                       :symbol "MSFT"
                       :type :stock
                       :exchange :nasdaq}
           #:commodity{:name "Apple, Inc."
                       :entity "Personal"
                       :symbol "AAPL"
                       :type :stock
                       :exchange :nasdaq}
           #:commodity{:name "United States Dollar"
                       :entity "Personal"
                       :symbol "USD"
                       :type :currency}
           #:commodity{:name "British Pound"
                       :entity "Business"
                       :symbol "GBP"
                       :type :currency}]))

(deftest get-a-count-of-commodities
  (with-context count-context
    (is  (= 3 (models/count {:commodity/entity (find-entity "Personal")})))))
