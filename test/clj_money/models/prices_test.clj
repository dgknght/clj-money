(ns clj-money.models.prices-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
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

(def ^:private price-context
  {:users [(factory :user)]
   :entities [{:name "Personal"}
              {:name "Business"}]
   :commodities [{:name "Apple"
                  :symbol "APPL"
                  :exchange :nasdaq}]})

(deftest a-price-can-be-addied-for-a-commodity
  (let [context (serialization/realize storage-spec price-context)
        commodity (-> context :commodities first)
        price (prices/create storage-spec {:commodity-id (:id commodity)
                                           :trade-date (t/local-date 2017 3 2)
                                           :price 12.34M})
        prices (prices/select-by-commodity-id storage-spec (:id commodity))]
    (is (integer? (:id price))
        "The result contains an ID value")
    (is (empty? (validation/error-messages price))
        "The result does not contain any validation errors")
    (is (seq (filter #(= (t/local-date 2017 3 2) (:trade-date %)) prices))
        "The price can be retrieved after create")))

(deftest commodity-id-is-required
  (let [context (serialization/realize storage-spec price-context)
        commodity (-> context :commodities first)
        price (prices/create storage-spec {:trade-date (t/local-date 2017 3 2)
                                           :price 12.34M})
        prices (prices/select-by-commodity-id storage-spec (:id commodity))]
    (is (nil? (:id price))
        "The result does not contain an ID value")
    (is (= ["Commodity id is required"] (validation/error-messages price :commodity-id))
        "The result contains a validation error")
    (is (not (seq (filter #(= (t/local-date 2017 3 2) (:trade-date %)) prices)))
        "The price cannot be retrieved after create")))

(deftest trade-date-is-required
  (let [context (serialization/realize storage-spec price-context)
        commodity (-> context :commodities first)
        price (prices/create storage-spec {:commodity-id (:id commodity)
                                           :price 12.34M})
        prices (prices/select-by-commodity-id storage-spec (:id commodity))]
    (is (nil? (:id price))
        "The result does not contain an ID value")
    (is (= ["Trade date is required"] (validation/error-messages price :trade-date))
        "The result contains a validation error")
    (is (not (seq (filter #(= (:id commodity) (:commodity-id %)) prices)))
        "The price cannot be retrieved after create")))

(deftest trade-date-must-be-a-date
  (let [context (serialization/realize storage-spec price-context)
        commodity (-> context :commodities first)
        price (prices/create storage-spec {:commodity-id (:id commodity)
                                           :trade-date "notadate"
                                           :price 12.34M})
        prices (prices/select-by-commodity-id storage-spec (:id commodity))]
    (is (nil? (:id price))
        "The result does not contain an ID value")
    (is (= ["Trade date must be an instance of class org.joda.time.LocalDate"] (validation/error-messages price :trade-date))
        "The result contains a validation error")
    (is (not (seq (filter #(= (:id commodity) (:commodity-id %)) prices)))
        "The price cannot be retrieved after create")))

(deftest trade-date-must-be-unique
  (let [context (serialization/realize storage-spec price-context)
        commodity (-> context :commodities first)
        price-1 (prices/create storage-spec {:commodity-id (:id commodity)
                                           :trade-date (t/local-date 2017 3 2)
                                           :price 12.34M})
        price-2 (prices/create storage-spec {:commodity-id (:id commodity)
                                           :trade-date (t/local-date 2017 3 2)
                                           :price 43.21M})
        prices (prices/select-by-commodity-id storage-spec (:id commodity))]
    (is (integer? (:id price-1))
        "The first result contains an ID value")
    (is (nil? (:id price-2))
        "The duplicate value does not receive an ID")
    (is (empty? (validation/error-messages price-1))
        "The first result does not contain any validation errors")
    (is (= ["Trade date must be unique"] (validation/error-messages price-2 :trade-date))
        "The duplicate value has an error message")
    (is (= [12.34M] (map :price prices))
        "The the duplicate price is not saved")))

(deftest trade-date-can-be-a-string-date
  (let [context (serialization/realize storage-spec price-context)
        commodity (-> context :commodities first)
        price (prices/create storage-spec {:commodity-id (:id commodity)
                                           :trade-date "2017-03-02"
                                           :price 12.34M})
        prices (prices/select-by-commodity-id storage-spec (:id commodity))]
    (is (integer? (:id price))
        "The result contains an ID value")
    (is (empty? (validation/error-messages price))
        "The result does not contain any validation errors")
    (is (seq (filter #(= (t/local-date 2017 3 2) (:trade-date %)) prices))
        "The price can be retrieved after create")))

(deftest price-is-required
  (let [context (serialization/realize storage-spec price-context)
        commodity (-> context :commodities first)
        price (prices/create storage-spec {:commodity-id (:id commodity)
                                           :trade-date (t/local-date 2017 3 2)})
        prices (prices/select-by-commodity-id storage-spec (:id commodity))]
    (is (nil? (:id price))
        "The result does not contain an ID value")
    (is (= ["Price is required"] (validation/error-messages price :price))
        "The result contains a validation error")
    (is (not (seq (filter #(= (:id commodity) (:commodity-id %)) prices)))
        "The price cannot be retrieved after create")))

(deftest price-must-be-a-number
  (let [context (serialization/realize storage-spec price-context)
        commodity (-> context :commodities first)
        price (prices/create storage-spec {:commodity-id (:id commodity)
                                           :trade-date (t/local-date 2017 3 2)
                                           :price "notanumber"})
        prices (prices/select-by-commodity-id storage-spec (:id commodity))]
    (is (nil? (:id price))
        "The result does not contain an ID value")
    (is (= ["Price must be an instance of class java.math.BigDecimal"] (validation/error-messages price :price))
        "The result contains a validation error")
    (is (not (seq (filter #(= (:id commodity) (:commodity-id %)) prices)))
        "The price cannot be retrieved after create")))

(deftest price-can-be-a-string-number
  (let [context (serialization/realize storage-spec price-context)
        commodity (-> context :commodities first)
        price (prices/create storage-spec {:commodity-id (:id commodity)
                                           :trade-date (t/local-date 2017 3 2)
                                           :price "12.34"})
        prices (prices/select-by-commodity-id storage-spec (:id commodity))]
    (is (integer? (:id price))
        "The result contains an ID value")
    (is (empty? (validation/error-messages price))
        "The result does not contain any validation errors")
    (is (seq (filter #(= (t/local-date 2017 3 2) (:trade-date %)) prices))
        "The price can be retrieved after create")))

(def ^:private existing-price-context
  (assoc price-context :prices [{:commodity-id "APPL"
                                 :trade-date (t/local-date 2017 3 2)
                                 :price 12.34M}]))

(deftest a-price-can-be-updated
  (let [context (serialization/realize storage-spec existing-price-context)
        price (-> context :prices first)
        result (prices/update storage-spec (assoc price :price "10"))
        retrieved (prices/find-by-id storage-spec (:id price))]
    (is (empty? (validation/error-messages result))
        "The result does not have any validation errors")
    (is (= 10.00M (:price retrieved))
        "The retrieved map has the correct values")))

(deftest a-price-can-be-deleted
  (let [context (serialization/realize storage-spec existing-price-context)
        price (-> context :prices first)
        _ (prices/delete storage-spec (:id price))
        prices (prices/select-by-commodity-id storage-spec (:commodity-id price))]
    (is (empty? (filter #(= (:id price) (:id %))  prices))
        "The result is not retrieved after delete")))
