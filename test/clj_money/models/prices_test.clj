(ns clj-money.models.prices-test
  (:require [clojure.test :refer [deftest use-fixtures is testing]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [clj-money.core]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [realize
                                            find-commodity]]
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
                  :symbol "AAPL"
                  :type :stock
                  :exchange :nasdaq}
                 {:name "US Dollar"
                  :symbol "USD"
                  :type :currency}]})

(deftest create-a-price
  (let [context (realize storage-spec price-context)
        commodity (find-commodity context "AAPL")
        price (prices/create storage-spec {:commodity-id (:id commodity)
                                           :trade-date (t/local-date 2017 3 2)
                                           :price 12.34M})
        actual (->> {:commodity-id (:id commodity)
                     :trade-date [:between
                                  (t/local-date 2017 3 1)
                                  (t/local-date 2017 3 31)]}
                    (prices/search storage-spec)
                    (map #(dissoc % :id :created-at :updated-at)))
        expected [{:commodity-id (:id commodity)
                   :trade-date (t/local-date 2017 3 2)
                   :price 12.34M}]
        updated-commodity (commodities/find-by-id storage-spec (:id commodity))]
    (is (:id price)
        "The result contains an ID value")
    (is (empty? (validation/error-messages price))
        "The result does not contain any validation errors")
    (when-not (= expected actual)
      (pprint {:expected expected
               :actual actual
               :diff (diff expected actual)}))
    (is (= expected actual)
        "The price can be retrieved after create")
    (is (= (t/local-date 2017 3 2)
            (:earliest-price updated-commodity))
        (= (t/local-date 2017 3 2)
           (:latest-price updated-commodity)))))

(deftest commodity-id-is-required
  (let [context (realize storage-spec price-context)
        commodity (find-commodity context "AAPL")
        price (prices/create storage-spec {:trade-date (t/local-date 2017 3 2)
                                           :price 12.34M})
        prices (prices/search storage-spec {:commodity-id (:id commodity)
                                            :trade-date [:between
                                                         (t/local-date 2017 3 1)
                                                         (t/local-date 2017 3 31)]})]
    (is (nil? (:id price))
        "The result does not contain an ID value")
    (is (= ["Commodity id is required"] (validation/error-messages price :commodity-id))
        "The result contains a validation error")
    (is (not (seq (filter #(= (t/local-date 2017 3 2) (:trade-date %)) prices)))
        "The price cannot be retrieved after create")))

(deftest trade-date-is-required
  (let [context (realize storage-spec price-context)
        commodity (find-commodity context "AAPL")
        price (prices/create storage-spec {:commodity-id (:id commodity)
                                           :price 12.34M})
        prices (prices/search storage-spec {:commodity-id (:id commodity)
                                            :trade-date [:between
                                                         (t/local-date 2017 1 1)
                                                         (t/local-date 2017 12 31)]})]
    (is (nil? (:id price))
        "The result does not contain an ID value")
    (is (= ["Trade date is required"] (validation/error-messages price :trade-date))
        "The result contains a validation error")
    (is (not (seq (filter #(= (:id commodity) (:commodity-id %)) prices)))
        "The price cannot be retrieved after create")))

(deftest trade-date-must-be-a-date
  (let [context (realize storage-spec price-context)
        commodity (find-commodity context "AAPL")
        price (prices/create storage-spec {:commodity-id (:id commodity)
                                           :trade-date "notadate"
                                           :price 12.34M})
        prices (prices/search storage-spec {:commodity-id (:id commodity)
                                            :trade-date [:between
                                                         (t/local-date 2017 1 1)
                                                         (t/local-date 2017 12 31)]})]
    (is (nil? (:id price))
        "The result does not contain an ID value")
    (is (= ["Trade date must be a date"] (validation/error-messages price :trade-date))
        "The result contains a validation error")
    (is (not (seq (filter #(= (:id commodity) (:commodity-id %)) prices)))
        "The price cannot be retrieved after create")))

(deftest trade-date-must-be-unique
  (let [context (realize storage-spec price-context)
        commodity (find-commodity context "AAPL")
        price-1 (prices/create storage-spec {:commodity-id (:id commodity)
                                           :trade-date (t/local-date 2017 3 2)
                                           :price 12.34M})
        price-2 (prices/create storage-spec {:commodity-id (:id commodity)
                                           :trade-date (t/local-date 2017 3 2)
                                           :price 43.21M})
        prices (prices/search storage-spec {:commodity-id (:id commodity)
                                            :trade-date [:between
                                                         (t/local-date 2017 1 1)
                                                         (t/local-date 2017 12 31)]})]
    (is (:id price-1)
        "The first result contains an ID value")
    (is (nil? (:id price-2))
        "The duplicate value does not receive an ID")
    (is (empty? (validation/error-messages price-1))
        "The first result does not contain any validation errors")
    (is (= ["Trade date must be unique"] (validation/error-messages price-2 :trade-date))
        "The duplicate value has an error message")
    (is (= [12.34M] (map :price prices))
        "The the duplicate price is not saved")))

(deftest price-is-required
  (let [context (realize storage-spec price-context)
        commodity (find-commodity context "AAPL")
        price (prices/create storage-spec {:commodity-id (:id commodity)
                                           :trade-date (t/local-date 2017 3 2)})
        prices (prices/search storage-spec {:commodity-id (:id commodity)
                                            :trade-date [:between
                                                         (t/local-date 2017 3 1)
                                                         (t/local-date 2017 3 31)]})]
    (is (nil? (:id price))
        "The result does not contain an ID value")
    (is (= ["Price is required"] (validation/error-messages price :price))
        "The result contains a validation error")
    (is (not (seq (filter #(= (:id commodity) (:commodity-id %)) prices)))
        "The price cannot be retrieved after create")))

(deftest price-must-be-a-number
  (let [context (realize storage-spec price-context)
        commodity (find-commodity context "AAPL")
        price (prices/create storage-spec {:commodity-id (:id commodity)
                                           :trade-date (t/local-date 2017 3 2)
                                           :price "notanumber"})
        prices (prices/search storage-spec {:commodity-id (:id commodity)
                                            :trade-date [:between
                                                         (t/local-date 2017 3 1)
                                                         (t/local-date 2017 3 31)]})]
    (is (nil? (:id price))
        "The result does not contain an ID value")
    (is (= ["Price must be a decimal"] (validation/error-messages price :price))
        "The result contains a validation error")
    (is (not (seq (filter #(= (:id commodity) (:commodity-id %)) prices)))
        "The price cannot be retrieved after create")))

(def ^:private existing-price-context
  (assoc price-context :prices [{:commodity-id "AAPL"
                                 :trade-date (t/local-date 2017 3 2)
                                 :price 12.34M}]))

(deftest a-price-can-be-updated
  (let [context (realize storage-spec existing-price-context)
        price (-> context :prices first)
        result (prices/update storage-spec (assoc price :price 10M))
        retrieved (prices/find-by-id storage-spec (:id price) (:trade-date price))]
    (is (empty? (validation/error-messages result))
        "The result does not have any validation errors")
    (is (= 10.00M (:price retrieved))
        "The retrieved map has the correct values")))

(deftest a-price-can-be-deleted
  (let [context (realize storage-spec existing-price-context)
        price (-> context :prices first)
        _ (prices/delete storage-spec price)
        prices (prices/search storage-spec {:commodity-id (:commodity-id price)
                                            :trade-date (:trade-date price)})]
    (is (empty? (filter #(= (:id price) (:id %))  prices))
        "The result is not retrieved after delete")))

(def ^:private multi-price-context
  (assoc price-context :prices [{:commodity-id "AAPL"
                                 :trade-date (t/local-date 2017 2 27)
                                 :price 12.34M}
                                {:commodity-id "AAPL"
                                 :trade-date (t/local-date 2017 3 2)
                                 :price 12.20M}
                                {:commodity-id "AAPL"
                                 :trade-date (t/local-date 2017 3 1)
                                 :price 12.00M}]))

(deftest get-the-most-recent-price-for-a-commodity
  (let [context (realize storage-spec multi-price-context)]
    (testing "When at least one price exists"
      (let [commodity (commodities/find-by-id
                        storage-spec
                        (:id (find-commodity context "AAPL")))
            price (prices/most-recent storage-spec commodity)]
        (is (= 12.20M (:price price)) "The most recent price is returned")))
    (testing "When no prices exist"
      (let [commodity (find-commodity context "USD")
            price (prices/most-recent storage-spec commodity)]
        (is (nil? price) "The nil is returned")))))
