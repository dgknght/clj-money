(ns clj-money.models.prices-test
  (:require [clojure.test :refer [deftest use-fixtures is testing]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test-assertions]
            [clj-money.core]
            [clj-money.models :as models]
            [clj-money.model-helpers :as helpers :refer [assert-invalid
                                                         assert-updated]]
            [clj-money.db.sql.ref]
            [clj-money.models.ref]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-account
                                            find-entity
                                            find-commodity
                                            find-price]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models.prices :as prices]))

(use-fixtures :each reset-db)

(def ^:private price-context
  [(factory :user {:user/email "john@doe.com"})
   #:entity{:name "Personal"
            :user "john@doe.com"}
   #:entity{:name "Business"
            :user "john@doe.com"}
   #:commodity{:name "Apple"
               :symbol "AAPL"
               :type :stock
               :exchange :nasdaq
               :entity "Personal"}
   #:commodity{:name "US Dollar"
               :symbol "USD"
               :type :currency
               :entity "Personal"}])

(defn- attributes []
  #:price{:trade-date (t/local-date 2017 3 2)
          :price 12.34M
          :commodity (find-commodity "AAPL")})

(defn- assert-created
  [attr]
  (helpers/assert-created attr :refs [:price/commodity]))

(deftest create-a-price
  (with-context price-context
    (let [{:as attr :price/keys [trade-date]} (attributes)
          entity (find-entity "Personal")]
      (assert-created attr)
      (is (comparable? #:commodity{:earliest-price trade-date 
                                   :latest-price trade-date}
                       (models/find-by {:commodity/symbol "AAPL"
                                        :commodity/entity entity}))
          "The commodity price date range is updated")
      (is (comparable? #:settings{:earliest-price-date trade-date
                                  :latest-price-date trade-date}
                       (:entity/settings (models/find entity)))
          "The entity price date range is updated"))))

(deftest commodity-id-is-required
  (with-context price-context
    (assert-invalid (dissoc (attributes) :price/commodity)
                    {:price/commodity ["Commodity is required"]})))

(deftest trade-date-is-required
  (with-context price-context
    (assert-invalid (dissoc (attributes) :price/trade-date)
                    {:price/trade-date ["Trade date is required"]})))

(deftest trade-date-must-be-a-date
  (with-context price-context
    (assert-invalid (assoc (attributes) :price/trade-date "notadate")
                    {:price/trade-date ["Trade date is invalid"]})))

(def ^:private existing-price-context
  (conj price-context #:price{:commodity "AAPL"
                              :trade-date (t/local-date 2017 3 2)
                              :price 12.34M}))

(deftest trade-date-must-be-unique
  (with-context existing-price-context
    (assert-invalid (attributes)
                    {:price/trade-date ["Trade date already exists"]})))

(deftest price-is-required
  (with-context price-context
    (assert-invalid (dissoc (attributes) :price/price)
                    {:price/price ["Price is required"]})))

(deftest price-must-be-a-number
  (with-context price-context
    (assert-invalid (assoc (attributes) :price/price "notanumber")
                    {:price/price ["Price must be a number"]})))

(deftest update-a-price
  (with-context existing-price-context
    (assert-updated (find-price ["AAPL" (t/local-date 2017 3 2)])
                    {:price/price 10M})))

(deftest delete-a-price
  (with-context existing-price-context
    (let [price (find-price ["AAPL" (t/local-date 2017 3 2)])]
      (models/delete price)
      (is (nil? (models/find price))
          "The model is not retrieved after delete"))))

(def ^:private multi-price-context
  (conj price-context
        #:price{:commodity "AAPL"
                :trade-date (t/local-date 2017 2 27)
                :price 12.34M}
        #:price{:commodity "AAPL"
                :trade-date (t/local-date 2017 3 2)
                :price 12.20M}
        #:price{:commodity "AAPL"
                :trade-date (t/local-date 2017 3 1)
                :price 12.00M}))

(deftest get-the-most-recent-price-for-a-commodity
  (with-context multi-price-context
    (testing "When at least one price exists"
      (let [commodity (find-commodity "AAPL")
            price (prices/most-recent (models/find commodity))]
        (is (= 12.20M (:price/price price))
            "The most recent price is returned")))
    (testing "When no prices exist"
      (let [commodity (find-commodity "USD")
            price (prices/most-recent commodity)]
        (is (nil? price) "The nil is returned")))))

(deftest deleting-a-commodity-deletes-the-prices
  (with-context existing-price-context
    (let [commodity (find-commodity "AAPL")
          criteria #:price{:commodity commodity
                           :trade-date [:between
                                        (t/local-date 2016 1 1)
                                        (t/local-date 2017 12 31)]}
          prices-before (models/select criteria)
          _ (models/delete commodity)]
      (is (seq prices-before)
          "The commodity prices exist before delete")
      (is (empty? (models/select criteria))
          "The commodity prices are absent after delete"))))

(def ^:private account-meta-context
  (conj basic-context
        #:commodity{:name "Apple, Inc."
                    :type :stock
                    :symbol "AAPL"
                    :exchange :nasdaq
                    :entity "Personal"}
        #:account{:name "IRA"
                  :entity "Personal"
                  :type :asset}
        #:trade{:type :purchase
                :entity "Personal"
                :commodity "AAPL"
                :account "IRA"
                :date (t/local-date 2015 1 1)
                :shares 100M
                :value 1000M}))

(deftest creating-a-price-updates-account-meta-data
  (with-context account-meta-context
    (assert-created #:price{:commodity (find-commodity "AAPL")
                            :trade-date (t/local-date 2015 1 2)
                            :price 12M})
    (is (comparable? #:account{:value 1200M}
                     (models/find (find-account "IRA")))
        "The account value reflects the new price after the price is created")))

(def ^:private account-meta-context-for-update
  (conj account-meta-context
        #:price{:trade-date (t/local-date 2015 2 1)
                :commodity "AAPL"
                :price 12M}))

(deftest updating-a-price-updates-account-meta-data
  (with-context account-meta-context-for-update
    (is (= 1200M (:account/value (models/find-by {:account/name "AAPL"})))
        "The account value reflects the price before update")
    (-> (find-price ["AAPL" (t/local-date 2015 2 1)])
        (assoc :price/price 13M
               :price/trade-date (t/local-date 2016 1 1))
        models/put)
    (is (= 1300M (:account/value (models/find-by {:account/name "AAPL"})))
        "The account value reflects the previous price after update")
    (is (seq-of-maps-like? [#:price{:trade-date (t/local-date 2015 1 1)
                                    :price 10M}
                            #:price{:trade-date (t/local-date 2016 1 1)
                                    :price 13M}]
                           (models/select {:price/trade-date [:between
                                                              (t/local-date 2015 1 1)
                                                              (t/local-date 2016 12 31)]}
                                          {:sort [:price/trade-date]}))
        "The price is moved to the new partition without duplication")))

(deftest deleting-a-price-updates-account-meta-data
  (with-context account-meta-context-for-update
    (is (= 1200M (:account/value (models/find-by {:account/name "AAPL"})))
        "The account value reflects the price before delete")
    (let [price (find-price ["AAPL" (t/local-date 2015 2 1)])]
      (models/delete price)
      (is (= 1000M (:account/value (models/find-by {:account/name "AAPL"})))
        "The account value reflects the previous price after delete"))))
