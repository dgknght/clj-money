(ns clj-money.models.lots-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [java-time.api :as t]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test_assertions]
            [clj-money.util :as util]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            find-account
                                            find-commodity]]
            [clj-money.model-helpers :refer [assert-invalid
                                             assert-created]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models.lots :as lots]))

(use-fixtures :each reset-db)

(def ^:private lot-context
  [(factory :user {:user/email "john@doe.com"})
   #:entity{:name "Personal"
            :user "john@doe.com"}
   #:commodity{:name "US Dollar"
               :entity "Personal"
               :symbol "USD"
               :type :currency}
   #:commodity{:name "Apple"
               :entity "Personal"
               :symbol "AAPL"
               :exchange :nasdaq
               :type :stock}
   #:account{:name "IRA"
             :entity "Personal"
             :type :asset
             :commodity "USD"}
   #:account{:name "Dining"
             :entity "Personal"
             :type :expense
             :commodity "USD"}
   #:account{:name "Checking"
             :entity "Personal"
             :type :asset
             :commodity "USD"}])

(defn- attributes []
  #:lot{:commodity (find-commodity "AAPL")
        :account (find-account "IRA")
        :purchase-date (t/local-date 2017 3 2)
        :purchase-price 10M
        :shares-purchased 100M})

(deftest create-a-lot
  (with-context lot-context
    (assert-created (attributes) :refs [:lot/commodity :lot/account])))

(deftest commodity-id-is-required
  (with-context lot-context
    (assert-invalid (dissoc (attributes) :lot/commodity)
                    {:lot/commodity ["Commodity is required"]})))

(deftest account-id-is-required
  (with-context lot-context
    (assert-invalid (dissoc (attributes) :lot/account)
                    {:lot/account ["Account is required"]})))

(deftest purchase-price-is-required
  (with-context lot-context
    (assert-invalid (dissoc (attributes) :lot/purchase-price)
                    {:lot/purchase-price ["Purchase price is required"]})))

(deftest account-id-must-reference-an-asset-account
  (with-context lot-context
    (assert-invalid (assoc (attributes) :lot/account (find-account "Dining"))
                    {:lot/account ["Account must be an asset"]})))

; (deftest purchase-date-is-required
;   (let [context (realize lot-context)
;         commodity (-> context :commodities first)
;         result (lots/create (-> context
;                                 attributes
;                                 (dissoc :purchase-date)))
;         lots (lots/select-by-commodity-id (:id commodity))]
;     (is (nil? (:id result)) "The result does not receive an ID value")
;     (is (invalid? result [:purchase-date] "Purchase date is required"))
;     (is (empty? lots) "The value is not retrieved after create")))
; 
; (deftest purchase-date-must-be-a-date
;   (let [context (realize lot-context)
;         commodity (find-commodity context "IRA")
;         result (lots/create (-> context
;                                 attributes
;                                 (assoc :purchase-date "not-a-date")))
;         lots (lots/select-by-commodity-id (:id commodity))]
;     (is (nil? (:id result)) "The result does not receive an ID value")
;     (is (invalid? result [:purchase-date] "Purchase date is invalid"))
;     (is (empty? lots) "The value is not retrieved after create")))
; 
; (deftest shares-purchased-is-required
;   (let [context (realize lot-context)
;         commodity (find-commodity context "IRA")
;         result (lots/create (-> context
;                                 attributes
;                                 (dissoc :shares-purchased)))
;         lots (lots/select-by-commodity-id (:id commodity))]
;     (is (nil? (:id result)) "The result does not receive an ID value")
;     (is (invalid? result [:shares-purchased] "Shares purchased is required"))
;     (is (empty? lots) "The value is not retrieved after create")))
; 
; (def ^:private existing-lot-context
;   (assoc lot-context :lots [{:account-id "IRA"
;                              :commodity-id "AAPL"
;                              :purchase-price 10M
;                              :shares-purchased 100M
;                              :shares-owned 100M
;                              :purchase-date (t/local-date 2016 3 2)}]))
; 
; (deftest update-a-lot
;   (let [context (realize existing-lot-context)
;         lot (-> context :lots first)
;         updated (update-in lot [:shares-owned] #(- % 30M))
;         result (lots/update updated)
;         retrieved (lots/find lot)]
;     (is (valid? result))
;     (is (= 70M (:shares-owned retrieved))
;         "The retrieved map contains the updated value")))
; 
; (deftest search-lots-by-account
;   (let [context (realize existing-lot-context)
;         ira (find-account context "IRA")
;         commodity (->> context
;                        :commodities
;                        (filter #(= "AAPL" (:symbol %)))
;                        first)
;         actual (map #(dissoc % :updated-at :created-at :id)
;                     (lots/search {:account-id (:id ira)}))
;         expected [{:commodity-id (:id commodity)
;                    :account-id (:id ira)
;                    :purchase-date (t/local-date 2016 3 2)
;                    :purchase-price 10M
;                    :shares-purchased 100M
;                    :shares-owned 100M}]]
;     (is (= expected actual) "The correct data is returned")))
; 
; ; Test unrealized-gains with:
; ;   Date that precedes some purchases
; ;   Date that precedes some sales
