(ns clj-money.api.lots-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [clj-factory.core :refer [factory]]
            [lambdaisland.uri :refer [map->query-string]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.test]
            [clj-money.json]
            [clj-money.factories.user-factory]
            [java-time.api :as t]
            [clj-money.api.test-helper :refer [parse-body
                                               request
                                               jsonize-decimals]]
            [clj-money.test-context :refer [with-context
                                            find-user
                                            find-account
                                            find-commodity]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private list-context
  [(factory :user {:user/email "john@doe.com"})
   (factory :user {:user/email "jane@doe.com"})
   #:entity{:name "Personal"
            :user "john@doe.com"}
   #:commodity{:name "US Dollar"
               :entity "Personal"
               :symbol "USD"
               :type :currency}
   #:commodity{:name "Some Fund"
               :entity "Personal"
               :symbol "FND"
               :exchange :nasdaq
               :type :fund}
   #:account{:name "IRA"
             :entity "Personal"
             :commodity "USD"
             :type :asset}
   #:account{:name "Opening Balances"
             :entity "Personal"
             :commodity "USD"
             :type :equity}
   #:transaction{:transaction-date (t/local-date 2016 1 1)
                 :entity "Personal"
                 :description "Opening Balances"
                 :debit-account "IRA"
                 :credit-account "Opening Balances"
                 :quantity 1000M}
   #:trade{:type :purchase
           :entity "Personal"
           :date (t/local-date 2016 2 1)
           :account "IRA"
           :commodity "FND"
           :shares 10M
           :value 50M}
   #:trade{:type :purchase
           :entity "Personal"
           :date (t/local-date 2016 3 1)
           :account "IRA"
           :commodity "FND"
           :shares 10M
           :value 60M}
   #:trade{:type :sale
           :entity "Personal"
           :date (t/local-date 2016 3 15)
           :account "IRA"
           :commodity "FND"
           :shares 5M
           :value 35M}])

(defn- get-lots-for-an-account
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (let [account (find-account "IRA")
        commodity (find-commodity "FND")]
    (-> (request :get (str (path :api
                                 :accounts
                                 (:id account)
                                 :lots)
                           "?"
                           (map->query-string
                             {:commodity-id (:id commodity)}))
                 :content-type content-type
                 :user (find-user email))
        app
        parse-body)))

(defn- assert-successful-get
  [{:as response :keys [parsed-body]}
   & {:keys [expected]
      :or {expected [#:lot{:purchase-date (t/local-date 2016 2 1)
                           :shares-purchased 10.0M
                           :purchase-price 5.0M
                           :shares-owned 5.0M}
                     #:lot{:purchase-date (t/local-date 2016 3 1)
                           :shares-purchased 10.0M
                           :purchase-price 6.0M
                           :shares-owned 10.0M}]}}]
  (is (http-success? response))
  (is (seq-of-maps-like? expected
                         (jsonize-decimals parsed-body))
      "The response body contains the lot data"))

(defn- assert-blocked-get
  [{:as response :keys [edn-body parsed-body]}]
  (is (http-success? response))
  (let [body (or parsed-body edn-body)]
    (is (empty? body) "The body is empty")))

(deftest a-user-can-get-lots-for-an-account-in-his-entity
  (with-context list-context
    (testing "default format"
      (assert-successful-get (get-lots-for-an-account "john@doe.com")))
    (testing "json format"
      (assert-successful-get
        (get-lots-for-an-account "john@doe.com"
                                 :content-type "application/json")
        :expected [{:purchaseDate "2016-02-01"
                    :sharesPurchased "10.00"
                    :purchasePrice "5.00"
                    :sharesOwned "5.00"
                    :_type "lot"}
                   {:purchaseDate "2016-03-01"
                    :sharesPurchased "10.00"
                    :purchasePrice "6.00"
                    :sharesOwned "10.00"
                    :_type "lot"}]))))

(deftest a-user-cannot-get-lots-for-an-account-in-anothers-entity
  (with-context list-context
    (assert-blocked-get (get-lots-for-an-account "jane@doe.com"))))

(defn- get-lots-for-multiple-accounts
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (let [ira (find-account "IRA")
        opening (find-account "Opening Balances")]
    (-> (request :get (str (path :api
                                 :lots)
                           "?"
                           (map->query-string {:account-id (map :id [ira opening])}))
                 :content-type content-type
                 :user (find-user email))
        app
        parse-body)))

(deftest a-user-can-get-lots-for-multiple-accounts-in-his-entity
  (with-context list-context
    (testing "default format"
      (assert-successful-get (get-lots-for-multiple-accounts "john@doe.com")))
    (testing "json format"
      (assert-successful-get
        (get-lots-for-multiple-accounts "john@doe.com"
                                        :content-type "application/json")
        :expected [{:purchaseDate "2016-02-01"
                    :sharesPurchased "10.00"
                    :purchasePrice "5.00"
                    :sharesOwned "5.00"
                    :_type "lot"}
                   {:purchaseDate "2016-03-01"
                    :sharesPurchased "10.00"
                    :purchasePrice "6.00"
                    :sharesOwned "10.00"
                    :_type "lot"}]))))

(deftest a-user-cannot-get-lots-for-multiple-accounts-in-anothers-entity
  (with-context list-context
    (assert-blocked-get (get-lots-for-multiple-accounts "jane@doe.com"))))
