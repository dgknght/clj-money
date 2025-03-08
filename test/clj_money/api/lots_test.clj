(ns clj-money.api.lots-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [ring.mock.request :as req]
            [clj-factory.core :refer [factory]]
            [lambdaisland.uri :refer [map->query-string]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [clj-money.factories.user-factory]
            [java-time.api :as t]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.test-context :refer [with-context
                                            find-user
                                            find-account
                                            find-commodity]]
            [clj-money.test-helpers :refer [reset-db
                                            parse-edn-body]]
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
  [email]
  (with-context list-context
    (let [account (find-account "IRA")
          commodity (find-commodity "FND")]
      (-> (req/request :get (str (path :api
                                       :accounts
                                       (:id account)
                                       :lots)
                                 "?"
                                 (map->query-string
                                   {:commodity-id (:id commodity)})))
          (add-auth (find-user email))
          app
          parse-edn-body))))

(defn- assert-successful-get
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (seq-of-maps-like? [#:lot{:purchase-date "2016-02-01"
                                :shares-purchased 10.0
                                :purchase-price 5.0
                                :shares-owned 5.0}
                          #:lot{:purchase-date "2016-03-01"
                                :shares-purchased 10.0
                                :purchase-price 6.0
                                :shares-owned 10.0}]
                         edn-body)
      "The response body contains the lot data"))

(defn- assert-blocked-get
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (empty? edn-body) "The body is empty"))

(deftest a-user-can-get-lots-for-an-account-in-his-entity
  (assert-successful-get (get-lots-for-an-account "john@doe.com")))

(deftest a-user-cannot-get-lots-for-an-account-in-anothers-entity
  (assert-blocked-get (get-lots-for-an-account "jane@doe.com")))

(defn- get-lots-for-multiple-accounts
  [email]
  (with-context list-context
    (let [ira (find-account "IRA")
          opening (find-account "Opening Balances")]
      (-> (req/request :get (str (path :api
                                       :lots)
                                 "?"
                                 (map->query-string {:account-id (map :id [ira opening])})))
          (add-auth (find-user email))
          app
          parse-edn-body))))

(deftest a-user-can-get-lots-for-multiple-accounts-in-his-entity
  (assert-successful-get (get-lots-for-multiple-accounts "john@doe.com")))

(deftest a-user-cannot-get-lots-for-multiple-accounts-in-anothers-entity
  (assert-blocked-get (get-lots-for-multiple-accounts "jane@doe.com")))
