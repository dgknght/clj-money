(ns clj-money.api.audit-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [java-time.api :as t]
            [lambdaisland.uri :refer [map->query-string]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.test]
            [clj-money.factories.user-factory]
            [clj-money.api.test-helper :refer [parse-body request]]
            [clj-money.test-context :refer [basic-context
                                            with-context
                                            find-user
                                            find-lot
                                            find-transaction-item]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private lot-context
  (conj basic-context
        #:commodity{:name "Apple"
                    :entity "Personal"
                    :symbol "AAPL"
                    :exchange :nasdaq
                    :type :stock}
        #:account{:name "IRA"
                  :entity "Personal"
                  :type :asset
                  :commodity "USD"
                  :system-tags #{:trading}}
        #:lot{:account "IRA"
              :commodity "AAPL"
              :purchase-price 150M
              :shares-purchased 10M
              :shares-owned 10M
              :purchase-date (t/local-date 2020 1 15)}))

(def ^:private transaction-item-context
  (conj basic-context
        #:transaction{:transaction-date (t/local-date 2020 1 1)
                      :entity "Personal"
                      :description "Paycheck"
                      :quantity 1000M
                      :debit-account "Checking"
                      :credit-account "Salary"}))

(defn- get-lot-audit
  [email]
  (let [lot (find-lot ["IRA" "AAPL" (t/local-date 2020 1 15)])]
    (-> (request :get (str (path :api :entities (:id lot) :audit)
                           "?"
                           (map->query-string {:attr "lot/shares-owned"}))
                 :user (find-user email))
        app
        parse-body)))

(defn- get-transaction-item-audit
  [email]
  (let [item (find-transaction-item [(t/local-date 2020 1 1) 1000M "Checking"])]
    (-> (request :get (str (path :api :entities (:id item) :audit)
                           "?"
                           (map->query-string {:attr "transaction-item/memo"}))
                 :user (find-user email))
        app
        parse-body)))

(deftest a-user-can-get-lot-audit
  (with-context lot-context
    (let [{:as response :keys [parsed-body]} (get-lot-audit "john@doe.com")]
      (is (http-success? response))
      (is (sequential? parsed-body)
          "The response body is a sequence"))))

(deftest a-user-cannot-get-lot-audit-for-anothers-entity
  (with-context lot-context
    (is (http-not-found? (get-lot-audit "jane@doe.com")))))

(deftest a-user-can-get-transaction-item-audit
  (with-context transaction-item-context
    (let [{:as response :keys [parsed-body]} (get-transaction-item-audit "john@doe.com")]
      (is (http-success? response))
      (is (sequential? parsed-body)
          "The response body is a sequence"))))

(deftest a-user-cannot-get-transaction-item-audit-for-anothers-entity
  (with-context transaction-item-context
    (is (http-not-found? (get-transaction-item-audit "jane@doe.com")))))
