(ns clj-money.api.transfers-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [clojure.pprint :refer [pprint]]
            [clj-factory.core :refer [factory]]
            [java-time.api :as t]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.test]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.factories.user-factory]
            [clj-money.api.test-helper :refer [parse-body
                                               request]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.test-context :refer [with-context
                                            find-entity
                                            find-user
                                            find-account
                                            find-commodity]]
            [clj-money.web.test-handler :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private base-context
  [(factory :user {:user/email "john@doe.com"})
   (factory :user {:user/email "jane@doe.com"})
   #:entity{:name "Personal"
            :user "john@doe.com"}
   #:entity{:name "Jane's"
            :user "jane@doe.com"}
   #:commodity{:symbol "AAPL"
               :entity "Personal"
               :name "Apple, Inc."
               :type :stock
               :exchange :nasdaq}
   #:commodity{:symbol "USD"
               :entity "Personal"
               :name "US Dollar"
               :type :currency}
   #:commodity{:symbol "USD"
               :entity "Jane's"
               :name "US Dollar"
               :type :currency}
   #:account{:name "IRA"
             :type :asset
             :entity "Personal"
             :commodity "USD"}
   #:account{:name "IRA 2"
             :type :asset
             :entity "Personal"
             :commodity "USD"}
   #:account{:name "Jane's IRA"
             :type :asset
             :entity "Jane's"
             :commodity "USD"}
   #:account{:name "Opening Balances"
             :type :equity
             :entity "Personal"
             :commodity "USD"}
   #:transaction{:transaction-date (t/local-date 2016 1 1)
                 :entity "Personal"
                 :description "Opening balances"
                 :debit-account "IRA"
                 :credit-account "Opening Balances"
                 :quantity 1000M}])

(def ^:private transfer-context
  (conj base-context
        #:trade{:entity "Personal"
                :date (t/local-date 2016 2 27)
                :type :purchase
                :account "IRA"
                :commodity "AAPL"
                :shares 100M
                :value 1000M}))

(def ^:private transfer-without-lots-context
  (conj base-context
        #:price{:commodity "AAPL"
                :trade-date (t/local-date 2016 2 27)
                :value 10M}))

(defn- transfer-a-commodity
  [email & {:keys [content-type to-account]
            :or {content-type "application/edn"
                 to-account "IRA 2"}}]
  (let [user (find-user email)
        entity (find-entity "Personal")
        aapl (find-commodity "AAPL")
        ira (find-account "IRA")
        to (find-account to-account)
        attr (if (= content-type "application/json")
               {:date "2016-03-02"
                :shares 100
                :commodity (util/->entity-ref aapl)
                :fromAccount (util/->entity-ref ira)
                :toAccount (util/->entity-ref to)
                :_type "transfer"}
               #:transfer{:date (t/local-date 2016 3 2)
                          :shares 100M
                          :commodity (util/->entity-ref aapl)
                          :from-account (util/->entity-ref ira)
                          :to-account (util/->entity-ref to)})]
    [(-> (request :post (path :api
                              :entities
                              (:id entity)
                              :transfers)
                  :content-type content-type
                  :body attr
                  :user user)
         app
         parse-body)
     (entities/select #:transaction{:entity entity
                                    :transaction-date (t/local-date 2016 3 2)})
     (entities/select {:lot/commodity aapl})]))

(def ^:private expected-transaction
  #:transaction{:transaction-date (t/local-date 2016 3 2)
                :description "Transfer 100 shares of AAPL"})

(defn- assert-successful-transfer
  [[{:as response :keys [parsed-body]} trxs lots]
   & {:keys [expected-response]
      :or {expected-response {:transfer/transaction expected-transaction}}}]
  (is (http-created? response))
  (is (comparable? expected-response parsed-body)
      "The created transaction is included in the response")
  (is (seq-of-maps-like? [expected-transaction] trxs)
      "The created transaction can be retrieved")
  (is (seq-of-maps-like? [#:lot{:account (util/->entity-ref (find-account "IRA 2"))
                                :shares-owned 100M}]
                         lots)
      "The lot is moved to the destination account"))

(defn- assert-blocked-transfer
  [[response trxs lots]]
  (is (http-not-found? response))
  (is (empty? trxs)
      "No transaction is created")
  (is (seq-of-maps-like? [#:lot{:account (util/->entity-ref (find-account "IRA"))
                                :shares-owned 100M}]
                         lots)
      "The lot stays in the source account"))

(deftest a-user-can-transfer-a-commodity-in-his-entity
  (with-context transfer-context
    (assert-successful-transfer (transfer-a-commodity "john@doe.com"))))

(deftest a-user-can-transfer-a-commodity-in-his-entity-with-json
  (with-context transfer-context
    (assert-successful-transfer
      (transfer-a-commodity "john@doe.com" :content-type "application/json")
      :expected-response {:transaction {:transactionDate "2016-03-02"
                                        :description "Transfer 100 shares of AAPL"}})))

(deftest a-user-cannot-transfer-a-commodity-in-anothers-entity
  (with-context transfer-context
    (assert-blocked-transfer
      (transfer-a-commodity "jane@doe.com" :to-account "Jane's IRA"))))

(deftest a-user-cannot-transfer-a-commodity-into-anothers-entity
  (with-context transfer-context
    (assert-blocked-transfer
      (transfer-a-commodity "john@doe.com" :to-account "Jane's IRA"))))

(deftest a-transfer-without-lots-is-rejected
  (with-context transfer-without-lots-context
    (let [[response trxs] (transfer-a-commodity "john@doe.com")]
      (is (http-bad-request? response))
      (is (empty? trxs)
          "No transaction is created"))))

(deftest a-transfer-to-a-commodity-account-is-rejected
  (with-context transfer-context
    (let [aapl-account (entities/find-by #:account{:name "AAPL"
                                                   :parent (find-account "IRA")})
          [response trxs lots] (-> (request :post (path :api
                                                        :entities
                                                        (:id (find-entity "Personal"))
                                                        :transfers)
                                            :body #:transfer{:date (t/local-date 2016 3 2)
                                                             :shares 100M
                                                             :commodity (util/->entity-ref (find-commodity "AAPL"))
                                                             :from-account (util/->entity-ref (find-account "IRA"))
                                                             :to-account (util/->entity-ref aapl-account)}
                                            :user (find-user "john@doe.com"))
                                   app
                                   parse-body
                                   (vector (entities/select #:transaction{:entity (find-entity "Personal")
                                                                          :transaction-date (t/local-date 2016 3 2)})
                                           (entities/select {:lot/commodity (find-commodity "AAPL")})))]
      (is (http-bad-request? response))
      (is (empty? trxs)
          "No transaction is created")
      (is (seq-of-maps-like? [#:lot{:account (util/->entity-ref (find-account "IRA"))}]
                             lots)
          "The lot stays in the source account"))))
