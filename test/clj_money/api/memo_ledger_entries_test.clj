(ns clj-money.api.memo-ledger-entries-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [java-time.api :as t]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.test]
            [clj-money.json]
            [clj-money.factories.user-factory]
            [clj-money.api.test-helper :refer [parse-body
                                               request]]
            [clj-money.test-context :refer [with-context
                                            find-user
                                            find-lot
                                            find-memo-ledger-entry]]
            [clj-money.entities :as entities]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private base-context
  [(factory :user {:user/email "john@doe.com"})
   (factory :user {:user/email "jane@doe.com"})
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
   #:lot{:account "IRA"
         :commodity "AAPL"
         :purchase-price 150M
         :shares-purchased 10M
         :shares-owned 10M
         :purchase-date (t/local-date 2020 1 15)}])

(def ^:private list-context
  (conj base-context
        #:memo-ledger-entry{:lot ["IRA" "AAPL"]
                            :transaction-date (t/local-date 2021 6 1)
                            :memo "2-for-1 stock split"}))

(defn- list-memo-entries
  [email]
  (let [lot (find-lot ["IRA" "AAPL"])]
    (-> (request :get (path :api :lots (:id lot) :memo-ledger-entries)
                 :user (find-user email))
        app
        parse-body)))

(defn- assert-successful-list
  [{:as response :keys [parsed-body]}]
  (is (http-success? response))
  (is (seq-of-maps-like?
        [{:memo-ledger-entry/transaction-date (t/local-date 2021 6 1)
          :memo-ledger-entry/memo "2-for-1 stock split"}]
        parsed-body)
      "The response body contains the memo entry data"))

(defn- assert-blocked-list
  [{:as response :keys [parsed-body]}]
  (is (http-success? response))
  (is (empty? parsed-body) "The body is empty"))

(deftest a-user-can-list-memo-entries-for-his-lot
  (with-context list-context
    (assert-successful-list (list-memo-entries "john@doe.com"))))

(deftest a-user-cannot-list-memo-entries-for-anothers-lot
  (with-context list-context
    (assert-blocked-list (list-memo-entries "jane@doe.com"))))

(defn- create-memo-entry
  [email]
  (let [lot (find-lot ["IRA" "AAPL"])
        response (-> (request :post (path :api :lots (:id lot) :memo-ledger-entries)
                              :body #:memo-ledger-entry{:transaction-date (t/local-date 2022 3 1)
                                                        :memo "3-for-1 stock split"}
                              :user (find-user email))
                     app
                     parse-body)]
    [response
     (some-> response
             :parsed-body
             :id
             entities/find)]))

(defn- assert-successful-create
  [[{:keys [parsed-body] :as response} retrieved]]
  (is (http-created? response))
  (is (:id parsed-body) "An ID is assigned to the new record")
  (is (comparable? {:memo-ledger-entry/memo "3-for-1 stock split"}
                   retrieved)
      "The created entry can be retrieved"))

(defn- assert-blocked-create
  [[response retrieved]]
  (is (http-not-found? response))
  (is (nil? retrieved)))

(deftest a-user-can-create-a-memo-entry-for-his-lot
  (with-context base-context
    (assert-successful-create (create-memo-entry "john@doe.com"))))

(deftest a-user-cannot-create-a-memo-entry-for-anothers-lot
  (with-context base-context
    (assert-blocked-create (create-memo-entry "jane@doe.com"))))

(defn- delete-memo-entry
  [email]
  (let [entry (find-memo-ledger-entry ["IRA" "AAPL" "2-for-1 stock split"])
        response (-> (request :delete (path :api :memo-ledger-entries (:id entry))
                              :user (find-user email))
                     app)]
    [response (entities/find entry)]))

(defn- assert-successful-delete
  [[response retrieved]]
  (is (http-success? response))
  (is (nil? retrieved) "The entry cannot be retrieved after delete"))

(defn- assert-blocked-delete
  [[response retrieved]]
  (is (http-not-found? response))
  (is retrieved "The entry can still be retrieved after a blocked delete"))

(deftest a-user-can-delete-a-memo-entry-in-his-entity
  (with-context list-context
    (assert-successful-delete (delete-memo-entry "john@doe.com"))))

(deftest a-user-cannot-delete-a-memo-entry-in-anothers-entity
  (with-context list-context
    (assert-blocked-delete (delete-memo-entry "jane@doe.com"))))
