(ns clj-money.api.scheduled-transactions-test
  (:require [clojure.test :refer [use-fixtures deftest is]]
            [clojure.pprint :refer [pprint]]
            [ring.mock.request :as req]
            [java-time.api :as t]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.dates :as dates :refer [with-fixed-time]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user
                                            find-entity
                                            find-account
                                            find-scheduled-transaction]]
            [clj-money.entities.transactions :as trxs]
            [clj-money.test-helpers :refer [reset-db
                                            edn-body
                                            parse-edn-body]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private sched-trans-context
  (conj basic-context
        #:scheduled-transaction{:entity "Personal"
                                :last-occurrence (t/local-date 2015 3 2)
                                :start-date (t/local-date 2004 3 2)
                                :enabled true
                                :date-spec {:day 2
                                            :month 3}
                                :period [1 :year]
                                :description "Landlord"
                                :memo "automatically created"
                                :items [#:scheduled-transaction-item{:action :credit
                                                                     :account "Checking"
                                                                     :quantity 50M
                                                                     :memo "checking"}
                                        #:scheduled-transaction-item{:action :debit
                                                                     :account "Rent"
                                                                     :quantity 50M
                                                                     :memo "rent"}]}))

(defn- get-list
  [user-email]
  (with-context sched-trans-context
    (let [entity (find-entity "Personal")]
      (-> (req/request :get (path :api
                                  :entities
                                  (:id entity)
                                  :scheduled-transactions))
          (add-auth (find-user user-email))
          app
          parse-edn-body))))

(defn- assert-successful-list
  [{:keys [edn-body] :as response}]
  (is (http-success? response))
  (is (seq-of-maps-like?
        [#:scheduled-transaction{:start-date (t/local-date 2004 03 02)
                                 :description "Landlord"
                                 :memo "automatically created"
                                 :period [1 :year]
                                 :items [#:scheduled-transaction-item{:action :credit
                                                                      :quantity 50M
                                                                      :memo "checking"}
                                         #:scheduled-transaction-item{:action :debit
                                                                      :quantity 50M
                                                                      :memo "rent"}]}]
        edn-body)
      "The body contains the existing scheduled transactions"))

(defn- assert-blocked-list
  [{:keys [edn-body] :as response}]
  (is (http-success? response))
  (is (empty? edn-body)))

(deftest a-user-can-get-a-list-of-scheduled-transactions-in-his-entity
  (assert-successful-list (get-list "john@doe.com")))

(deftest a-user-cannot-get-a-list-of-scheduled-transactions-for-anothers-entity
  (assert-blocked-list (get-list "jane@doe.com")))

(defn- attr []
  #:scheduled-transaction{:description "Paycheck"
                          :start-date (t/local-date 2021 01 01)
                          :date-spec {:days #{:friday}}
                          :period [2 :week]
                          :memo "biweekly"
                          :items [#:scheduled-transaction-item{:action :debit
                                                               :quantity 1000M
                                                               :account (util/->entity-ref (find-account "Checking"))
                                                               :memo "checking"}
                                  #:scheduled-transaction-item{:action :credit
                                                               :quantity 1000M
                                                               :account (util/->entity-ref (find-account "Salary"))
                                                               :memo "salary"}]})

(defn- create-sched-tran
  [user-email]
  (let [entity (find-entity "Personal")
        response (-> (req/request :post (path :api
                                              :entities
                                              (:id entity)
                                              :scheduled-transactions))
                     (edn-body (attr))
                     (add-auth (find-user user-email))
                     app
                     parse-edn-body)]
    [response
     (when-let [id (get-in response [:edn-body :id])]
       (entities/find id :scheduled-transaction))]))

(defn- assert-sched-tran-created
  [[{:keys [edn-body] :as response} retrieved]]
  (is (http-created? response))
  (is (:id edn-body) "The return value contains an :id")
  (let [expected #:scheduled-transaction{:description "Paycheck"
                                         :start-date (t/local-date 2021 1 1)
                                         :date-spec {:days #{:friday}}
                                         :period [2 :week]
                                         :memo "biweekly"
                                         :items [#:scheduled-transaction-item{:action :debit
                                                                              :quantity 1000M
                                                                              :memo "checking"}
                                                 #:scheduled-transaction-item{:action :credit
                                                                              :quantity 1000M
                                                                              :memo "salary"}]}]
    (is (comparable? expected edn-body)
        "The return value contains the created schedule transaction")
    (is (comparable? expected retrieved)
        "The scheduled transaction can be retrieved")))

(defn- assert-blocked-create
  [[response retrieved]]
  (is (http-not-found? response))
  (is (empty? retrieved) "The record is not created"))

(deftest a-user-can-create-a-scheduled-transaction-in-his-entity
  (with-context sched-trans-context
    (assert-sched-tran-created (create-sched-tran "john@doe.com"))))

(deftest a-user-cannot-create-a-scheduled-transaction-in-anothers-entity
  (with-context sched-trans-context
    (assert-blocked-create (create-sched-tran "jane@doe.com"))))

(def ^:private update-context
  (conj basic-context
        #:scheduled-transaction{:entity "Personal"
                                :description "Paycheck"
                                :enabled true
                                :period [1 :month]
                                :last-occurrence (t/local-date 2016 1 1)
                                :start-date (t/local-date 2015 1 1)
                                :date-spec {:day 1}
                                :items [#:scheduled-transaction-item{:action :debit
                                                                     :account "Checking"
                                                                     :quantity 1000M}
                                        #:scheduled-transaction-item{:action :credit
                                                                     :account "Salary"
                                                                     :quantity 1000M}]}))

(def ^:private update-attr
  #:scheduled-transaction{:period [2 :week]})

(defn- update-sched-tran
  [user-email]
  (with-context update-context
    (let [tran (find-scheduled-transaction "Paycheck")]
      [(-> (req/request :patch (path :api
                                     :scheduled-transactions
                                     (:id tran)))
           (edn-body update-attr)
           (add-auth (find-user user-email))
           app
           parse-edn-body)
       (entities/find tran)])))

(defn- assert-successful-update
  [[{:keys [edn-body] :as response} retrieved]]
  (is (http-success? response))
  (is (comparable? #:scheduled-transaction{:period [2 :week]}
                   edn-body)
      "The updated scheduled transaction is returned")
  (is (comparable? update-attr retrieved)
      "The database is updated correctly"))

(defn- assert-blocked-update
  [[response retrieved]]
  (is (http-not-found? response))
  (is (comparable? #:scheduled-transaction{:period [1 :month]}
         retrieved)
      "The database is not updated"))

(deftest a-user-can-edit-a-scheduled-transaction-in-his-entity
  (assert-successful-update (update-sched-tran "john@doe.com")))

(deftest a-user-cannot-edit-a-scheduled-transaction-in-anothers-entity
  (assert-blocked-update (update-sched-tran "jane@doe.com")))

(defn- delete-sched-tran
  [user-email]
  (with-context update-context
    (let [tran (find-scheduled-transaction "Paycheck")]
      [(-> (req/request :delete (path :api
                                      :scheduled-transactions
                                      (:id tran)))
           (add-auth (find-user user-email))
           app
           parse-edn-body)
       (entities/find tran)])))

(defn- assert-successful-delete
  [[response retrieved]]
  (is (http-no-content? response))
  (is (nil? retrieved) "The record cannot be retrieved after delete"))

(defn- assert-blocked-delete
  [[response retrieved]]
  (is (http-not-found? response))
  (is retrieved "The record can still be retrieved after blocked delete"))

(deftest a-user-can-delete-a-scheduled-transaction-in-his-entity
  (assert-successful-delete (delete-sched-tran "john@doe.com")))

(deftest a-user-cannot-delete-a-scheduled-transaction-in-anothers-entity
  (assert-blocked-delete (delete-sched-tran "jane@doe.com")))

(defn- realize-trans
  [user-email]
  (with-context update-context
    (let [sched-tran (find-scheduled-transaction "Paycheck")]
      [(with-fixed-time "2016-02-02T00:00:00Z"
         (-> (req/request :post (path :api
                                      :scheduled-transactions
                                      (:id sched-tran)
                                      :realize))
             (add-auth (find-user user-email))
             app
             parse-edn-body))
       (trxs/append-items
         (entities/select
           #:transaction{:transaction-date [:between>
                                            (t/local-date 2016 1 1)
                                            (t/local-date 2017 1 1)]
                         :entity (:scheduled-transaction/entity sched-tran)
                         :description "Paycheck"}))
       (entities/find sched-tran)])))

(defn- assert-successful-realization
  [[response trxs retrieved]]
  (is (http-created? response))
  (is (= 1 (count trxs))
      "One transaction is created.")
  (is (seq-of-maps-like?
        [#:transaction{:description "Paycheck"
                       :transaction-date (t/local-date 2016 2 1)}]
        trxs)
      "The transaction is created with the correct attributes.")
  (is (= #{#:transaction-item{:action :debit
                              :quantity 1000M}
           #:transaction-item{:action :credit
                              :quantity 1000M}}
         (->> (:transaction/items (first trxs))
              (map #(select-keys % [:transaction-item/action
                                    :transaction-item/quantity]))
              (into #{})))
      "The transaction is created with the correct line items.")
  (is (comparable? #:scheduled-transaction{:last-occurrence (t/local-date 2016 2 1)}
                   retrieved)
      "The scheduled trx record is updated with the last occurrence"))

(defn- assert-blocked-realization
  [[response trxs retrieved]]
  (is (http-not-found? response))
  (is (empty? trxs) "The transaction is not created.")
  (is (comparable? #:scheduled-transaction{:last-occurrence (t/local-date 2016 1 1)}
                   retrieved)
      "The scheduled trx record is not updated with the last occurrence"))

(deftest a-user-can-realize-a-scheduled-transaction-in-his-entity
  (assert-successful-realization (realize-trans "john@doe.com")))

(deftest a-user-cannot-realize-a-scheduled-transaction-in-anothers-entity
  (assert-blocked-realization (realize-trans "jane@doe.com")))

(def ^:private mass-realize-context
  (conj update-context
        #:scheduled-transaction{:entity "Personal"
                                :description "Groceries"
                                :period [1 :week]
                                :last-occurrence (t/local-date 2016 1 24)
                                :start-date (t/local-date 2015 1 1)
                                :enabled true
                                :date-spec {:days #{:sunday} }
                                :items [#:scheduled-transaction-item{:action :debit
                                                                     :account "Groceries"
                                                                     :quantity 100M}
                                        #:scheduled-transaction-item{:action :credit
                                                                     :account "Checking"
                                                                     :quantity 100M}]}
        #:scheduled-transaction{:entity "Personal"
                                :description "Groceries disabled"
                                :period [1 :week]
                                :last-occurrence (t/local-date 2016 1 24)
                                :start-date (t/local-date 2015 1 1)
                                :enabled false
                                :date-spec {:days #{:sunday} }
                                :items [#:scheduled-transaction-item{:action :debit
                                                                     :account "Groceries"
                                                                     :quantity 100M}
                                        #:scheduled-transaction-item{:action :credit
                                                                     :account "Checking"
                                                                     :quantity 100M}]}
        #:scheduled-transaction{:entity "Personal"
                                :description "Groceries after end date"
                                :period [1 :week]
                                :last-occurrence (t/local-date 2016 1 24)
                                :start-date (t/local-date 2015 1 1)
                                :end-date (t/local-date 2015 12 31)
                                :enabled true
                                :date-spec {:days #{:sunday} }
                                :items [#:scheduled-transaction-item{:action :debit
                                                                     :account "Groceries"
                                                                     :quantity 100M}
                                        #:scheduled-transaction-item{:action :credit
                                                                     :account "Checking"
                                                                     :quantity 100M}]}))

(defn- realize-all-trans
  [user-email]
  (with-context mass-realize-context
    (let [entity (find-entity "Personal")]
      [(with-fixed-time "2016-02-02T00:00:00Z"
         (-> (req/request :post (path :api
                                      :entities
                                      (:id entity)
                                      :scheduled-transactions
                                      :realize))
             (add-auth (find-user user-email))
             app
             parse-edn-body))
       (trxs/append-items
         (entities/select #:transaction{:transaction-date [:between>
                                                         (t/local-date 2016 1 1)
                                                         (t/local-date 2017 1 1)]
                                      :entity entity}
                        {:sort [:transaction/transaction-date]}))])))

(defn- assert-successful-mass-realization
  [[{:as response :keys [edn-body]} retrieved]]
  (is (http-created? response))
  (let [expected [#:transaction{:description "Groceries"
                                :transaction-date (t/local-date 2016 1 31)}
                  #:transaction{:description "Paycheck"
                                :transaction-date (t/local-date 2016 2 1)}
                  #:transaction{:description "Groceries"
                                :transaction-date (t/local-date 2016 2 7)}]]
    (is (seq-of-maps-like? expected edn-body)
        "The created transactions are returned.")
    (is (seq-of-maps-like? expected retrieved)
        "The transactions can be retrieved.")))

(defn- assert-blocked-mass-realization
  [[response retrieved]]
  (is (http-not-found? response))
  (is (empty? retrieved) "No transactions are created."))

(deftest a-user-can-realize-all-scheduled-transactions-in-his-entity
  (assert-successful-mass-realization (realize-all-trans "john@doe.com")))

(deftest a-user-cannot-realize-all-transactions-in-anothers-entity
  (assert-blocked-mass-realization (realize-all-trans "jane@doe.com")))
