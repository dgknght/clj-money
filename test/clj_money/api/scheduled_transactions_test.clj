(ns clj-money.api.scheduled-transactions-test
  (:require [clojure.test :refer [use-fixtures deftest is]]
            [clojure.pprint :refer [pprint]]
            [ring.mock.request :as req]
            [java-time.api :as t]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [clj-money.util :as util]
            [clj-money.models :as models]
            [clj-money.dates :as dates :refer [with-fixed-time]]
            [clj-money.api.test-helper :refer [add-auth
                                               parse-json-body]]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user
                                            find-entity
                                            find-account
                                            find-scheduled-transaction]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models.transactions :as trans]
            [clj-money.models.scheduled-transactions :as sched-trans]
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
                                :interval-type :year
                                :interval-count 1
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
          parse-json-body))))

(defn- assert-successful-list
  [{:keys [json-body] :as response}]
  (is (http-success? response))
  (is (seq-of-maps-like?
        [#:scheduled-transaction{:start-date "2004-03-02"
                                 :description "Landlord"
                                 :memo "automatically created"
                                 :interval-type "year"
                                 :interval-count 1
                                 :items [#:scheduled-transaction-item{:action "credit"
                                                                      :quantity 50.0
                                                                      :memo "checking"}
                                         #:scheduled-transaction-item{:action "debit"
                                                                      :quantity 50.0
                                                                      :memo "rent"}]}]
        json-body)
      "The body contains the existing scheduled transactions"))

(defn- assert-blocked-list
  [{:keys [json-body] :as response}]
  (is (http-success? response))
  (is (empty? json-body)))

(deftest a-user-can-get-a-list-of-scheduled-transactions-in-his-entity
  (assert-successful-list (get-list "john@doe.com")))

(deftest a-user-cannot-get-a-list-of-scheduled-transactions-for-anothers-entity
  (assert-blocked-list (get-list "jane@doe.com")))

(defn- attr []
  #:scheduled-transaction{:description "Paycheck"
                          :start-date "2021-01-01"
                          :date-spec {:days [:friday]}
                          :interval-type :week
                          :interval-count 2
                          :memo "biweekly"
                          :items [#:scheduled-transaction-item{:action :debit
                                                               :quantity 1000M
                                                               :account (util/->model-ref (find-account "Checking"))
                                                               :memo "checking"}
                                  #:scheduled-transaction-item{:action :credit
                                                               :quantity 1000M
                                                               :account (util/->model-ref (find-account "Salary"))
                                                               :memo "salary"}]})

(defn- create-sched-tran
  [user-email]
  (let [entity (find-entity "Personal")
        response (-> (req/request :post (path :api
                                              :entities
                                              (:id entity)
                                              :scheduled-transactions))
                     (req/json-body (attr))
                     (add-auth (find-user user-email))
                     app
                     parse-json-body)]
    [response
     (when-let [id (get-in response [:json-body :id])]
       (models/find id :scheduled-transaction))]))

(defn- assert-sched-tran-created
  [[{:keys [json-body] :as response} retrieved]]
  (is (http-created? response))
  (is (:id json-body) "The return value contains an :id")
  (is (comparable?
        #:scheduled-transaction{:description "Paycheck"
                                :start-date "2021-01-01"
                                :date-spec {:days ["friday"]}
                                :interval-type "week"
                                :interval-count 2
                                :memo "biweekly"
                                :items [#:scheduled-transaction-item{:action "debit"
                                                                     :quantity 1000
                                                                     :memo "checking"}
                                        #:scheduled-transaction-item{:action "credit"
                                                                     :quantity 1000
                                                                     :memo "salary"}]}
        json-body)
      "The return value contains the created schedule transaction")
  (is (comparable? #:scheduled-transaction{:description "Paycheck"
                                           :start-date (t/local-date 2021 1 1)
                                           :date-spec {:days ["friday"]}
                                           :interval-type :week
                                           :interval-count 2
                                           :memo "biweekly"
                                           :items [#:scheduled-transaction-item{:action :debit
                                                                                :quantity 1000M
                                                                                :memo "checking"}
                                                   #:scheduled-transaction-item{:action :credit
                                                                                :quantity 1000M
                                                                                :memo "salary"}]}
                   retrieved)
      "The scheduled transaction can be retrieved"))

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
                                :interval-type :month
                                :interval-count 1
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
  #:scheduled-transaction{:interval-type :week
                          :interval-count 2})

(defn- update-sched-tran
  [user-email]
  (with-context update-context
    (let [tran (find-scheduled-transaction "Paycheck")]
      [(-> (req/request :patch (path :api
                                     :scheduled-transactions
                                     (:id tran)))
           (req/json-body update-attr)
           (add-auth (find-user user-email))
           app
           parse-json-body)
       (models/find tran)])))

(defn- assert-successful-update
  [[{:keys [json-body] :as response} retrieved]]
  (is (http-success? response))
  (is (comparable? #:scheduled-transaction{:interval-type "week"
                                           :interval-count 2}
                   json-body)
      "The updated scheduled transaction is returned")
  (is (comparable? update-attr retrieved)
      "The database is updated correctly"))

(defn- assert-blocked-update
  [[response retrieved]]
  (is (http-not-found? response))
  (is (comparable? #:scheduled-transaction{:interval-type :month
                                 :interval-count 1}
         retrieved)
      "The database is not updated"))

(deftest a-user-can-edit-a-scheduled-transaction-in-his-entity
  (assert-successful-update (update-sched-tran "john@doe.com")))

(deftest a-user-cannot-edit-a-scheduled-transaction-in-anothers-entity
  (assert-blocked-update (update-sched-tran "jane@doe.com")))

; (defn- delete-sched-tran
;   [user-email]
;   (let [ctx (realize update-context)
;         user (find-user ctx user-email)
;         tran (find-scheduled-transaction ctx "Paycheck")
;         res (-> (req/request :delete (path :api
;                                            :scheduled-transactions
;                                            (:id tran)))
;                 (add-auth user)
;                 app
;                 parse-json-body)
;         retrieved (sched-trans/find tran)]
;     [res retrieved]))
; 
; (defn- assert-successful-delete
;   [[response retrieved]]
;   (is (http-no-content? response))
;   (is (nil? retrieved) "The record cannot be retrieved after delete"))
; 
; (defn- assert-blocked-delete
;   [[response retrieved]]
;   (is (http-not-found? response))
;   (is retrieved "The record can still be retrieved after blocked delete"))
; 
; (deftest a-user-can-delete-a-scheduled-transaction-in-his-entity
;   (assert-successful-delete (delete-sched-tran "john@doe.com")))
; 
; (deftest a-user-cannot-delete-a-scheduled-transaction-in-anothers-entity
;   (assert-blocked-delete (delete-sched-tran "jane@doe.com")))
; 
; (defn- realize-trans
;   [user-email]
;   (let [ctx (realize update-context)
;         user (find-user ctx user-email)
;         sched-tran (find-scheduled-transaction ctx "Paycheck")
;         res (with-fixed-time "2016-02-02T00:00:00Z"
;                      (-> (req/request :post (path :api
;                                                   :scheduled-transactions
;                                                   (:id sched-tran)
;                                                   :realize))
;                          (add-auth user)
;                          app
;                          parse-json-body))
;         retrieved (trans/search {:transaction-date [:between> (t/local-date 2016 1 1) (t/local-date 2017 1 1)]
;                                  :entity-id (:entity-id sched-tran)
;                                  :description "Paycheck"}
;                                 {:include-items? true})]
;     [res retrieved]))
; 
; (defn- assert-successful-realization
;   [[response retrieved]]
;   (is (http-created? response))
;   (is (= 1 (count retrieved))
;       "One transaction is created.")
;   (is (= {:description "Paycheck"
;           :transaction-date (t/local-date 2016 2 1)}
;          (select-keys (first retrieved) [:description :transaction-date]))
;       "The transaction is created with the correct attributes.")
;   (is (= #{{:action :debit
;             :quantity 1000M}
;            {:action :credit
;             :quantity 1000M}}
;          (->> (:items (first retrieved) retrieved)
;               (map #(select-keys % [:action :quantity]))
;               (into #{})))
;       "The transaction is created with the correct line items."))
; 
; (defn- assert-blocked-realization
;   [[response retrieved]]
;   (is (http-not-found? response))
;   (is (empty? retrieved) "The transaction is not created."))
; 
; (deftest a-user-can-realize-a-scheduled-transaction-in-his-entity
;   (assert-successful-realization (realize-trans "john@doe.com")))
; 
; (deftest a-user-cannot-realize-a-scheduled-transaction-in-anothers-entity
;   (assert-blocked-realization (realize-trans "jane@doe.com")))
; 
; (def ^:private mass-realize-context
;   (update-in update-context
;              [:scheduled-transactions]
;              concat
;              [{:entity-id "Personal"
;                :description "Groceries"
;                :interval-type :week
;                :interval-count 1
;                :last-occurrence (t/local-date 2016 1 24)
;                :start-date (t/local-date 2015 1 1)
;                :enabled true
;                :date-spec {:days #{:sunday} }
;                :items [{:action :debit
;                         :account-id "Groceries"
;                         :quantity 100M}
;                        {:action :credit
;                         :account-id "Checking"
;                         :quantity 100M}]}
;               {:entity-id "Personal"
;                :description "Groceries disabled"
;                :interval-type :week
;                :interval-count 1
;                :last-occurrence (t/local-date 2016 1 24)
;                :start-date (t/local-date 2015 1 1)
;                :enabled false
;                :date-spec {:days #{:sunday} }
;                :items [{:action :debit
;                         :account-id "Groceries"
;                         :quantity 100M}
;                        {:action :credit
;                         :account-id "Checking"
;                         :quantity 100M}]}
;               {:entity-id "Personal"
;                :description "Groceries after end date"
;                :interval-type :week
;                :interval-count 1
;                :last-occurrence (t/local-date 2016 1 24)
;                :start-date (t/local-date 2015 1 1)
;                :end-date (t/local-date 2015 12 31)
;                :enabled true
;                :date-spec {:days #{:sunday} }
;                :items [{:action :debit
;                         :account-id "Groceries"
;                         :quantity 100M}
;                        {:action :credit
;                         :account-id "Checking"
;                         :quantity 100M}]}]))
; 
; (defn- realize-all-trans
;   [user-email]
;   (let [ctx (realize mass-realize-context)
;         user (find-user ctx user-email)
;         entity (find-entity ctx "Personal")
;         res (with-fixed-time "2016-02-02T00:00:00Z"
;                      (-> (req/request :post (path :api
;                                                   :entities
;                                                   (:id entity)
;                                                   :scheduled-transactions
;                                                   :realize))
;                          (add-auth user)
;                          app
;                          parse-json-body))
;         retrieved (trans/search {:transaction-date [:between> (t/local-date 2016 1 1) (t/local-date 2017 1 1)]
;                                  :entity-id (:id entity)}
;                                 {:include-items? true
;                                  :sort [:transaction-date]})]
;     [res retrieved]))
; 
; (defn- comparable-trans
;   [trans from-json?]
;   (cond-> (select-keys trans [:transaction-date :description])
;     from-json? (update-in [:transaction-date] dates/unserialize-local-date)))
; 
; (defn- assert-successful-mass-realization
;   [[response retrieved]]
;   (is (http-created? response))
;   (let [expected [{:description "Groceries"
;                    :transaction-date (t/local-date 2016 1 31)}
;                   {:description "Paycheck"
;                    :transaction-date (t/local-date 2016 2 1)}
;                   {:description "Groceries"
;                    :transaction-date (t/local-date 2016 2 7)}]]
;     (is (= expected (map #(comparable-trans % true) (:json-body response)))
;         "The created transactions are returned.")
;     (is (= expected (map #(comparable-trans % false) retrieved))
;         "The transactions can be retrieved.")))
; 
; (defn- assert-blocked-mass-realization
;   [[response retrieved]]
;   (is (http-not-found? response))
;   (is (empty? retrieved) "No transactions are created."))
; 
; (deftest a-user-can-realize-all-scheduled-transactions-in-his-entity
;   (assert-successful-mass-realization (realize-all-trans "john@doe.com")))
; 
; (deftest a-user-cannot-realize-all-transactions-in-anothers-entity
;   (assert-blocked-mass-realization (realize-all-trans "jane@doe.com")))
