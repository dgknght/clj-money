(ns clj-money.authorization-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [java-time.api :as t]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.images.sql]
            [clj-money.entities :as entities]
            [clj-money.test-helpers :refer [reset-db
                                            dbtest]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user
                                            find-account
                                            find-commodity
                                            find-lot]]
            [clj-money.authorization :as authorization]
            [clj-money.authorization.accounts]
            [clj-money.authorization.attachments]
            [clj-money.authorization.budget-items]
            [clj-money.authorization.budgets]
            [clj-money.authorization.commodities]
            [clj-money.authorization.entities]
            [clj-money.authorization.lot-notes]
            [clj-money.authorization.lots]
            [clj-money.authorization.prices]
            [clj-money.authorization.receipt-ingestions]
            [clj-money.authorization.reconciliations]
            [clj-money.authorization.scheduled-transactions]
            [clj-money.authorization.statement-ingestions]
            [clj-money.authorization.transaction-items]
            [clj-money.authorization.transactions]))

(use-fixtures :each reset-db)

(def ^:private grant-context
  (conj basic-context
        #:grant{:entity "Personal"
                :user "jane@doe.com"
                :permissions {:account #{:show :delete}}}))

(defn- allowed?
  [action]
  (authorization/allowed? (find-account "Checking")
                          action
                          (find-user "jane@doe.com")))

(deftest a-grant-allows-the-actions-it-permits
  (with-context grant-context
    (is (allowed? ::authorization/show))
    (testing "the grant's :delete permits ::authorization/destroy"
      (is (allowed? ::authorization/destroy)))))

(deftest a-grant-does-not-allow-the-actions-it-omits
  (with-context grant-context
    (is (not (allowed? ::authorization/create)))
    (is (not (allowed? ::authorization/update)))
    (testing "::authorization/recalculate requires :update"
      (is (not (allowed? ::authorization/recalculate))))))

(deftest a-grant-only-covers-its-resource-types
  (with-context grant-context
    (is (not (authorization/allowed? (find-commodity "USD")
                                     ::authorization/show
                                     (find-user "jane@doe.com"))))))

(def ^:private resource-context
  (concat basic-context
          [#:commodity{:name "Apple"
                       :entity "Personal"
                       :symbol "AAPL"
                       :exchange :nasdaq
                       :type :stock}
           #:price{:commodity "AAPL"
                   :trade-date (t/local-date 2015 1 1)
                   :value 100M}
           #:account{:name "IRA"
                     :entity "Personal"
                     :type :asset
                     :commodity "USD"
                     :system-tags #{:trading}}
           #:lot{:account "IRA"
                 :commodity "AAPL"
                 :purchase-price 100M
                 :shares-purchased 10M
                 :shares-owned 10M
                 :purchase-date (t/local-date 2015 1 1)}
           #:lot-note{:lots [["IRA" "AAPL" (t/local-date 2015 1 1)]]
                      :transaction-date (t/local-date 2015 2 1)
                      :memo "2-for-1 stock split"}
           #:transaction{:transaction-date (t/local-date 2015 1 1)
                         :entity "Personal"
                         :description "Paycheck"
                         :quantity 1000M
                         :debit-account "Checking"
                         :credit-account "Salary"}
           #:image{:user "john@doe.com"
                   :original-filename "receipt.jpg"
                   :content-type "image/jpg"
                   :content (io/file "fixtures/attachment.jpg")}
           #:attachment{:caption "Receipt"
                        :transaction [(t/local-date 2015 1 1) "Paycheck"]
                        :image "receipt.jpg"}
           #:receipt-ingestion{:entity "Personal"
                               :image "receipt.jpg"
                               :status :pending}
           #:statement-ingestion{:entity "Personal"
                                 :image "receipt.jpg"
                                 :status :pending}
           #:reconciliation{:account "Checking"
                            :balance 1000M
                            :status :completed
                            :end-of-period (t/local-date 2015 1 31)
                            :items [[(t/local-date 2015 1 1) 1000M]]}
           #:budget{:entity "Personal"
                    :name "2015"
                    :start-date (t/local-date 2015 1 1)
                    :period [12 :month]
                    :items [#:budget-item{:account "Rent"
                                          :periods (repeat 12 100M)}]}
           #:scheduled-transaction{:entity "Personal"
                                   :start-date (t/local-date 2015 1 1)
                                   :enabled true
                                   :date-spec {:day 1}
                                   :period [1 :month]
                                   :description "Landlord"
                                   :items [#:scheduled-transaction-item{:action :credit
                                                                        :account "Checking"
                                                                        :quantity 500M}
                                           #:scheduled-transaction-item{:action :debit
                                                                        :account "Rent"
                                                                        :quantity 500M}]}]))

(def ^:private resource-types
  [:entity
   :account
   :commodity
   :price
   :transaction
   :transaction-item
   :attachment
   :budget
   :budget-item
   :reconciliation
   :lot
   :lot-note
   :scheduled-transaction
   :receipt-ingestion
   :statement-ingestion])

(defn- grant-all
  [actions]
  (concat resource-context
          [#:grant{:entity "Personal"
                   :user "jane@doe.com"
                   :permissions (zipmap [:account :entity :transaction
                                         :budget :budget-item :commodity
                                         :price :attachment :reconciliation]
                                        (repeat actions))}]))

(def ^:private index-grant-context
  (grant-all #{:index}))

(def ^:private show-grant-context
  (grant-all #{:show}))

(defn- scoped-ids
  [resource-type email & [action]]
  (->> (entities/select (if action
                          (authorization/+scope {} resource-type (find-user email) action)
                          (authorization/+scope {} resource-type (find-user email))))
       (map :id)
       set))

(defn- owners-ids
  [resource-type]
  (let [ids (scoped-ids resource-type "john@doe.com")]
    (is (seq ids)
        (format "The owner can reach the %s" (name resource-type)))
    ids))

(dbtest a-scope-excludes-anothers-resources-without-a-grant
  (with-context resource-context
    (doseq [resource-type resource-types]
      (is (empty? (set/intersection (owners-ids resource-type)
                                    (scoped-ids resource-type "jane@doe.com")
                                    (scoped-ids resource-type "jane@doe.com" ::authorization/manage)))
          (format "The %s are excluded" (name resource-type))))))

(dbtest an-index-grant-includes-anothers-resources-in-a-scope
  (with-context index-grant-context
    (doseq [resource-type resource-types]
      (is (set/subset? (owners-ids resource-type)
                       (scoped-ids resource-type "jane@doe.com"))
          (format "The %s are included" (name resource-type))))))

(dbtest a-show-grant-includes-anothers-resources-only-for-a-lookup
  (with-context show-grant-context
    (doseq [resource-type resource-types]
      (let [owners (owners-ids resource-type)]
        (is (empty? (set/intersection owners
                                      (scoped-ids resource-type "jane@doe.com")))
            (format "The %s are excluded from a list" (name resource-type)))
        (is (set/subset? owners
                         (scoped-ids resource-type "jane@doe.com" ::authorization/manage))
            (format "The %s are included for a lookup" (name resource-type)))))))

(def ^:private no-entity-context
  (conj basic-context
        #:user{:email "jim@doe.com"
               :first-name "Jim"
               :last-name "Doe"
               :password "Please001!"
               :roles #{:user}}))

(dbtest a-scope-is-empty-for-a-user-without-entities
  (with-context no-entity-context
    (is (empty? (scoped-ids :entity "jim@doe.com")))
    (is (empty? (scoped-ids :account "jim@doe.com" ::authorization/manage)))))

(dbtest a-resource-type-that-cannot-be-granted-takes-the-permissions-of-its-parent
  (with-context show-grant-context
    (let [jane (find-user "jane@doe.com")
          lot (find-lot ["IRA" "AAPL" (t/local-date 2015 1 1)])]
      (is (authorization/allowed? lot ::authorization/show jane)
          "A lot takes the account permissions")
      (is (not (authorization/allowed? lot ::authorization/update jane))
          "A lot takes only the account permissions granted"))))
