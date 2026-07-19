(ns clj-money.entities.entities-test
  (:require [clojure.test :refer [is testing]]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user
                                            find-account
                                            find-entity
                                            find-image
                                            find-price
                                            find-attachment
                                            find-budget-item
                                            find-scheduled-transaction
                                            find-grant
                                            find-lot
                                            find-reconciliation]]
            [clj-factory.core :refer [factory]]
            [clj-money.entity-helpers :as helpers :refer [assert-invalid
                                                         assert-updated
                                                         assert-deleted]]
            [clj-money.images.sql]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.factories.user-factory]
            [clj-money.test-helpers :refer [dbtest]]))

(def ^:private entity-context
  [(factory :user {:user/email "john@doe.com"})
   (factory :user {:user/email "jane@doe.com"})])

(def ^:private list-context
  (conj entity-context
        #:entity{:name "Personal"
                 :user "john@doe.com"}
        #:entity{:name "Business"
                 :user "john@doe.com"}
        #:commodity{:entity "Personal"
                    :type :currency
                    :name "US Dollar"
                    :symbol "USD"}
        #:account{:entity "Personal"
                  :type :expense
                  :name "Dining"}
        #:account{:entity "Personal"
                  :type :expense
                  :name "Groceries"}))

(defn- attributes []
  #:entity{:name "Personal"
           :user (find-user "john@doe.com")})

(defn- assert-created
  [attr]
  (helpers/assert-created attr :refs [:entity/user]))

(dbtest create-an-entity
  (with-context entity-context
    (testing "An entity can be created with minimal attributes"
      (assert-created (attributes)))
    (testing "An entity can be created with all attributes"
      (assert-created
        #:entity{:name "Business"
                 :user (find-user "john@doe.com")
                 :settings #:settings{:inventory-method :fifo}}))))

(dbtest name-is-required
  (with-context entity-context
    (assert-invalid (dissoc (attributes) :entity/name)
                    {:entity/name ["Name is required"]})))

(dbtest name-is-unique-for-a-user
  (with-context list-context
    (assert-invalid (attributes)
                    {:entity/name ["Name is already in use"]})))

(dbtest name-can-be-duplicated-between-users
  (with-context list-context
    (let [user (find-user "jane@doe.com") ]
      (assert-created (assoc (attributes)
                             :entity/user user)))))

(dbtest get-a-list-of-entities
  (with-context list-context
    (let [user (find-user "john@doe.com")]
      (is (seq-of-maps-like? [{:entity/name "Business"}
                              {:entity/name "Personal"}]
                             (entities/select {:entity/user user}
                                            {:sort [[:entity/name :asc]]}))
          "Entities matching the criteria are returned"))))

(dbtest update-an-entity
  (with-context list-context
    (let [dining (find-account "Dining")
          groceries (find-account "Groceries")]
      (assert-updated (find-entity "Personal")
                      #:entity{:name "Entity Y"
                               :transaction-date-range [(t/local-date 2020 1 1)
                                                        (t/local-date 2020 12 31)]
                               :price-date-range [(t/local-date 2020 1 1)
                                                  (t/local-date 2020 12 31)]
                               :settings {:settings/monitored-accounts (->> [dining
                                                                             groceries]
                                                                            (map util/->entity-ref)
                                                                            set)}}))))

(dbtest delete-an-entity
  (with-context list-context
    (assert-deleted (find-entity "Personal"))))

(dbtest inventory-method-can-be-lifo
  (with-context entity-context
    (is (comparable? {:entity/settings {:settings/inventory-method :lifo}}
                     (entities/put (assoc (attributes)
                                        :entity/settings {:settings/inventory-method :lifo}))))))

(dbtest inventory-method-cannot-be-something-other-than-fifo-or-lifo
  (with-context entity-context
    (assert-invalid (assoc (attributes)
                           :entity/settings {:settings/inventory-method :not-valid})
                    {:entity/settings
                     {:settings/inventory-method
                      ["Inventory method must be fifo or lifo"]}})))

(def ^:private purge-context
  (conj basic-context
        #:commodity{:entity "Personal"
                    :type :stock
                    :name "Apple, Inc."
                    :symbol "AAPL"
                    :exchange :nasdaq}
        #:price{:commodity "AAPL"
                :trade-date (t/local-date 2017 3 2)
                :value 12.34M}
        #:transaction{:transaction-date (t/local-date 2017 1 1)
                      :entity "Personal"
                      :description "Paycheck"
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 1000M}
        #:image{:user "john@doe.com"
                :original-filename "receipt.jpg"
                :content-type "image/jpg"
                :content (io/file (io/resource "fixtures/attachment.jpg"))}
        #:attachment{:caption "Receipt"
                     :transaction [(t/local-date 2017 1 1) "Paycheck"]
                     :image "receipt.jpg"}
        #:budget{:name "2017"
                 :entity "Personal"
                 :start-date (t/local-date 2017 1 1)
                 :period [1 :month]
                 :items [#:budget-item{:account "Groceries"
                                       :periods (repeat 1 50M)}]}
        #:scheduled-transaction{:entity "Personal"
                                :description "Scheduled Paycheck"
                                :start-date (t/local-date 2017 1 1)
                                :date-spec {:day 1}
                                :period [1 :month]
                                :items [#:scheduled-transaction-item{:action :debit
                                                                     :account "Checking"
                                                                     :quantity 900M}
                                        #:scheduled-transaction-item{:action :credit
                                                                     :account "Salary"
                                                                     :quantity 900M}]}
        #:reconciliation{:account "Checking"
                         :end-of-period (t/local-date 2017 1 31)
                         :balance 1000M
                         :status :new
                         :items [[(t/local-date 2017 1 1) 1000M]]}
        #:grant{:entity "Personal"
                :user "jane@doe.com"
                :permissions {:account #{:index :show}}}
        #:lot{:account "Checking"
              :commodity "AAPL"
              :purchase-price 12.34M
              :shares-purchased 10M
              :shares-owned 10M
              :purchase-date (t/local-date 2017 3 2)}))

(dbtest purge-an-entity
  (with-context purge-context
    (let [entity (find-entity "Personal")
          image (find-image "receipt.jpg")
          business (find-entity "Business")
          scheduled-transaction (find-scheduled-transaction "Scheduled Paycheck")
          grant (find-grant ["Personal" "jane@doe.com"])
          lot (find-lot ["Checking" "AAPL" (t/local-date 2017 3 2)])]
      (entities/purge! entity)

      (is (nil? (entities/find entity))
          "The entity itself is removed")
      (is (empty? (entities/select {:account/entity entity}))
          "The entity's accounts are removed")
      (is (empty? (entities/select {:commodity/entity entity}))
          "The entity's commodities are removed")
      (is (nil? (entities/find (find-price ["AAPL" (t/local-date 2017 3 2)])))
          "Prices for the entity's commodities are removed")
      (is (empty? (entities/select {:transaction/entity entity}))
          "The entity's transactions are removed")
      (is (nil? (entities/find (find-attachment "Receipt")))
          "Attachments on the entity's transactions are removed")
      (is (empty? (entities/select {:budget/entity entity}))
          "The entity's budgets are removed")
      (is (nil? (entities/find (find-budget-item ["2017" "Groceries"])))
          "The entity's budget items are removed")
      (is (nil? (entities/find scheduled-transaction))
          "The entity's scheduled transactions are removed")
      (is (nil? (entities/find (find-reconciliation ["Checking" (t/local-date 2017 1 31)])))
          "The entity's reconciliations are removed")
      (is (nil? (entities/find grant))
          "The entity's grants are removed")
      (is (nil? (entities/find lot))
          "The entity's lots are removed")

      (is (some? (entities/find image))
          "A shared, user-owned image is not removed along with an attachment")

      (is (some? (entities/find business))
          "An unrelated entity belonging to the same user is not removed")
      (is (seq (entities/select {:account/entity business}))
          "An unrelated entity's accounts are not removed")
      (is (seq (entities/select {:commodity/entity business}))
          "An unrelated entity's commodities are not removed"))))
