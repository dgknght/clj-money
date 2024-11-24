(ns clj-money.models.accounts-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [clojure.pprint :refer [pprint]]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test-assertions]
            [clj-money.factories.user-factory]
            [clj-money.factories.entity-factory]
            [clj-money.factories.account-factory]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.test-context :refer [with-context
                                            find-entity
                                            find-commodity
                                            find-account]]
            [clj-money.model-helpers :as helpers :refer [assert-invalid
                                                         assert-updated
                                                         assert-deleted]]
            [clj-money.models :as models]
            [clj-money.test-helpers :refer [reset-db]]))

(use-fixtures :each reset-db)

(defn- attributes []
  #:account{:name "Checking"
            :type :asset
            :entity (find-entity "Personal")
            :commodity (find-commodity "USD")
            :system-tags #{:something-special}})

(def ^:private account-context
  [(factory :user #:user{:email "john@doe.com"})
   #:entity{:name "Personal"
            :user "john@doe.com"}
   #:commodity{:symbol "USD"
               :type :currency
               :name "US Dollar"
               :entity "Personal"}])

(def ^:private select-context
  (conj account-context
        #:account{:name "Credit card"
                  :entity "Personal"
                  :type :liability
                  :commodity "USD"}
        #:account{:name "Checking"
                  :entity "Personal"
                  :type :asset
                  :commodity "USD"}))

(deftest select-accounts
  (with-context select-context
    (let [entity (find-entity "Personal")
          expected [#:account{:name "Checking"
                              :type :asset
                              :quantity 0M
                              :value 0M
                              :entity (select-keys entity [:id])}
                    #:account{:name "Credit card"
                              :type :liability
                              :quantity 0M
                              :value 0M
                              :entity (select-keys entity [:id])}]]
      (is (seq-of-maps-like? expected (models/select #:account{:entity entity}))
          "It returns the accounts for the entity"))))

#_(def ^:private nested-context
  (-> select-context
      (update-in [:accounts] concat [#:account{:name "Savings"
                                               :type :asset
                                               :entity "Personal"}
                                     #:account{:name "Reserve"
                                               :type :asset
                                               :parent "Savings"
                                               :entity "Personal"}
                                     #:account{:name "Car"
                                               :type :asset
                                               :parent "Savings"
                                               :entity "Personal"}
                                     #:account{:name "Doug"
                                               :type :asset
                                               :parent "Car"
                                               :entity "Personal"}
                                     #:account{:name "Eli"
                                               :type :asset
                                               :parent "Car"
                                               :entity "Personal"}
                                     #:account{:name "Taxes"
                                               :type :expense
                                               :entity "Personal"}
                                     #:account{:name "Federal Income Tax"
                                               :type :expense
                                               :parent "Taxes"
                                               :entity "Personal"}
                                     #:account{:name "Social Security"
                                               :type :expense
                                               :parent "Taxes"
                                               :entity "Personal"}])))

; I'm not sure we really need this, since we query all
; accounts for an entity together
#_(deftest select-account-with-children
  (with-context nested-context
    (let [parent (find-account "Savings")
          result (models/select {:account/parent parent}
                                {:include-children? true})]
      (is (= #{"Savings" "Reserve" "Car" "Doug" "Eli"}
             (set (map :account/name result)))))))

(defn- assert-created
  [attr]
  (helpers/assert-created attr :refs [:account/entity
                                      :account/commodity
                                      :account/parent]))

(deftest create-an-account
  (with-context account-context
    (assert-created (attributes))))

(def ^:private duplicate-name-context
  (conj select-context
        #:entity{:name "Business"
                 :user "john@doe.com"}
        #:commodity{:symbol "USD"
                    :name "US Dollar"
                    :type :currency
                    :entity "Business"}
        #:account{:name "Savings"
                  :type :asset
                  :entity "Personal"
                  :commodity "USD"}
        #:account{:name "Household"
                  :type :expense
                  :entity "Personal"
                  :commodity "USD"}
        #:account{:name "Repairs"
                  :type :expense
                  :entity "Personal"
                  :parent "Household"
                  :commodity "USD"}))

(deftest name-can-be-duplicated-across-entities
  (with-context duplicate-name-context
    (assert-created (assoc (attributes)
                           :account/entity (find-entity "Business")))))

(deftest name-can-be-duplicated-across-account-types
  (with-context duplicate-name-context
    (assert-created (assoc (attributes)
                           :account/type :liability
                           :account/name "Household"))))

(def ^:private create-child-context
  (conj select-context
        #:account{:name "Savings"
                  :type :asset
                  :commodity "USD"
                  :entity "Personal"}))

(deftest create-a-child-account
  (with-context create-child-context
    (assert-created #:account{:name "Car"
                              :type :asset
                              :commodity (find-commodity "USD")
                              :parent (find-account "Savings")
                              :entity (find-entity "Personal")})))

(deftest child-must-have-same-type-as-parent
  (with-context create-child-context
    (assert-invalid #:account{:name "Federal income tax"
                              :type :expense
                              :commodity (find-commodity "USD")
                              :parent (find-account "Savings") 
                              :entity (find-entity "Personal")}
                    {:account/type ["Type must match the parent type"]})))

(deftest name-is-required
  (with-context account-context
    (assert-invalid (dissoc (attributes) :account/name)
                    {:account/name ["Name is required"]})))

(deftest name-is-unique-within-a-parent
  (with-context duplicate-name-context
    (assert-invalid #:account{:name "Repairs"
                              :parent (find-account "Household")
                              :entity (find-entity "Personal")
                              :commodity (find-commodity "USD")
                              :type :expense}
                    {:account/name ["Name is already in use"]})))

(deftest name-can-be-duplicated-across-parents
  (with-context duplicate-name-context
    (assert-created (assoc (attributes)
                           :account/parent (find-account "Savings")
                           :account/name "Repairs"))))

(deftest type-cannot-be-something-other-than-expense-equity-liability-income-asset
  (with-context account-context
    (assert-invalid (assoc (attributes) :account/type :invalidtype)
                    {:account/type ["Type must be expense, equity, liability, income, or asset"]})))

(deftest commodity-id-defaults-to-entity-default
  (with-context account-context
    (let [account (assert-created (dissoc (attributes)
                                          :account/commodity))]
      (is (comparable? {:commodity/symbol "USD"}
                       (models/find (:id (get-in account [:account/commodity]))
                                    :commodity))))))

(deftest update-an-account
  (with-context select-context
    (assert-updated (find-account "Checking")
                    #:account{:name "New name"
                              :allocations {1 50M
                                            2 50M}})))

(def same-parent-context
  (conj select-context 
        #:account{:name "Current assets"
                  :type :asset
                  :entity "Personal"
                  :commodity "USD"}
        #:account{:name "Fixed assets"
                  :type :asset
                  :entity "Personal"
                  :commodity "USD"}
        #:account{:name "House"
                  :type :asset
                  :parent "Current assets"
                  :entity "Personal"
                  :commodity "USD"}))

(deftest change-an-account-parent
  (with-context same-parent-context
    (let [fixed-assets (find-account "Fixed assets")
          result (-> (find-account "House")
                     (assoc :account/parent fixed-assets)
                     models/put)]
      (is (= (:id fixed-assets)
             (:id (:account/parent result)))
          "The returned account has the correct parent-id value")
      (is (comparable? (:id fixed-assets)
                       (:id (:account/parent (models/find result))))
          "The retrieved account has the correct parent-id value"))))

(deftest delete-an-account
  (with-context select-context
    (assert-deleted (find-account "Checking"))))
