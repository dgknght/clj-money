(ns clj-money.models.budget-items-test
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.db.ref]
            [clj-money.models.ref]
            [clj-money.model-helpers :as helpers :refer [assert-invalid
                                                         assert-updated
                                                         assert-deleted]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-account
                                            find-budget
                                            find-budget-item]]
            [clj-money.test-helpers :refer [dbtest]]))

(defn- assert-created
  [attr]
  (helpers/assert-created attr :refs [:budget-item/budget :budget-item/account]))

(defn- attributes []
  #:budget-item{:budget (find-budget "2016")
                :account (find-account "Groceries") 
                :periods [101M 102M 103M]})

(def ^:private ctx
  (conj basic-context
        #:budget{:name "2016"
                 :entity "Personal"
                 :period [3 :month]
                 :start-date (t/local-date 2016 1 1)}))

(dbtest create-a-budget-item
  (with-context ctx
    (assert-created (attributes))))

(dbtest account-is-required
  (with-context ctx
    (assert-invalid (dissoc (attributes) :budget-item/account)
                    {:budget-item/account ["Account is required"]})))

(dbtest account-must-belong-to-budget-entity
  (with-context ctx
    (assert-invalid
      (assoc (attributes)
             :budget-item/account (find-account "Sales"))
      {:budget-item/account
       ["Account must belong to the same entity as the budget"]})))

(dbtest period-count-must-match-the-budget
  (with-context ctx
    (assert-invalid
      (assoc (attributes)
             :budget-item/periods [101M])
      {:budget-item/periods ["Must have the number of periods specified by the budget"]})))

(def ^:private existing-context
  (conj ctx
        #:budget-item{:budget "2016"
                      :account "Groceries"
                      :periods [101M 102M 103M]}))

(dbtest update-a-budget-item
  (with-context existing-context
    (assert-updated (find-budget-item ["2016" "Groceries"])
                    {:budget-item/periods [111M 222M 333M]})))

(dbtest remove-an-item
  (with-context existing-context
    (assert-deleted (find-budget-item ["2016" "Groceries"]))))
