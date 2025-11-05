(ns clj-money.api.budget-items-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.test]
            [clj-money.json]
            [clj-money.entities :as entities]
            [clj-money.api.test-helper :refer [parse-body
                                               request]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user
                                            find-account
                                            find-budget
                                            find-budget-item]]
            [clj-money.util :as util]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private ctx
  (conj basic-context
        #:budget{:entity "Personal"
                 :name "2016"
                 :period [3 :month]
                 :start-date (t/local-date 2016 1 1)}))

(defn- create-budget-item
  [user-email & {:keys [content-type body]}]
  (let [budget (find-budget "2016")
        groceries (find-account "Groceries")
        default-body #:budget-item{:account (util/->entity-ref groceries)
                                   :periods [100 101 102]}]
    [(-> (request :post (path :api
                              :budgets
                              (:id budget)
                              :items)
                  :content-type (or content-type "application/edn")
                  :body (or body default-body)
                  :user (find-user user-email))
         app
         parse-body)
     {:budget budget
      :account groceries}]))

(defn- assert-successful-create
  [[res {:keys [account]}]
   & {:keys [expected expected-response]
      :or {expected #:budget-item{:account {:id (:id account)}
                                  :periods [100M 101M 102M]}}}]
  (is (http-created? res))
  (is (comparable? (or expected-response expected)
                   (:parsed-body res))
      "The created budget item is returned in the response")
  (is (comparable? expected
                   (entities/find (if (:edn-body res)
                                    (:edn-body res)
                                    (:parsed-body res))))
      "The created budget item can be retrieved"))

(defn- assert-not-found-create
  [[res {:keys [budget account]}]]
  (is (http-not-found? res))
  (is (empty? (entities/select {:budget-item/account account
                              :budget/_self budget}))
      "The budget item is not created"))

(deftest a-user-can-add-an-item-to-a-budget-in-his-entity
  (with-context ctx
    (assert-successful-create (create-budget-item "john@doe.com"))))

(deftest a-user-cannot-add-an-item-to-a-budget-in-anothers-entity
  (with-context ctx
    (assert-not-found-create (create-budget-item "jane@doe.com"))))

(def ^:private update-ctx
  (conj basic-context
        #:budget{:entity "Personal"
                 :name "2016"
                 :period [3 :month]
                 :start-date (t/local-date 2016 1 1)
                 :items [#:budget-item{:account "Groceries"
                                       :periods [100M 101M 102M]}]}))

(defn- update-budget-item
  [user-email & {:keys [content-type body]
                 :or {content-type "application/edn"
                      body #:budget-item{:periods [110M 111M 112M]}}}]
  (let [item (find-budget-item ["2016" "Groceries"])]
    [(-> (request :patch (path :api
                               :budget-items
                               (:id item))
                  :content-type content-type
                  :body body
                  :user (find-user user-email))
         app
         parse-body)
     {:budget-item item}]))

(defn- assert-successful-update
  [[res {:keys [budget-item]}]
   & {:keys [expected expected-response]
      :or {expected {:budget-item/periods [110M 111M 112M]}}}]
  (is (http-success? res))
  (is (comparable? (or expected-response expected)
                   (:parsed-body res))
      "The updated budget item is returned in the response")
  (is (comparable? expected
                   (entities/find budget-item))
      "The updated budget item can be retrieved"))

(defn- assert-not-found-update
  [[res {:keys [budget-item]}]]
  (is (http-not-found? res))
  (is (comparable? {:budget-item/periods [100M 101M 102M]}
                   (entities/find budget-item))
      "The budget item is not updated"))

(deftest a-user-can-update-an-item-to-a-budget-in-his-entity
  (with-context update-ctx
    (assert-successful-update (update-budget-item "john@doe.com"))))

(deftest a-user-cannot-update-an-item-to-a-budget-in-anothers-entity
  (with-context update-ctx
    (assert-not-found-update (update-budget-item "jane@doe.com"))))

(defn- delete-budget-item
  [user-email & {:keys [content-type]
                 :or {content-type "application/edn"}}]
  (let [item (find-budget-item ["2016" "Groceries"])]
    [(-> (request :delete (path :api
                                :budget-items
                                (:id item))
                  :content-type content-type
                  :user (find-user user-email))
         app
         parse-body)
     {:budget-item item}]))

(defn- assert-successful-delete
  [[res {:keys [budget-item]}]]
  (is (http-no-content? res))
  (is (nil? (entities/find budget-item))
      "The budget item cannot be retrieved after delete"))

(defn- assert-not-found-delete
  [[res {:keys [budget-item]}]]
  (is (http-not-found? res))
  (is (entities/find budget-item)
      "The budget item can be retrieved after attempted delete"))

(deftest a-user-can-delete-an-item-from-a-budget-in-his-entity
  (with-context update-ctx
    (assert-successful-delete (delete-budget-item "john@doe.com"))))

(deftest a-user-cannot-delete-an-item-from-a-budget-in-anothers-entity
  (with-context update-ctx
    (assert-not-found-delete (delete-budget-item "jane@doe.com"))))
