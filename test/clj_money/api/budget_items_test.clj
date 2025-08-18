(ns clj-money.api.budget-items-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [ring.mock.request :as req]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [clj-money.models :as models]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.test-helpers :refer [reset-db
                                            edn-body
                                            parse-edn-body]]
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
  [user-email]
  (let [budget (find-budget "2016")
        groceries (find-account "Groceries")]
    [(-> (req/request :post (path :api
                                  :budgets
                                  (:id budget)
                                  :items))
         (edn-body #:budget-item{:account (util/->model-ref groceries)
                                 :periods [100 101 102]})
         (add-auth (find-user user-email))
         app
         parse-edn-body)
     {:budget budget
      :account groceries}]))

(defn- assert-successful-create
  [[res {:keys [account]}]]
  (let [expected #:budget-item{:account {:id (:id account)}
                               :periods [100M 101M 102M]}]
    (is (http-created? res))
    (is (comparable? expected
                     (:edn-body res))
        "The created budget item is returned in the response")
    (is (comparable? expected
                     (models/find (:edn-body res)))
        "The created budget item can be retrieved")))

(defn- assert-not-found-create
  [[res {:keys [budget account]}]]
  (is (http-not-found? res))
  (is (empty? (models/select {:budget-item/account account
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
  [user-email]
  (let [item (find-budget-item ["2016" "Groceries"])]
    [(-> (req/request :patch (path :api
                                   :budget-items
                                   (:id item)))
         (edn-body #:budget-item{:periods [110M 111M 112M]})
         (add-auth (find-user user-email))
         app
         parse-edn-body)
     {:budget-item item}]))

(defn- assert-successful-update
  [[res {:keys [budget-item]}]]
  (is (http-success? res))
  (is (comparable? {:budget-item/periods [110M 111M 112M]}
                   (:edn-body res))
      "The updated budget item is returned in the response")
  (is (comparable? {:budget-item/periods [110M 111M 112M]}
                   (models/find budget-item))
      "The updated budget item can be retrieved"))

(defn- assert-not-found-update
  [[res {:keys [budget-item]}]]
  (is (http-not-found? res))
  (is (comparable? {:budget-item/periods [100M 101M 102M]}
                   (models/find budget-item))
      "The budget item is not updated"))

(deftest a-user-can-update-an-item-to-a-budget-in-his-entity
  (with-context update-ctx
    (assert-successful-update (update-budget-item "john@doe.com"))))

(deftest a-user-cannot-update-an-item-to-a-budget-in-anothers-entity
  (with-context update-ctx
    (assert-not-found-update (update-budget-item "jane@doe.com"))))

(defn- delete-budget-item
  [user-email]
  (let [item (find-budget-item ["2016" "Groceries"])]
    [(-> (req/request :delete (path :api
                                    :budget-items
                                    (:id item)))
         (add-auth (find-user user-email))
         app
         parse-edn-body)
     {:budget-item item}]))

(defn- assert-successful-delete
  [[res {:keys [budget-item]}]]
  (is (http-no-content? res))
  (is (nil? (models/find budget-item))
      "The budget item cannot be retrieved after delete"))

(defn- assert-not-found-delete
  [[res {:keys [budget-item]}]]
  (is (http-not-found? res))
  (is (models/find budget-item)
      "The budget item can be retrieved after attempted delete"))

(deftest a-user-can-delete-an-item-from-a-budget-in-his-entity
  (with-context update-ctx
    (assert-successful-delete (delete-budget-item "john@doe.com"))))

(deftest a-user-cannot-delete-an-item-from-a-budget-in-anothers-entity
  (with-context update-ctx
    (assert-not-found-delete (delete-budget-item "jane@doe.com"))))
