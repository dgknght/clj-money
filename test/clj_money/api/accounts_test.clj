(ns clj-money.api.accounts-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [ring.mock.request :as req]
            [lambdaisland.uri :refer [map->query-string]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [clj-money.json]
            [clj-money.db :as db]
            [clj-money.util :as util]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user
                                            find-entity
                                            find-commodity
                                            find-account]]
            [clj-money.test-helpers :refer [reset-db
                                            edn-body
                                            parse-edn-body]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.web.server :refer [app]]
            [clj-money.models :as models]))

(use-fixtures :each reset-db)

(defn- create-an-account
  [email]
  (with-context
    (let [user (find-user email)
          entity (find-entity "Personal")
          usd (find-commodity "USD")
          response (-> (req/request :post (path :api
                                                :entities
                                                (:id entity)
                                                :accounts))
                       (edn-body {:account/name "Savings"
                                       :account/type :asset
                                       :account/commodity (util/->model-ref usd)})
                       (add-auth user)
                       app
                       parse-edn-body)
          retrieved (when-let [id (get-in response [:edn-body :id])]
                      (models/find id :account))]
      [response retrieved])))

(defn- assert-successful-create
  [[{:keys [edn-body] :as response} retrieved]]
  (is (http-success? response))
  (is (comparable? #:account{:name "Savings"
                             :type :asset}
                   edn-body)
      "The created account is returned in the response")
  (is (comparable? #:account{:name "Savings"
                             :type :asset}
                   retrieved)
      "The created account can be retrieved from the data store"))

(defn- assert-blocked-create
  [[response retrieved]]
  (is (http-not-found? response))
  (is (nil? retrieved) "The account is not created"))

(deftest a-user-can-create-an-account-in-his-entity
  (assert-successful-create (create-an-account "john@doe.com")))

(deftest a-user-cannot-create-an-account-in-anothers-entity
  (assert-blocked-create (create-an-account "jane@doe.com")))

(def list-context
  (filter #(or (not= :account (db/model-type %))
               (= "Checking" (:account/name %)))
          basic-context))

(defn- get-a-list
  [email]
  (with-context list-context
    (-> (req/request :get (path :api
                                :entities
                                (:id (find-entity "Personal"))
                                :accounts))
        (add-auth (find-user email))
        app
        parse-edn-body)))

(defn- assert-successful-list
  [response]
  (is (http-success? response))
  (is (seq-of-maps-like? [#:account{:name "Checking"
                                    :type :asset}] 
                         (:edn-body response))
      "The accounts are returned in the response"))

(defn- assert-blocked-list
  [response]
  (is (http-success? response))
  (is (empty? (:edn-body response))))

(deftest a-user-can-get-a-list-of-accounts-in-his-entity
  (assert-successful-list (get-a-list "john@doe.com")))

(deftest a-user-cannot-get-a-list-of-accounts-in-anothers-entity
  (assert-blocked-list (get-a-list "jane@doe.com")))

(deftest a-user-can-get-a-list-of-accounts-by-name
  (with-context
    (let [res (-> (req/request :get (str (path :api
                                               :entities
                                               (:id (find-entity "Personal"))
                                               :accounts)
                                         "?"
                                         (map->query-string {:name "Checking"})
                                         ))
                  (add-auth (find-user "john@doe.com"))
                  app
                  parse-edn-body)]
      (is (http-success? res))
      (is (seq-of-maps-like? [{:account/name "Checking"}]
                             (:edn-body res))))))

(defn- get-an-account
  [email]
  (with-context
    (-> (req/request :get (path :api
                                :accounts
                                (:id (find-account "Checking"))))
        (add-auth (find-user email))
        app
        parse-edn-body)))

(defn- assert-successful-get
  [{:keys [edn-body] :as response}]
  (is (http-success? response))
  (is (comparable? #:account{:name "Checking"
                             :type :asset}
                   edn-body)
      "The accounts are returned in the response"))

(defn- assert-blocked-get
  [response]
  (is (http-not-found? response)))

(deftest a-user-can-get-an-account-in-his-entity
  (assert-successful-get (get-an-account "john@doe.com")))

(deftest a-user-cannot-get-an-account-in-anothers-entity
  (assert-blocked-get (get-an-account "jane@doe.com")))

(defn- update-an-account
  [email]
  (with-context
    (let [account (find-account "Checking")
          response (-> (req/request :patch (path :api
                                                 :accounts
                                                 (:id account)))
                       (edn-body (-> account
                                     (assoc :account/name "New Name")
                                     (select-keys [:account/name
                                                   :account/type
                                                   :account/commodity
                                                   :account/parent])))
                       (add-auth (find-user email))
                       app
                       parse-edn-body)
          retrieved (models/find account)]
      [response retrieved])))

(defn- assert-successful-update
  [[{:keys [edn-body] :as response} retrieved]]
  (is (http-success? response))
  (is (empty? (:dgknght.app-lib.validation/errors edn-body))
      "There are no validation errors")
  (is (comparable? {:account/name "New Name"}
                   edn-body)
      "The updated account is returned in the response")
  (is (comparable? {:account/name "New Name"}
                   retrieved)
      "The retrieved value has the updated attributes"))

(defn- assert-blocked-update
  [[response retrieved]]
  (is (http-not-found? response))
  (is (comparable? {:account/name "Checking"}
                   retrieved)
      "The retrieved value does not reflect an update."))

(deftest a-user-can-update-an-account-in-his-entity
  (assert-successful-update (update-an-account "john@doe.com")))

(deftest a-user-cannot-update-an-account-in-anothers-entity
  (assert-blocked-update (update-an-account "jane@doe.com")))

(defn- delete-an-account
  [email]
  (with-context
    (let [account (find-account "Checking")
          response (-> (req/request :delete (path :api
                                                  :accounts
                                                  (:id account)))
                       (add-auth (find-user email))
                       app)
          retrieved (models/find account)]
      [response retrieved])))

(defn- assert-successful-delete
  [[response retrieved]]
  (is (http-success? response))
  (is (nil? retrieved)
      "The record cannot be retrieved after delete"))

(defn- assert-blocked-delete
  [[response retrieved]]
  (is (http-not-found? response))
  (is retrieved
      "The record can be retrieved after a failed delete"))

(deftest a-user-can-delete-an-account-in-his-entity
  (assert-successful-delete (delete-an-account "john@doe.com")))

(deftest a-user-cannot-delete-an-account-in-anothers-entity
  (assert-blocked-delete (delete-an-account "jane@doe.com")))
