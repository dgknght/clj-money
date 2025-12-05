(ns clj-money.api.accounts-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [lambdaisland.uri :refer [map->query-string]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [clj-money.json]
            [clj-money.util :as util]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user
                                            find-entity
                                            find-commodity
                                            find-account]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.api.test-helper :refer [request
                                               parse-body]]
            [clj-money.web.server :refer [app]]
            [clj-money.entities :as entities]))

(use-fixtures :each reset-db)

(defn- create-an-account
  [email & {:keys [content-type body]
            :or {content-type "application/edn"
                 body #:account{:name "Savings"
                                :type :asset
                                :commodity {:id 1}}}}]
  (let [entity (find-entity "Personal")
        response (-> (request :post (path :api
                                          :entities
                                          (:id entity)
                                          :accounts)
                              :user (find-user email)
                              :content-type content-type
                              :body body)
                     app
                     parse-body)
        retrieved (-> response
                      :parsed-body
                      :id
                      entities/find)]
    [response retrieved]))

(defn- assert-successful-create
  [[{:keys [parsed-body] :as response} retrieved]
   & {:keys [expected expected-response]
      :or {expected #:account{:name "Savings"
                              :type :asset}}}]
  (is (http-success? response))
  (is (comparable? (or expected-response expected)
                   parsed-body)
      "The created account is returned in the response")
  (is (comparable? expected retrieved)
      "The created account can be retrieved from the data store"))

(defn- assert-blocked-create
  [[response retrieved]]
  (is (http-not-found? response))
  (is (nil? retrieved) "The account is not created"))

(deftest a-user-can-create-an-account-in-his-entity
  (with-context
    (let [commodity (find-commodity "USD")]
      (testing "default format (edn)"
        (assert-successful-create (create-an-account "john@doe.com")))
      (testing "json format"
        (assert-successful-create
          (create-an-account "john@doe.com"
                             :content-type "application/json"
                             :body {:name "JSON Savings"
                                    :type :asset
                                    :commodity {:id (str (:id commodity))}
                                    :_type :account})
          :expected #:account{:name "JSON Savings"
                              :type :asset
                              :commodity (util/->entity-ref commodity)}
          :expected-response {:name "JSON Savings"
                              :type "asset"
                              :commodity {:id (str (:id commodity))}
                              :_type "account"})))))

(deftest a-user-cannot-create-an-account-in-anothers-entity
  (with-context
    (assert-blocked-create (create-an-account "jane@doe.com"))))

(def list-context
  (filter #(or (not= :account (util/entity-type %))
               (= "Checking" (:account/name %)))
          basic-context))

(defn- get-a-list
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (-> (request :get (path :api
                          :entities
                          (:id (find-entity "Personal"))
                          :accounts)
               :content-type content-type
               :user (find-user email))
      app
      parse-body))

(defn- assert-blocked-list
  [response]
  (is (http-success? response))
  (is (empty? (:parsed-body response))))

(deftest a-user-can-get-a-list-of-accounts-in-his-entity
  (with-context list-context
    (testing "default format"
      (let [res (get-a-list "john@doe.com")]
        (is (http-success? res))
        (is (seq-of-maps-like? [#:account{:name "Checking"
                                          :type :asset}]
                               (:parsed-body res))
            "The accounts are returned in the response")))
    (testing "json format"
      (let [res (get-a-list "john@doe.com" :content-type "application/json")]
        (is (http-success? res))
        (is (seq-of-maps-like? [{:name "Checking"
                                 :type "asset"
                                 :_type "account"}]
                               (:parsed-body res))
            "The accounts are returned in the response")))))

(deftest a-user-cannot-get-a-list-of-accounts-in-anothers-entity
  (with-context list-context
    (assert-blocked-list (get-a-list "jane@doe.com"))))

(deftest a-user-can-get-a-list-of-accounts-by-name
  (with-context
    (let [res (-> (request :get (str (path :api
                                           :entities
                                           (:id (find-entity "Personal"))
                                           :accounts)
                                     "?"
                                     (map->query-string {:name "Checking"}))
                          :user (find-user "john@doe.com"))
                  app
                  parse-body)]
      (is (http-success? res))
      (is (seq-of-maps-like? [{:account/name "Checking"}]
                             (:parsed-body res))))))

(def ^:private tag-context
  [#:user{:email "john@doe.com"
          :first-name "John"
          :last-name "Doe"
          :password "Please001!"}
   #:entity{:name "Personal"
            :user "john@doe.com"}
   #:commodity{:name "US Dollar"
               :entity "Personal"
               :symbol "USD"
               :type :currency}
   #:account{:name "Checking"
             :type :asset
             :entity "Personal"}
   #:account{:name "Rent"
             :type :expense
             :entity "Personal"
             :user-tags #{:mandatory}}
   #:account{:name "Dining"
             :entity "Personal"
             :type :expense
             :user-tags #{:discretionary}}
   #:account{:name "Tax"
             :entity "Personal"
             :type :expense
             :user-tags #{:mandatory}}])

(deftest a-user-can-get-a-list-of-accounts-by-tag
  (with-context tag-context
    (let [res (-> (request :get (str (path :api
                                           :entities
                                           (:id (find-entity "Personal"))
                                           :accounts)
                                     "?"
                                     (map->query-string
                                       {:user-tags ["mandatory" "discretionary"]}))
                          :user (find-user "john@doe.com"))
                  app
                  parse-body)]
      (is (http-success? res))
      (is (= #{"Rent" "Tax" "Dining"}
             (set (map :account/name (:parsed-body res))))))))

(defn- get-an-account
  [email]
  (with-context
    (-> (request :get (path :api
                            :accounts
                            (:id (find-account "Checking")))
                 :user (find-user email))
        app
        parse-body)))

(defn- assert-successful-get
  [{:keys [parsed-body] :as response}]
  (is (http-success? response))
  (is (comparable? #:account{:name "Checking"
                             :type :asset}
                   parsed-body)
      "The accounts are returned in the response"))

(defn- assert-blocked-get
  [response]
  (is (http-not-found? response)))

(deftest a-user-can-get-an-account-in-his-entity
  (assert-successful-get (get-an-account "john@doe.com")))

(deftest a-user-cannot-get-an-account-in-anothers-entity
  (assert-blocked-get (get-an-account "jane@doe.com")))

(defn- update-an-account
  [email & {:keys [content-type body]
            :or {content-type "application/edn"
                 body #:account{:name "New Name"
                                :type :asset
                                :commodity {:id 1}}}}]
  (let [account (find-account "Checking")
        response (-> (request :patch (path :api
                                           :accounts
                                           (:id account))
                              :user (find-user email)
                              :content-type content-type
                              :body body)
                     app
                     parse-body)
        retrieved (entities/find account)]
    [response retrieved]))

(defn- assert-successful-update
  [[{:keys [parsed-body] :as response} retrieved]
   & {:keys [expected expected-response]
      :or {expected {:account/name "New Name"}
           expected-response {:account/name "New Name"}}}]
  (is (http-success? response))
  (is (empty? (:dgknght.app-lib.validation/errors parsed-body))
      "There are no validation errors")
  (is (comparable? expected-response parsed-body)
      "The updated account is returned in the response")
  (is (comparable? expected retrieved)
      "The retrieved value has the updated attributes"))

(defn- assert-blocked-update
  [[response retrieved]]
  (is (http-not-found? response))
  (is (comparable? {:account/name "Checking"}
                   retrieved)
      "The retrieved value does not reflect an update."))

(deftest a-user-can-update-an-account-in-his-entity
  (with-context
    (testing "default format"
      (assert-successful-update (update-an-account "john@doe.com")))
    (testing "json format"
      (assert-successful-update
        (update-an-account "john@doe.com"
                           :content-type "application/json"
                           :body {:name "JSON Name"
                                  :type "asset"
                                  :_type "account"})
        :expected {:account/name "JSON Name"
                   :account/type :asset}
        :expected-response {:name "JSON Name"
                            :type "asset"
                            :_type "account"}))))

(deftest a-user-cannot-update-an-account-in-anothers-entity
  (with-context
    (assert-blocked-update (update-an-account "jane@doe.com"))))

(defn- delete-an-account
  [email]
  (with-context
    (let [account (find-account "Checking")
          response (-> (request :delete (path :api
                                              :accounts
                                              (:id account))
                                :user (find-user email))
                       app)
          retrieved (entities/find account)]
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
