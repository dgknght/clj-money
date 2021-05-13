(ns clj-money.api.accounts-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [ring.mock.request :as req]
            [cheshire.core :as json]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test :refer [parse-json-body]]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [realize
                                            find-user
                                            find-entity
                                            find-commodity
                                            find-account]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.web.server :refer [app]]
            [clj-money.models.accounts :as accounts]))

(use-fixtures :each reset-db)

(def ^:private context
  {:users (->> ["john@doe.com" "jane@doe.com"]
               (mapv #(factory :user {:email %})))
   :entities [{:name "Personal"
               :user-id "john@doe.com"}
              {:name "Business"
               :user-id "jane@doe.com"}]
   :commodities [{:name "US Dollar"
                  :symbol "USD"
                  :type :currency}]
   :accounts [{:name "Checking"
               :type :asset}]})

(defn- create-an-account
  [email]
  (let [ctx (realize context)
        user (find-user ctx email)
        entity (find-entity ctx "Personal")
        usd (find-commodity ctx "USD")
        response (-> (req/request :post (path :api
                                              :entities
                                              (:id entity)
                                              :accounts))
                     (req/json-body {:name "Savings"
                                     :type "asset"
                                     :commodity-id (:id usd)})
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)
        retrieved (accounts/search {:entity-id (:id entity)})]
    [response body retrieved]))

(defn- assert-successful-create
  [[response body retrieved]]
  (is (http-success? response))
  (is (comparable? {:name "Savings"
                    :type "asset"}
                   body)
      "The created account is returned in the response")
  (is (some #(= "Savings" (:name %)) retrieved)
      "The created account can be retrieved from the data store"))

(defn- assert-blocked-create
  [[response _ retrieved]]
  (is (http-not-found? response))
  (is (not-any? #(= "Savings" (:name %)) retrieved)
      "The account is not created"))

(deftest a-user-can-create-an-account-in-his-entity
  (assert-successful-create (create-an-account "john@doe.com")))

(deftest a-user-cannot-create-an-account-in-anothers-entity
  (assert-blocked-create (create-an-account "jane@doe.com")))

(defn- get-a-list
  [email]
  (let [ctx (realize  context)
        entity (find-entity ctx "Personal")
        user (find-user ctx email)
        response (-> (req/request :get (path :api
                                             :entities
                                             (:id entity)
                                             :accounts))
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)]
    [response body]))

(defn- assert-successful-list
  [[response body]]
  (is (http-success? response))
  (is (= #{"Checking"}
         (->> body
              (map :name)
              set))
      "The accounts are returned in the response"))

(defn- assert-blocked-list
  [[response body]]
  (is (http-success? response))
  (is (empty? body)))

(deftest a-user-can-get-a-list-of-accounts-in-his-entity
  (assert-successful-list (get-a-list "john@doe.com")))

(deftest a-user-cannot-get-a-list-of-accounts-in-anothers-entity
  (assert-blocked-list (get-a-list "jane@doe.com")))

(defn- get-an-account
  [email]
  (let [ctx (realize  context)
        checking (find-account ctx "Checking")
        user (find-user ctx email)]
    (-> (req/request :get (path :api
                                :accounts
                                (:id checking)))
        (add-auth user)
        app
        parse-json-body)))

(defn- assert-successful-get
  [{:keys [json-body] :as response}]
  (is (http-success? response))
  (is (comparable? {:name "Checking"
                    :type "asset"}
                   json-body)
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
  (let [ctx (realize  context)
        account (find-account ctx "Checking")
        user (find-user ctx email)
        response (-> (req/request :patch (path :api
                                               :accounts
                                               (:id account)))
                     (req/json-body (-> account
                                        (assoc :name "New Name")
                                        (select-keys [:name :type :commodity-id :parent-id])))
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)
        retrieved (accounts/find account)]
    [response body retrieved]))

(defn- assert-successful-update
  [[response body retrieved]]
  (is (http-success? response))
  (is (comparable? {:name "New Name"}
                   body)
      "The updated account is returned in the response")
  (is (comparable? {:name "New Name"}
                   retrieved)
      "The retrieved value has the updated attributes"))

(defn- assert-blocked-update
  [[response _ retrieved]]
  (is (http-not-found? response))
  (is (comparable? {:name "Checking"}
                   retrieved)
      "The retrieved value has not been updated."))

(deftest a-user-can-update-an-account-in-his-entity
  (assert-successful-update (update-an-account "john@doe.com")))

(deftest a-user-cannot-update-an-account-in-anothers-entity
  (assert-blocked-update (update-an-account "jane@doe.com")))

(defn- delete-an-account
  [email]
  (let [ctx (realize context)
        account (find-account ctx "Checking")
        user (find-user ctx email)
        response (-> (req/request :delete (path :api
                                                :accounts
                                                (:id account)))
                     (add-auth user)
                     app)
        retrieved (accounts/find account)]
    [response retrieved]))

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
