(ns clj-money.api.commodities-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [environ.core :refer [env]]
            [ring.mock.request :as req]
            [cheshire.core :as json]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [realize
                                            find-user
                                            find-entity
                                            find-commodity]]
            [clj-money.test-helpers :refer [reset-db
                                            selective=]]
            [clj-money.web.test-helpers :refer [assert-successful
                                                assert-created
                                                assert-bad-request
                                                assert-not-found]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.x-platform.util :refer [path]]
            [clj-money.models.commodities :as coms]
            [clj-money.validation :as v]
            [clj-money.web.server :refer [app]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db (env :db)))

(def ^:private context
  {:users (->> ["john@doe.com" "jane@doe.com"]
               (mapv #(factory :user {:email %})))
   :entities [{:name "Personal"
               :user-id "john@doe.com"}
              {:name "Business"
               :user-id "jane@doe.com"}]
   :commodities [{:name "US Dollar"
                  :symbol "USD"
                  :type :currency
                  :entity-id "Personal"}
                 {:name "Microsoft, Inc"
                  :symbol "MSFT"
                  :type :stock
                  :exchange :nasdaq
                  :entity-id "Personal"}]})

(defn- get-a-count-of-commodities
  [email]
  (let [ctx (realize (env :db) context)
        user (find-user ctx email)
        entity (find-entity ctx "Personal")
        response (-> (req/request :get (path :api
                                             :entities
                                             (:id entity)
                                             :commodities
                                             :count))
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)]
    [response body]))

(defn- assert-successful-count
  [[response body]]
  (assert-successful response)
  (is (= {:count 2} body) "The body contains the count"))

(defn- assert-blocked-count
  [[response]]
  (assert-not-found response))

(deftest a-user-can-get-a-count-of-commodities-in-his-entity
  (assert-successful-count (get-a-count-of-commodities "john@doe.com")))

(deftest a-user-cannot-get-a-count-of-commodities-in-anothers-entity
  (assert-blocked-count (get-a-count-of-commodities "jane@doe.com")))

(defn- get-a-commodity
  [email]
  (let [ctx (realize (env :db) context)
        user (find-user ctx email)
        msft (find-commodity ctx "MSFT")
        response (-> (req/request :get (path :api
                                             :commodities
                                             (:id msft)))
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)]
    [response body]))

(defn- assert-successful-get
  [[response body]]
  (assert-successful response)
  (is (selective= {:name "Microsoft, Inc"
                   :symbol "MSFT"
                   :type "stock"
                   :exchange "nasdaq"}
                  body)
      "The specified commodity is returned in the response"))

(defn- assert-blocked-get
  [[response]]
  (assert-not-found response))

(deftest a-user-can-get-a-commodity-in-his-entity
  (assert-successful-get (get-a-commodity "john@doe.com")))

(deftest a-user-cannot-get-a-commodity-in-aothers-entity
  (assert-blocked-get (get-a-commodity "jane@doe.com")))

(def ^:private commodity-attributes
  {:type "stock"
   :name "Apple, Inc."
   :symbol "AAPL"
   :exchange "nasdaq"})

(defn- create-a-commodity
  [email]
  (let [ctx (realize (env :db) context)
        user (find-user ctx email)
        entity (find-entity ctx "Personal")
        response (-> (req/request :post (path :api
                                              :entities
                                              (:id entity)
                                              :commodities))
                     (req/json-body commodity-attributes)
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)
        retrieved (coms/search (env :db) {:entity-id (:id entity)})]
    [response body retrieved]))

(defn- assert-successful-create
  [[response body retrieved]]
  (assert-created response)
  (is (selective= commodity-attributes
                  body)
      "The newly created commodity is returned in the response")
  (is (some #(selective= (-> commodity-attributes
                             (update-in [:type] keyword)
                             (update-in [:exchange] keyword))
                         %)
            retrieved)
      "The new commodity can be retrieved from the database"))

(defn- assert-blocked-create
  [[response _ retrieved]]
  (assert-not-found response)
  (is (not-any? #(selective= (-> commodity-attributes
                             (update-in [:type] keyword)
                             (update-in [:exchange] keyword))
                         %)
                retrieved)
      "The commodity is not created"))

(deftest a-user-can-create-a-commodity-in-his-entity
  (assert-successful-create (create-a-commodity "john@doe.com")))

(deftest a-user-cannot-create-an-commodity-in-anothers-entity
  (assert-blocked-create (create-a-commodity "jane@doe.com")))

(deftest attempt-to-create-an-invalid-commodity
  (let [ctx (realize (env :db) context)
        user (find-user ctx "john@doe.com")
        entity (find-entity ctx "Personal")
        response (-> (req/request :post (path :api
                                              :entities
                                              (:id entity)
                                              :commodities))
                     (req/json-body (assoc commodity-attributes :exchange "notvalid"))
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)
        retrieved (coms/search (env :db) {:entity-id (:id entity)})]
    (assert-bad-request response)
    (is (= ["Exchange must be one of: amex, nasdaq, nyse"]
           (get-in body [::v/errors :exchange]))
        "The validation error is present")
    (is (not-any? #(= "AAPL" (:symbol %)) retrieved) "The record is not created")))

(defn- update-a-commodity
  [email]
  (let [ctx (realize (env :db) context)
        user (find-user ctx email)
        msft (find-commodity ctx "MSFT")
        response (-> (req/request :patch (path :api
                                              :commodities
                                              (:id msft)))
                     (req/json-body (assoc msft :name "Microsoft, Ltd."))
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)
        retrieved (coms/find-by-id (env :db) (:id msft))]
    [response body retrieved]))

(defn- assert-successful-update
  [[response body retrieved]]
  (assert-successful response)
  (is (selective= {:name "Microsoft, Ltd."}
                  body)
      "The updated commodity is returned in the body")
  (is (selective= {:name "Microsoft, Ltd."}
                 retrieved)
      "The record is updated in the database"))

(defn- assert-blocked-update
  [[response _ retrieved]]
  (assert-not-found response)
  (is (selective= {:name "Microsoft, Inc"}
                  retrieved)
      "The record is not updated in the database"))

(deftest a-user-can-update-a-commodity-in-his-entity
  (assert-successful-update (update-a-commodity "john@doe.com")))

(deftest a-user-cannot-update-a-commodity-in-anothers-entity
  (assert-blocked-update (update-a-commodity "jane@doe.com")))

(defn- delete-a-commodity
  [email]
  (let [ctx (realize (env :db) context)
        user (find-user ctx email)
        msft (find-commodity ctx "MSFT")
        response (-> (req/request :delete (path :api
                                                :commodities
                                                (:id msft)))
                     (add-auth user)
                     app)
        retrieved (coms/find-by-id (env :db) (:id msft))]
    [response retrieved]))

(defn- assert-successful-delete
  [[response retrieved]]
  (assert-successful response)
  (is (nil? retrieved)
      "The commodity cannot be retrieved after delete"))

(defn- assert-blocked-delete
  [[response retrieved]]
  (assert-not-found response)
  (is retrieved
      "The commodity can be retrieved after failed delete"))

(deftest a-user-can-delete-a-commodity-in-his-entity
  (assert-successful-delete (delete-a-commodity "john@doe.com")))

(deftest a-user-cannot-delete-a-commodity-in-anothers-entity
  (assert-blocked-delete (delete-a-commodity "jane@doe.com")))
