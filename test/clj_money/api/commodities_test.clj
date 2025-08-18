(ns clj-money.api.commodities-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [clojure.pprint :refer [pprint]]
            [ring.mock.request :as req]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [clj-money.models :as models]
            [clj-money.models.ref]
            [clj-money.db.ref]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            find-user
                                            find-entity
                                            find-commodity]]
            [clj-money.test-helpers :refer [reset-db
                                            edn-body
                                            parse-edn-body]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private context
  (concat (->> ["john@doe.com" "jane@doe.com"]
               (mapv #(factory :user {:user/email %})))
          [#:entity{:name "Personal"
                    :user "john@doe.com"}
           #:entity{:name "Business"
                    :user "jane@doe.com"}
           #:commodity{:name "US Dollar"
                       :symbol "USD"
                       :type :currency
                       :entity "Personal"}
           #:commodity{:name "Microsoft, Inc"
                       :symbol "MSFT"
                       :type :stock
                       :exchange :nasdaq
                       :entity "Personal"}]))

(defn- get-a-count-of-commodities
  [email]
  (with-context context
    (-> (req/request :get (path :api
                                :entities
                                (:id (find-entity "Personal"))
                                :commodities
                                :count))
        (add-auth (find-user email))
        app
        parse-edn-body)))

(defn- assert-successful-count
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (= {:count 2} edn-body) "The body contains the count"))

(defn- assert-blocked-count
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (= {:count 0} edn-body) "The body contains a count of zero"))

(deftest a-user-can-get-a-count-of-commodities-in-his-entity
  (assert-successful-count (get-a-count-of-commodities "john@doe.com")))

(deftest a-user-cannot-get-a-count-of-commodities-in-anothers-entity
  (assert-blocked-count (get-a-count-of-commodities "jane@doe.com")))

(defn- get-a-list-of-commodities
  [email]
  (with-context context
    (-> (req/request :get (path :api
                                :entities
                                (:id (find-entity "Personal"))
                                :commodities))
        (add-auth (find-user email))
        app
        parse-edn-body)))

(defn- assert-successful-list
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (seq-of-maps-like? [#:commodity{:name "Microsoft, Inc"
                                      :symbol "MSFT"
                                      :type :stock
                                      :exchange :nasdaq}
                          #:commodity{:name "US Dollar"
                                      :symbol "USD"
                                      :type :currency}]
                         edn-body)
      "The body contains the list of commodities"))

(defn- assert-blocked-list
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (empty? edn-body) "The body is empty"))

(deftest a-user-can-get-a-list-of-commodities-in-his-entity
  (assert-successful-list (get-a-list-of-commodities "john@doe.com")))

(deftest a-user-cannot-get-a-list-of-commodities-in-anothers-entity
  (assert-blocked-list (get-a-list-of-commodities "jane@doe.com")))

(defn- get-a-commodity
  [email]
  (with-context context
    (-> (req/request :get (path :api
                                :commodities
                                (:id (find-commodity "MSFT"))))
        (add-auth (find-user email))
        app
        parse-edn-body)))

(defn- assert-successful-get
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (comparable? #:commodity{:name "Microsoft, Inc"
                               :symbol "MSFT"
                               :type :stock
                               :exchange :nasdaq}
                   edn-body)
      "The specified commodity is returned in the response"))

(defn- assert-blocked-get
  [response]
  (is (http-not-found? response)))

(deftest a-user-can-get-a-commodity-in-his-entity
  (assert-successful-get (get-a-commodity "john@doe.com")))

(deftest a-user-cannot-get-a-commodity-in-anothers-entity
  (assert-blocked-get (get-a-commodity "jane@doe.com")))

(def ^:private attributes
  #:commodity{:type :stock
              :name "Apple, Inc."
              :symbol "AAPL"
              :exchange :nasdaq
              :price-config {:price-config/enabled true}})

(defn- create-a-commodity
  [email]
  (with-context context
    (let [entity (find-entity "Personal")
          response (-> (req/request :post (path :api
                                                :entities
                                                (:id entity)
                                                :commodities))
                       (edn-body attributes)
                       (add-auth (find-user email))
                       app
                       parse-edn-body)]
      [response (when-let [id (-> response :edn-body :id)]
                  (models/find id :commodity))])))

(defn- assert-successful-create
  [[{:as response :keys [edn-body]} retrieved]]
  (is (http-created? response))
  (is (comparable? attributes
                   edn-body)
      "The newly created commodity is returned in the response")
  (is (comparable? #:commodity{:type :stock
                               :name "Apple, Inc."
                               :symbol "AAPL"
                               :exchange :nasdaq
                               :price-config {:price-config/enabled true}}
                   retrieved)
      "The new commodity can be retrieved from the database"))

(defn- assert-blocked-create
  [[response _ retrieved]]
  (is (http-not-found? response))
  (is (nil? retrieved)
      "The commodity is not created"))

(deftest a-user-can-create-a-commodity-in-his-entity
  (assert-successful-create (create-a-commodity "john@doe.com")))

(deftest a-user-cannot-create-an-commodity-in-anothers-entity
  (assert-blocked-create (create-a-commodity "jane@doe.com")))

(deftest attempt-to-create-an-invalid-commodity
  (with-context context
    (let [entity (find-entity "Personal")
          response (-> (req/request :post (path :api
                                                :entities
                                                (:id entity)
                                                :commodities))
                       (edn-body (assoc attributes :commodity/exchange "notvalid"))
                       (add-auth (find-user "john@doe.com"))
                       app
                       parse-edn-body)]
      (is (http-bad-request? response))
      (is (= ["Exchange must be amex, nasdaq, nyse, or otc"]
             (get-in response [:edn-body :commodity/exchange])))
      (is (empty? (models/select {:commodity/entity entity
                                  :commodity/symbol "AAPL"}))
          "The record is not created"))))

(defn- update-a-commodity
  [email]
  (with-context context
    (let [msft (find-commodity "MSFT")
          response (-> (req/request :patch (path :api
                                                 :commodities
                                                 (:id msft)))
                       (edn-body (assoc msft :commodity/name "Microsoft, Ltd."))
                       (add-auth (find-user email))
                       app
                       parse-edn-body)]
      [response (models/find msft)])))

(defn- assert-successful-update
  [[{:as response :keys [edn-body]} retrieved]]
  (is (http-success? response))
  (is (comparable? {:commodity/name "Microsoft, Ltd."}
                   edn-body)
      "The returned value has the updated attributes")
  (is (comparable? {:commodity/name "Microsoft, Ltd."}
                   retrieved)
      "The retrieved value has the updated attributes"))

(defn- assert-blocked-update
  [[response retrieved]]
  (is (http-not-found? response))
  (is (comparable? {:commodity/name "Microsoft, Inc"}
                   retrieved)
      "The retrieved value has the original attributes"))

(deftest a-user-can-update-a-commodity-in-his-entity
  (assert-successful-update (update-a-commodity "john@doe.com")))

(deftest a-user-cannot-update-a-commodity-in-anothers-entity
  (assert-blocked-update (update-a-commodity "jane@doe.com")))

(defn- delete-a-commodity
  [email]
  (with-context context
    (let [msft (find-commodity "MSFT")
          response (-> (req/request :delete (path :api
                                                  :commodities
                                                  (:id msft)))
                       (add-auth (find-user email))
                       app)
          retrieved (models/find msft)]
      [response retrieved])))

(defn- assert-successful-delete
  [[response retrieved]]
  (is (http-success? response))
  (is (nil? retrieved)
      "The commodity cannot be retrieved after delete"))

(defn- assert-blocked-delete
  [[response retrieved]]
  (is (http-not-found? response))
  (is retrieved
      "The commodity can be retrieved after failed delete"))

(deftest a-user-can-delete-a-commodity-in-his-entity
  (assert-successful-delete (delete-a-commodity "john@doe.com")))

(deftest a-user-cannot-delete-a-commodity-in-anothers-entity
  (assert-blocked-delete (delete-a-commodity "jane@doe.com")))
