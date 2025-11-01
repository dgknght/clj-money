(ns clj-money.api.commodities-test
  (:require [clojure.test :refer [deftest testing use-fixtures is]]
            [clojure.pprint :refer [pprint]]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [clj-money.entities :as entities]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            find-user
                                            find-entity
                                            find-commodity]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.api.test-helper :refer [request
                                               parse-body]]
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
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (-> (request :get (path :api
                          :entities
                          (:id (find-entity "Personal"))
                          :commodities
                          :count)
               :user (find-user email)
               :content-type content-type)
      app
      parse-body))

(defn- assert-successful-count
  [{:as response :keys [parsed-body]}]
  (is (http-success? response))
  (is (= {:count 2} parsed-body) "The body contains the count"))

(defn- assert-blocked-count
  [{:as response :keys [parsed-body]}]
  (is (http-success? response))
  (is (= {:count 0} parsed-body) "The body contains a count of zero"))

(deftest a-user-can-get-a-count-of-commodities-in-his-entity
  (with-context context
    (testing "default format (edn)"
      (assert-successful-count (get-a-count-of-commodities "john@doe.com")))
    (testing "json format"
      (assert-successful-count (get-a-count-of-commodities "john@doe.com"
                                                           :content-type "application/json")))))

(deftest a-user-cannot-get-a-count-of-commodities-in-anothers-entity
  (with-context context
    (assert-blocked-count (get-a-count-of-commodities "jane@doe.com"))))

(defn- get-a-list-of-commodities
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (-> (request :get (path :api
                          :entities
                          (:id (find-entity "Personal"))
                          :commodities)
               :user (find-user email)
               :content-type content-type)
      app
      parse-body))

(defn- assert-successful-list
  [{:as response :keys [parsed-body]}
   & {:keys [expected]
      :or {expected [#:commodity{:name "Microsoft, Inc"
                                 :symbol "MSFT"
                                 :type :stock
                                 :exchange :nasdaq}
                     #:commodity{:name "US Dollar"
                                 :symbol "USD"
                                 :type :currency}]}}]
  (is (http-success? response))
  (is (seq-of-maps-like? expected parsed-body)
      "The body contains the list of commodities"))

(defn- assert-blocked-list
  [{:as response :keys [parsed-body]}]
  (is (http-success? response))
  (is (empty? parsed-body) "The body is empty"))

(deftest a-user-can-get-a-list-of-commodities-in-his-entity
  (with-context context
    (testing "default format (edn)"
      (assert-successful-list (get-a-list-of-commodities "john@doe.com")))))

(deftest a-user-cannot-get-a-list-of-commodities-in-anothers-entity
  (with-context context
    (assert-blocked-list (get-a-list-of-commodities "jane@doe.com"))))

(defn- get-a-commodity
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (-> (request :get (path :api
                          :commodities
                          (:id (find-commodity "MSFT")))
               :user (find-user email)
               :content-type content-type)
      app
      parse-body))

(defn- assert-successful-get
  [{:as response :keys [parsed-body]}
   & {:keys [expected]
      :or {expected #:commodity{:name "Microsoft, Inc"
                                :symbol "MSFT"
                                :type :stock
                                :exchange :nasdaq}}}]
  (is (http-success? response))
  (is (comparable? expected parsed-body)
      "The specified commodity is returned in the response"))

(defn- assert-blocked-get
  [response]
  (is (http-not-found? response)))

(deftest a-user-can-get-a-commodity-in-his-entity
  (with-context context
    (testing "default format (edn)"
      (assert-successful-get (get-a-commodity "john@doe.com")))))

(deftest a-user-cannot-get-a-commodity-in-anothers-entity
  (with-context context
    (assert-blocked-get (get-a-commodity "jane@doe.com"))))

(def ^:private attributes
  #:commodity{:type :stock
              :name "Apple, Inc."
              :symbol "AAPL"
              :exchange :nasdaq
              :price-config {:price-config/enabled true}})

(defn- create-a-commodity
  [email & {:keys [content-type body]
            :or {content-type "application/edn"
                 body attributes}}]
  (let [entity (find-entity "Personal")
        response (-> (request :post (path :api
                                          :entities
                                          (:id entity)
                                          :commodities)
                              :user (find-user email)
                              :content-type content-type
                              :body body)
                     app
                     parse-body)
        retrieved (when-let [id (:id (:parsed-body response))]
                    (entities/find id :commodity))]
    [response retrieved]))

(defn- assert-successful-create
  [[{:as response :keys [parsed-body]} retrieved]
   & {:keys [expected expected-response]
      :or {expected attributes
           expected-response attributes}}]
  (is (http-created? response))
  (is (comparable? expected-response
                   parsed-body)
      "The newly created commodity is returned in the response")
  (is (comparable? expected
                   retrieved)
      "The new commodity can be retrieved from the database"))

(defn- assert-blocked-create
  [[response _ retrieved]]
  (is (http-not-found? response))
  (is (nil? retrieved)
      "The commodity is not created"))

(deftest a-user-can-create-a-commodity-in-his-entity
  (with-context context
    (testing "default format (edn)"
      (assert-successful-create (create-a-commodity "john@doe.com")))
    (testing "json format"
      (assert-successful-create
        (create-a-commodity "john@doe.com"
                           :content-type "application/json"
                           :body {:type "stock"
                                  :name "Google, Inc."
                                  :symbol "GOOG"
                                  :exchange "nasdaq"
                                  :_type "commodity"})
        :expected #:commodity{:type :stock
                              :name "Google, Inc."
                              :symbol "GOOG"
                              :exchange :nasdaq}
        :expected-response {:type "stock"
                            :name "Google, Inc."
                            :symbol "GOOG"
                            :exchange "nasdaq"
                            :_type "commodity"}))))

(deftest a-user-cannot-create-an-commodity-in-anothers-entity
  (with-context context
    (assert-blocked-create (create-a-commodity "jane@doe.com"))))

(deftest attempt-to-create-an-invalid-commodity
  (with-context context
    (let [entity (find-entity "Personal")
          response (-> (request :post (path :api
                                            :entities
                                            (:id entity)
                                            :commodities)
                                :user (find-user "john@doe.com")
                                :content-type "application/edn"
                                :body (assoc attributes :commodity/exchange "notvalid"))
                       app
                       parse-body)]
      (is (http-bad-request? response))
      (is (= ["Exchange must be amex, nasdaq, nyse, or otc"]
             (get-in response [:parsed-body :commodity/exchange])))
      (is (empty? (entities/select {:commodity/entity entity
                                  :commodity/symbol "AAPL"}))
          "The record is not created"))))

(defn- update-a-commodity
  [email & {:keys [content-type body]
            :or {content-type "application/edn"
                 body #:commodity{:name "Microsoft, Ltd."}}}]
  (let [msft (find-commodity "MSFT")
        response (-> (request :patch (path :api
                                           :commodities
                                           (:id msft))
                              :user (find-user email)
                              :content-type content-type
                              :body body)
                     app
                     parse-body)]
    [response (entities/find msft)]))

(defn- assert-successful-update
  [[{:as response :keys [parsed-body]} retrieved]
   & {:keys [expected expected-response]
      :or {expected {:commodity/name "Microsoft, Ltd."}
           expected-response {:commodity/name "Microsoft, Ltd."}}}]
  (is (http-success? response))
  (is (comparable? expected-response
                   parsed-body)
      "The returned value has the updated attributes")
  (is (comparable? expected
                   retrieved)
      "The retrieved value has the updated attributes"))

(defn- assert-blocked-update
  [[response retrieved]]
  (is (http-not-found? response))
  (is (comparable? {:commodity/name "Microsoft, Inc"}
                   retrieved)
      "The retrieved value has the original attributes"))

(deftest a-user-can-update-a-commodity-in-his-entity
  (with-context context
    (testing "default format (edn)"
      (assert-successful-update (update-a-commodity "john@doe.com")))
    (testing "json format"
      (assert-successful-update
        (update-a-commodity "john@doe.com"
                           :content-type "application/json"
                           :body {:name "JSON Corporation"
                                  :_type "commodity"})
        :expected {:commodity/name "JSON Corporation"}
        :expected-response {:name "JSON Corporation"
                            :_type "commodity"}))))

(deftest a-user-cannot-update-a-commodity-in-anothers-entity
  (with-context context
    (assert-blocked-update (update-a-commodity "jane@doe.com"))))

(defn- delete-a-commodity
  [email]
  (let [msft (find-commodity "MSFT")
        response (-> (request :delete (path :api
                                            :commodities
                                            (:id msft))
                              :user (find-user email))
                     app)
        retrieved (entities/find msft)]
    [response retrieved]))

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
  (with-context context
    (testing "default format (edn)"
      (assert-successful-delete (delete-a-commodity "john@doe.com")))))

(deftest a-user-cannot-delete-a-commodity-in-anothers-entity
  (with-context context
    (assert-blocked-delete (delete-a-commodity "jane@doe.com"))))
