(ns clj-money.api.users-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [ring.mock.request :as req]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.api.test-helper :refer [add-auth
                                               parse-json-body]]
            [clj-money.test-context :refer [with-context
                                            find-user]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private context
  [#:user{:email "john@doe.com"
          :first-name "John"
          :last-name "Doe"
          :password "please01"
          :password-confirmation "please01"}])

(deftest a-user-gets-his-own-info
  (with-context context
    (let [response (-> (req/request :get (path :api
                                               :users
                                               :me))
                       (add-auth (find-user "john@doe.com"))
                       app
                       parse-json-body)]
      (is (http-success? response))
      (is (comparable? #:user{:email "john@doe.com"
                              :first-name "John"
                              :last-name "Doe"}
                       (:json-body response))))))

(deftest an-unauthenticated-user-cannot-get-me-info
  (let [response (app (req/request :get (path :api
                                              :users
                                              :me)))]
    (is (http-unauthorized? response))))

(deftest a-user-signs-in-directly
  (with-context context
    (let [response (-> (req/request :post (path :oapi
                                                :users
                                                :authenticate))
                       (req/json-body {:email "john@doe.com"
                                       :password "please01"})
                       app
                       parse-json-body)]
      (is (http-success? response))
      (is (:auth-token (:json-body response))))))
