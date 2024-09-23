(ns clj-money.api.users-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [cheshire.core :as json]
            [ring.mock.request :as req]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.api.test-helper :refer [add-auth
                                               parse-json-body]]
            [clj-money.test-context :refer [realize
                                            find-user]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private context
  {:users [{:email "john@doe.com"
            :first-name "John"
            :last-name "Doe"
            :password "please01"
            :password-confirmation "please01"}]})

(deftest a-user-gets-his-own-info
  (let [ctx (realize context)
        user (find-user ctx "john@doe.com")
        response (app (-> (req/request :get (path :api
                                                  :users
                                                  :me))
                          (add-auth user)))]
    (is (http-success? response))
    (is (comparable? {:email "john@doe.com"
                      :first-name "John"
                      :last-name "Doe"}
                     (json/parse-string (:body response) true)))))

(deftest an-unauthenticated-user-cannot-get-me-info
  (let [response (app (req/request :get (path :api
                                              :users
                                              :me)))]
    (is (http-unauthorized? response))))

(deftest a-user-signs-in-directly
  (realize context)
  (let [response (-> (req/request :post (path :oapi
                                              :users
                                              :authenticate))
                     (req/json-body {:email "john@doe.com"
                                     :password "please01"})
                     app
                     parse-json-body)]
    (is (http-success? response))
    (is (:auth-token (:json-body response)))))
