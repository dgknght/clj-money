(ns clj-money.api.users-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [ring.mock.request :as req]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test]
            [clj-money.test-helpers :refer [reset-db
                                            edn-body
                                            parse-edn-body]]
            [clj-money.api.test-helper :refer [add-auth]]
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
        response (-> (req/request :get (path :api
                                             :users
                                             :me))
                     (req/header "accept" "application/edn")
                     (add-auth user)
                     app
                     parse-edn-body)]
    (is (http-success? response))
    (is (comparable? {:email "john@doe.com"
                      :first-name "John"
                      :last-name "Doe"}
                     (:edn-body response)))))

(deftest an-unauthenticated-user-cannot-get-me-info
  (let [response (-> (req/request :get (path :api
                                              :users
                                              :me))
                     (req/header "accept" "application/edn")
                     app)]
    (is (http-unauthorized? response))))

(deftest a-user-signs-in-directly
  (realize context)
  (let [response (-> (req/request :post (path :oapi
                                              :users
                                              :authenticate))
                     (edn-body {:email "john@doe.com"
                                :password "please01"})
                     app
                     parse-edn-body)]
    (is (http-success? response))
    (is (:auth-token (:edn-body response)))))
