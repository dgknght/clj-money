(ns clj-money.api.users-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [cheshire.core :as json]
            [ring.mock.request :as req]
            [clj-money.test-helpers :refer [reset-db
                                            selective=]]
            [clj-money.web.test-helpers :refer [assert-unauthorized
                                                assert-successful]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.test-context :refer [realize
                                            find-user]]
            [clj-money.util :refer [path]]
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
    (assert-successful response)
    (is (selective= {:email "john@doe.com"
                     :first-name "John"
                     :last-name "Doe"}
                    (json/parse-string (:body response) true)))))

(deftest an-unauthenticated-user-cannot-get-me-info
  (let [response (app (req/request :get (path :api
                                              :users
                                              :me)))]
    (assert-unauthorized response)))
