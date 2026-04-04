(ns clj-money.api.users-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.test]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.api.test-helper :refer [parse-body
                                               request]]
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

(def ^:private admin-ctx
  [#:user{:email "admin@example.com"
          :first-name "Admin"
          :last-name "User"
          :password "please01"
          :roles #{:admin}}
   #:user{:email "user1@example.com"
          :first-name "First"
          :last-name "User"
          :password "please01"
          :roles #{:user}}
   #:user{:email "user2@example.com"
          :first-name "Second"
          :last-name "User"
          :password "please01"
          :roles #{:user}}])

(deftest an-admin-can-list-users
  (with-context admin-ctx
    (let [response (-> (request :get (path :api :users)
                                :user (find-user "admin@example.com"))
                       app
                       parse-body)]
      (is (http-success? response))
      (is (seq-of-maps-like?
            [#:user{:email "admin@example.com"}
             #:user{:email "user1@example.com"}
             #:user{:email "user2@example.com"}]
            (:parsed-body response))
          "All users are returned"))))

(deftest a-non-admin-cannot-list-users
  (with-context admin-ctx
    (let [response (-> (request :get (path :api :users)
                                :user (find-user "user1@example.com"))
                       app)]
      (is (http-forbidden? response)))))

(deftest a-user-gets-his-own-info
  (with-context context
    (testing "default format"
      (let [response (-> (request :get (path :api
                                             :users
                                             :me)
                                  :content-type "application/edn"
                                  :user (find-user "john@doe.com"))
                         app
                         parse-body)]
        (is (http-success? response))
        (is (comparable? #:user{:email "john@doe.com"
                                :first-name "John"
                                :last-name "Doe"}
                         (:parsed-body response)))))
    (testing "json format"
      (let [response (-> (request :get (path :api
                                             :users
                                             :me)
                                  :content-type "application/json"
                                  :user (find-user "john@doe.com"))
                         app
                         parse-body)]
        (is (http-success? response))
        (is (comparable? {:email "john@doe.com"
                          :firstName "John"
                          :lastName "Doe"
                          :_type "user"}
                         (:parsed-body response)))))))

(deftest an-unauthenticated-user-cannot-get-me-info
  (let [response (-> (request :get (path :api
                                         :users
                                         :me)
                              :content-type "application/edn")
                     app)]
    (is (http-unauthorized? response))))

(deftest a-user-signs-in-directly
  (with-context context
    (testing "default format"
      (let [response (-> (request :post (path :oapi
                                              :users
                                              :authenticate)
                                  :content-type "application/edn"
                                  :body {:email "john@doe.com"
                                         :password "please01"})
                         app
                         parse-body)]
        (is (http-success? response))
        (is (:auth-token (:parsed-body response)))))
    (testing "json format"
      (let [response (-> (request :post (path :oapi
                                              :users
                                              :authenticate)
                                  :content-type "application/json"
                                  :body {:email "john@doe.com"
                                         :password "please01"})
                         app
                         parse-body)]
        (is (http-success? response))
        (is (:authToken (:parsed-body response)))))))
