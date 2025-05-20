(ns clj-money.web.images-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [ring.mock.request :as req]
            [java-time.api :as t]
            [dgknght.app-lib.web :refer [path]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.test-context :refer [basic-context
                                            with-context
                                            find-image
                                            find-user]]
            [clj-money.web.auth :as auth]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def image-context
  (conj basic-context
        #:transaction{:transaction-date (t/local-date 2015 1 1)
                      :entity "Personal"
                      :description "Paycheck"
                      :debit-account "Checking"
                      :credit-account "Salary"
                      :quantity 1000M}
        #:image{:original-filename "attachment.jpg"
                :user "john@doe.com"
                :content-type "image/jpg"
                :content "resources/fixtures/attachment.jpg"}
        #:attachment{:image "attachment.jpg"
                     :transaction [(t/local-date 2015 1 1) "Paycheck"]}))

(defn- add-auth-cookie
  [req user]
  (if user
    (req/cookie req :auth-token (auth/make-token user))
    req))

(defn- get-image
  [email]
  (with-context image-context
    (let [image (find-image "attachment.jpg")]
      (-> (req/request :get (path :app
                                  :images
                                  (:id image)))
          (add-auth-cookie (when email
                             (find-user email)))
          app))))

(defn- assert-successful-get
  [response]
  ; TODO: check response body?
  (is (= 200 (:status response))))

(defn- assert-not-found
  [response]
  ; TODO: check response body?
  (is (= 404 (:status response))))

(defn- assert-unauthorized
  [response]
  ; TODO: check response body?
  (is (= 401 (:status response))))

(deftest a-user-can-view-an-image-in-his-entity
  (assert-successful-get (get-image "john@doe.com")))

(deftest a-user-cannot-view-an-image-in-anothers-entity
  (assert-not-found (get-image "jane@doe.com")))

(deftest an-unauthenticated-user-cannot-view-an-image
  (assert-unauthorized (get-image nil)))
