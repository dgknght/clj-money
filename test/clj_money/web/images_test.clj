(ns clj-money.web.images-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [ring.mock.request :as req]
            [clj-time.core :as t]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.test-context :refer [basic-context
                                            realize
                                            find-image
                                            find-user]]
            [clj-money.util :refer [path]]
            [clj-money.web.auth :as auth]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def image-context
  (assoc basic-context
         :transactions [{:transaction-date (t/local-date 2015 1 1)
                         :description "Paycheck"
                         :debit-account-id "Checking"
                         :credit-account-id "Salary"
                         :quantity 1000M}]
         :images [{:original-filename "attachment.jpg"
                   :content-type "image/jpg"
                   :body "resources/fixtures/attachment.jpg"}]
         :attachments [{:image-id "attachment.jpg"
                        :transaction-id {:transaction-date (t/local-date 2015 1 1)
                                         :description "Paycheck"}}]))

(defn- add-auth-cookie
  [req user]
  (req/cookie req :auth-token (auth/make-token user)))

(defn- get-image
  [email]
  (let [ctx (realize image-context)
        image (find-image ctx "attachment.jpg")
        user (when email (find-user ctx email))]
    (-> (req/request :get (path :images
                                (:id image)))
        (add-auth-cookie user)
        app)))

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
