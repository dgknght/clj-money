(ns clj-money.api.invitations-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.test]
            [clj-money.json]
            [clj-money.entities :as entities]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.api.test-helper :refer [parse-body
                                               request]]
            [clj-money.test-context :refer [with-context
                                            find-user
                                            find-invitation]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private admin-ctx
  [#:user{:email "admin@example.com"
          :first-name "Admin"
          :last-name "User"
          :password "please01"
          :roles #{:admin}}])

(def ^:private list-ctx
  (conj admin-ctx
        #:user{:email "other@example.com"
               :first-name "Other"
               :last-name "User"
               :password "please01"
               :roles #{:admin}}
        #:invitation{:recipient "first@example.com"
                     :status :unsent
                     :user "admin@example.com"}
        #:invitation{:recipient "second@example.com"
                     :status :unsent
                     :user "admin@example.com"}))

(deftest an-admin-can-get-a-list-of-invitations
  (with-context list-ctx
    (let [response (-> (request :get (path :api :invitations)
                                :user (find-user "admin@example.com"))
                       app
                       parse-body)]
      (is (http-success? response))
      (is (seq-of-maps-like?
            [#:invitation{:recipient "first@example.com"}
             #:invitation{:recipient "second@example.com"}]
            (:parsed-body response))
          "The response contains the admin's invitations"))))

(deftest an-admin-can-see-all-invitations
  (with-context list-ctx
    (let [response (-> (request :get (path :api :invitations)
                                :user (find-user "other@example.com"))
                       app
                       parse-body)]
      (is (http-success? response))
      (is (seq-of-maps-like?
            [#:invitation{:recipient "first@example.com"}
             #:invitation{:recipient "second@example.com"}]
            (:parsed-body response))
          "All site invitations are visible to any admin"))))

(deftest an-admin-can-create-an-invitation
  (with-context admin-ctx
    (let [user (find-user "admin@example.com")
          response (-> (request :post (path :api :invitations)
                                :user user
                                :body #:invitation{:recipient "new@example.com"
                                                   :status :unsent})
                       app
                       parse-body)]
      (is (http-created? response))
      (is (comparable? #:invitation{:recipient "new@example.com"
                                    :status :unsent}
                       (:parsed-body response))
          "The new invitation is returned in the response")
      (is (seq-of-maps-like?
            [#:invitation{:recipient "new@example.com"}]
            (entities/select {:invitation/user user}))
          "The invitation is retrievable from the database"))))

(def ^:private show-ctx
  (conj list-ctx
        #:user{:email "non-admin@example.com"
               :first-name "Non"
               :last-name "Admin"
               :password "please01"}))

(deftest an-admin-can-view-an-invitation
  (with-context show-ctx
    (let [inv (find-invitation "first@example.com")
          response (-> (request :get (path :api :invitations (:id inv))
                                :user (find-user "admin@example.com"))
                       app
                       parse-body)]
      (is (http-success? response))
      (is (comparable? #:invitation{:recipient "first@example.com"}
                       (:parsed-body response))
          "The invitation is returned in the response"))))

(deftest any-admin-can-view-any-invitation
  (with-context show-ctx
    (let [inv (find-invitation "first@example.com")
          response (-> (request :get (path :api :invitations (:id inv))
                                :user (find-user "other@example.com"))
                       app
                       parse-body)]
      (is (http-success? response))
      (is (comparable? #:invitation{:recipient "first@example.com"}
                       (:parsed-body response))))))

(deftest a-non-admin-cannot-view-an-invitation
  (with-context show-ctx
    (let [inv (find-invitation "first@example.com")
          response (-> (request :get (path :api :invitations (:id inv))
                                :user (find-user "non-admin@example.com"))
                       app)]
      (is (http-not-found? response)))))

(deftest an-admin-can-update-an-invitation
  (with-context show-ctx
    (let [inv (find-invitation "first@example.com")
          response (-> (request :patch (path :api :invitations (:id inv))
                                :user (find-user "admin@example.com")
                                :body #:invitation{:status :sent})
                       app
                       parse-body)]
      (is (http-success? response))
      (is (comparable? #:invitation{:recipient "first@example.com"
                                    :status :sent}
                       (:parsed-body response))
          "The updated invitation is returned in the response"))))

(deftest any-admin-can-update-any-invitation
  (with-context show-ctx
    (let [inv (find-invitation "first@example.com")
          response (-> (request :patch (path :api :invitations (:id inv))
                                :user (find-user "other@example.com")
                                :body #:invitation{:status :sent})
                       app
                       parse-body)]
      (is (http-success? response))
      (is (comparable? #:invitation{:status :sent}
                       (:parsed-body response))))))

(deftest an-admin-can-delete-an-invitation
  (with-context show-ctx
    (let [inv (find-invitation "first@example.com")
          response (-> (request :delete (path :api :invitations (:id inv))
                                :user (find-user "admin@example.com"))
                       app)]
      (is (http-success? response))
      (is (nil? (entities/find inv))
          "The invitation is no longer retrievable"))))

(deftest any-admin-can-delete-any-invitation
  (with-context show-ctx
    (let [inv (find-invitation "first@example.com")
          response (-> (request :delete (path :api :invitations (:id inv))
                                :user (find-user "other@example.com"))
                       app)]
      (is (http-success? response))
      (is (nil? (entities/find inv))
          "The invitation is no longer retrievable"))))
