(ns clj-money.api.invitations-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [java-time.api :as t]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.test :refer [with-mail-capture]]
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
                     :token "token-first-123"
                     :expires-at (t/plus (t/instant) (t/days 10))
                     :user "admin@example.com"}
        #:invitation{:recipient "second@example.com"
                     :status :unsent
                     :token "token-second-123"
                     :expires-at (t/plus (t/instant) (t/days 10))
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

(deftest an-admin-can-create-an-invitation-and-send-immediately
  (with-context admin-ctx
    (let [user (find-user "admin@example.com")]
      (with-mail-capture [mailbox]
        (let [response (-> (request :post (path :api :invitations)
                                    :user user
                                    :body #:invitation{:recipient "new@example.com"
                                                       :status :sent})
                           app
                           parse-body)]
          (is (http-created? response))
          (is (comparable? #:invitation{:recipient "new@example.com"
                                        :status :sent}
                           (:parsed-body response))
              "The new invitation is returned with :sent status")
          (is (seq-of-maps-like?
                [#:invitation{:recipient "new@example.com"}]
                (entities/select {:invitation/user user}))
              "The invitation is retrievable from the database")
          (is (= 1 (count @mailbox))
              "One email is sent")
          (is (comparable? {:to "new@example.com"}
                           (first @mailbox))
              "The email is sent to the recipient"))))))

(deftest an-admin-can-create-an-invitation-without-sending
  (with-context admin-ctx
    (let [user (find-user "admin@example.com")]
      (with-mail-capture [mailbox]
        (let [response (-> (request :post (path :api :invitations)
                                    :user user
                                    :body #:invitation{:recipient "new@example.com"})
                           app
                           parse-body)]
          (is (http-created? response))
          (is (comparable? #:invitation{:recipient "new@example.com"
                                        :status :unsent}
                           (:parsed-body response))
              "The new invitation is returned with :unsent status")
          (is (zero? (count @mailbox))
              "No email is sent"))))))

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
                                :body #:invitation{:note "Hello there"})
                       app
                       parse-body)]
      (is (http-success? response))
      (is (comparable? #:invitation{:recipient "first@example.com"
                                    :note "Hello there"}
                       (:parsed-body response))
          "The updated invitation is returned in the response"))))

(deftest any-admin-can-update-any-invitation
  (with-context show-ctx
    (let [inv (find-invitation "first@example.com")
          response (-> (request :patch (path :api :invitations (:id inv))
                                :user (find-user "other@example.com")
                                :body #:invitation{:note "Hello there"})
                       app
                       parse-body)]
      (is (http-success? response))
      (is (comparable? #:invitation{:note "Hello there"}
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

(def ^:private sent-inv-ctx
  (conj admin-ctx
        #:invitation{:recipient "sent@example.com"
                     :status :sent
                     :token "token-sent-123"
                     :expires-at (t/plus (t/instant) (t/days 10))
                     :user "admin@example.com"}))

(deftest a-sent-invitation-cannot-be-deleted
  (with-context sent-inv-ctx
    (let [inv (find-invitation "sent@example.com")
          response (-> (request :delete (path :api :invitations (:id inv))
                                :user (find-user "admin@example.com"))
                       app)]
      (is (http-unprocessable? response)))))

(deftest a-sent-invitation-cannot-be-updated
  (with-context sent-inv-ctx
    (let [inv (find-invitation "sent@example.com")
          response (-> (request :patch (path :api :invitations (:id inv))
                                :user (find-user "admin@example.com")
                                :body #:invitation{:note "Hello"})
                       app)]
      (is (http-unprocessable? response)))))

(deftest an-admin-can-send-an-invitation
  (with-context show-ctx
    (let [inv (find-invitation "first@example.com")
          user (find-user "admin@example.com")]
      (with-mail-capture [mailbox]
        (let [response (-> (request :post (path :api :invitations (:id inv) :send)
                                    :user user)
                           app
                           parse-body)]
          (is (http-success? response))
          (is (comparable? #:invitation{:status :sent}
                           (:parsed-body response))
              "The invitation status is updated to :sent")
          (is (= 1 (count @mailbox))
              "One email is sent")
          (is (comparable? {:to "first@example.com"}
                           (first @mailbox))
              "The email is sent to the recipient"))))))

(def ^:private token-ctx
  (conj admin-ctx
        #:invitation{:recipient "invited@example.com"
                     :status :sent
                     :token "test-token-123"
                     :expires-at (t/plus (t/instant) (t/days 10))
                     :user "admin@example.com"}))

(deftest anyone-can-find-an-invitation-by-token
  (with-context token-ctx
    (let [response (-> (request :get (path :oapi :invitations "test-token-123" :accept))
                       app
                       parse-body)]
      (is (http-success? response))
      (is (comparable? #:invitation{:recipient "invited@example.com"}
                       (:parsed-body response))
          "The invitation is returned"))))

(deftest a-bad-token-returns-not-found
  (with-context token-ctx
    (let [response (-> (request :get (path :oapi :invitations "bad-token" :accept))
                       app)]
      (is (http-not-found? response)))))

(deftest a-recipient-can-accept-an-invitation
  (with-context token-ctx
    (let [response (-> (request :post (path :oapi :invitations "test-token-123" :accept)
                                :body {:user/first-name "New"
                                       :user/last-name "User"
                                       :user/password "please01"})
                       app
                       parse-body)]
      (is (http-created? response))
      (is (:auth-token (:parsed-body response))
          "An auth token is returned")
      (is (comparable? #:user{:email "invited@example.com"
                              :first-name "New"
                              :last-name "User"}
                       (:user (:parsed-body response)))
          "The new user is returned")
      (is (comparable? #:invitation{:status :accepted}
                       (entities/find-by {:invitation/recipient "invited@example.com"}))
          "The invitation status is updated to :accepted"))))

(deftest a-recipient-can-decline-an-invitation
  (with-context token-ctx
    (let [response (-> (request :post (path :oapi :invitations "test-token-123" :decline))
                       app)]
      (is (http-success? response))
      (is (comparable? #:invitation{:status :declined}
                       (entities/find-by {:invitation/recipient "invited@example.com"}))
          "The invitation status is updated to :declined"))))

(deftest a-bad-decline-token-returns-not-found
  (with-context token-ctx
    (let [response (-> (request :post (path :oapi :invitations "bad-token" :decline))
                       app)]
      (is (http-not-found? response)))))

(def ^:private expired-inv-ctx
  (conj admin-ctx
        #:invitation{:recipient "expired@example.com"
                     :status :sent
                     :token "expired-token-123"
                     :expires-at (t/minus (t/instant) (t/days 1))
                     :user "admin@example.com"}))

(deftest an-expired-invitation-cannot-be-found-by-token
  (with-context expired-inv-ctx
    (let [response (-> (request :get (path :oapi :invitations "expired-token-123" :accept))
                       app)]
      (is (= 410 (:status response))
          "A 410 Gone response is returned"))))

(deftest an-expired-invitation-cannot-be-accepted
  (with-context expired-inv-ctx
    (let [response (-> (request :post (path :oapi :invitations "expired-token-123" :accept)
                                :body {:user/first-name "New"
                                       :user/last-name "User"
                                       :user/password "please01"})
                       app)]
      (is (= 410 (:status response))
          "A 410 Gone response is returned"))))

(deftest an-expired-invitation-cannot-be-declined
  (with-context expired-inv-ctx
    (let [response (-> (request :post (path :oapi :invitations "expired-token-123" :decline))
                       app)]
      (is (= 410 (:status response))
          "A 410 Gone response is returned"))))
