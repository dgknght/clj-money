(ns clj-money.api.cli-auth-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [java-time.api :as t]
            [dgknght.app-lib.test-assertions]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.api.test-helper :refer [parse-body request]]
            [clj-money.test-context :refer [with-context find-user]]
            [clj-money.web.server :refer [app]]
            [clj-money.entities :as entities]
            [clj-money.entities.cli-auth-sessions :as cli-sessions]))

(use-fixtures :each reset-db)

(def ^:private context
  [#:user{:email "john@doe.com"
          :first-name "John"
          :last-name "Doe"
          :password "please01"
          :password-confirmation "please01"}])

(deftest initiate-cli-auth-flow
  (testing "A CLI application can initiate the device flow"
    (let [response (-> (request :post "/oapi/cli/auth/device"
                                :content-type "application/json")
                       app
                       parse-body)]
      (is (http-success? response))
      (is (contains? (:parsed-body response) :deviceCode))
      (is (contains? (:parsed-body response) :userCode))
      (is (contains? (:parsed-body response) :verificationUri))
      (is (contains? (:parsed-body response) :verificationUriComplete))
      (is (= 900 (:expiresIn (:parsed-body response))))
      (is (= 5 (:interval (:parsed-body response)))))))

(deftest poll-for-token-pending
  (testing "Polling returns authorization_pending when user has not
            approved"
    (with-context context
      (let [session (cli-sessions/create-session)
            device-code (:cli-auth-session/device-code session)
            response (-> (request :post "/oapi/cli/auth/token"
                                  :content-type "application/json"
                                  :body {:device-code device-code})
                         app
                         parse-body)]
        (is (= 400 (:status response)))
        (is (= "authorization_pending"
               (:error (:parsed-body response))))))))

(deftest poll-for-token-approved
  (testing "Polling returns access token when user has approved"
    (with-context context
      (let [user (find-user "john@doe.com")
            session (cli-sessions/create-session)
            _ (cli-sessions/approve-session session user)
            device-code (:cli-auth-session/device-code session)
            response (-> (request :post "/oapi/cli/auth/token"
                                  :content-type "application/json"
                                  :body {:device-code device-code})
                         app
                         parse-body)]
        (is (http-success? response))
        (is (contains? (:parsed-body response) :accessToken))
        (is (= "Bearer" (:tokenType (:parsed-body response))))
        (is (= 900 (:expiresIn (:parsed-body response))))))))

(deftest poll-for-token-denied
  (testing "Polling returns access_denied when user has denied"
    (with-context context
      (let [session (cli-sessions/create-session)
            _ (cli-sessions/deny-session session)
            device-code (:cli-auth-session/device-code session)
            response (-> (request :post "/oapi/cli/auth/token"
                                  :content-type "application/json"
                                  :body {:device-code device-code})
                         app
                         parse-body)]
        (is (= 403 (:status response)))
        (is (= "access_denied"
               (:error (:parsed-body response))))))))

(deftest poll-for-token-expired
  (testing "Polling returns expired_token when session has expired"
    (with-context context
      (let [session (entities/put
                     {:cli-auth-session/status :pending
                      :cli-auth-session/expires-at
                      (t/minus (t/instant) (t/minutes 1))})
            device-code (:cli-auth-session/device-code session)
            response (-> (request :post "/oapi/cli/auth/token"
                                  :content-type "application/json"
                                  :body {:device-code device-code})
                         app
                         parse-body)]
        (is (= 400 (:status response)))
        (is (= "expired_token"
               (:error (:parsed-body response))))))))

(deftest poll-for-token-invalid-code
  (testing "Polling returns error for invalid device code"
    (let [response (-> (request :post "/oapi/cli/auth/token"
                                :content-type "application/json"
                                :body {:device-code "invalid-code"})
                       app
                       parse-body)]
      (is (= 400 (:status response)))
      (is (= "invalid_request"
             (:error (:parsed-body response)))))))

(deftest approve-cli-auth-session
  (testing "An authenticated user can approve a CLI auth session"
    (with-context context
      (let [user (find-user "john@doe.com")
            session (cli-sessions/create-session)
            user-code (:cli-auth-session/user-code session)
            response (-> (request :post "/api/cli/auth/approve"
                                  :content-type "application/json"
                                  :user user
                                  :body {:user-code user-code})
                         app
                         parse-body)]
        (is (http-success? response))
        (is (true? (:success (:parsed-body response))))
        ;; Verify session is now approved
        (let [updated (cli-sessions/find-by-user-code user-code)]
          (is (= :approved (:cli-auth-session/status updated)))
          (is (= (:id user) (:cli-auth-session/user-id updated))))))))

(deftest deny-cli-auth-session
  (testing "An authenticated user can deny a CLI auth session"
    (with-context context
      (let [user (find-user "john@doe.com")
            session (cli-sessions/create-session)
            user-code (:cli-auth-session/user-code session)
            response (-> (request :post "/api/cli/auth/deny"
                                  :content-type "application/json"
                                  :user user
                                  :body {:user-code user-code})
                         app
                         parse-body)]
        (is (http-success? response))
        (is (true? (:success (:parsed-body response))))
        ;; Verify session is now denied
        (let [updated (cli-sessions/find-by-user-code user-code)]
          (is (= :denied (:cli-auth-session/status updated))))))))

(deftest approve-expired-session
  (testing "Cannot approve an expired session"
    (with-context context
      (let [user (find-user "john@doe.com")
            session (entities/put
                     {:cli-auth-session/status :pending
                      :cli-auth-session/expires-at
                      (t/minus (t/instant) (t/minutes 1))})
            user-code (:cli-auth-session/user-code session)
            response (-> (request :post "/api/cli/auth/approve"
                                  :content-type "application/json"
                                  :user user
                                  :body {:user-code user-code})
                         app
                         parse-body)]
        (is (= 400 (:status response)))
        (is (contains? (:parsed-body response) :error))))))

(deftest approve-invalid-user-code
  (testing "Cannot approve with invalid user code"
    (with-context context
      (let [user (find-user "john@doe.com")
            response (-> (request :post "/api/cli/auth/approve"
                                  :content-type "application/json"
                                  :user user
                                  :body {:user-code "INVALID1"})
                         app
                         parse-body)]
        (is (http-not-found? response))))))

(deftest approve-requires-authentication
  (testing "Approving a session requires authentication"
    (with-context context
      (let [session (cli-sessions/create-session)
            user-code (:cli-auth-session/user-code session)
            response (-> (request :post "/api/cli/auth/approve"
                                  :content-type "application/json"
                                  :body {:user-code user-code})
                         app)]
        (is (http-unauthorized? response))))))
