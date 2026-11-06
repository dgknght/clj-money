(ns clj-money.api.cli-logins-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [java-time.api :as t]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [clj-money.entities :as entities]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.api.test-helper :refer [parse-body
                                               request]]
            [clj-money.test-context :refer [with-context
                                            find-user]]
            [clj-money.web.test-handler :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private user-ctx
  [#:user{:email "john@doe.com"
          :first-name "John"
          :last-name "Doe"
          :password "please01"}])

(defn- find-login
  [user-code]
  (entities/find-by {:cli-login/user-code user-code}))

(defn- json-post
  [path body]
  (-> (request :post path
               :content-type "application/json"
               :body body)
      app
      parse-body))

(defn- start-login []
  (json-post (path :api :cli-logins) {}))

(defn- poll
  [device-code]
  (json-post (path :api :cli-logins :token) {:deviceCode device-code}))

(defn- approve
  [user-code user]
  (-> (request :post (path :api :cli-logins user-code :approve)
               :user user)
      app
      parse-body))

(deftest the-cli-starts-a-login
  (let [{:keys [parsed-body] :as res} (start-login)
        {:keys [deviceCode userCode]} parsed-body]
    (is (http-created? res)
        "The login is created without authentication")
    (is (string? deviceCode)
        "The response includes the device code the CLI polls with")
    (is (re-matches #"[A-Z]{4}-[A-Z]{4}" userCode)
        "The response includes a code for the user to compare in the browser")
    (is (= "https://www.mymoney.com/cli/login"
           (:verificationUri parsed-body))
        "The response includes the page where the user approves the login")
    (is (= (str "https://www.mymoney.com/cli/login?code=" userCode)
           (:verificationUriComplete parsed-body))
        "The response includes the approval page with the code filled in")
    (is (= 600 (:expiresIn parsed-body))
        "The response says how many seconds the codes are good for")
    (is (= 5 (:interval parsed-body))
        "The response says how many seconds to wait between polls")
    (is (find-login userCode)
        "The login is saved")))

(deftest polling-before-approval-reports-pending
  (let [{{:keys [deviceCode]} :parsed-body} (start-login)
        res (poll deviceCode)]
    (is (http-bad-request? res))
    (is (= {:error "authorization_pending"} (:parsed-body res)))))

(def ^:private pending-ctx
  (conj user-ctx
        #:cli-login{:device-code "the-device-code-0123456789abcdef"
                    :user-code "BCDF-GHJK"
                    :expires-at (t/plus (t/instant) (t/minutes 10))}))

(deftest a-user-approves-a-login
  (with-context pending-ctx
    (let [user (find-user "john@doe.com")]
      (is (http-no-content? (approve "BCDF-GHJK" user)))
      (is (= (:id user)
             (get-in (find-login "BCDF-GHJK") [:cli-login/user :id]))
          "The login is linked to the approving user"))))

(deftest approving-requires-authentication
  (with-context pending-ctx
    (is (http-unauthorized? (approve "BCDF-GHJK" nil)))
    (is (nil? (:cli-login/user (find-login "BCDF-GHJK"))))))

(deftest an-unknown-code-cannot-be-approved
  (with-context pending-ctx
    (is (http-not-found? (approve "ZZZZ-ZZZZ" (find-user "john@doe.com"))))))

(def ^:private approved-ctx
  (conj user-ctx
        #:cli-login{:device-code "the-device-code-0123456789abcdef"
                    :user-code "BCDF-GHJK"
                    :user "john@doe.com"
                    :expires-at (t/plus (t/instant) (t/minutes 10))}))

(deftest polling-after-approval-returns-a-token
  (with-context approved-ctx
    (let [{:keys [parsed-body] :as res} (poll "the-device-code-0123456789abcdef")]
      (is (http-success? res))
      (is (string? (:authToken parsed-body))
          "The response includes the auth token")
      (testing "the token authenticates API requests"
        (let [me (-> (request :get (path :api :users :me)
                              :content-type "application/json")
                     (assoc-in [:headers "authorization"]
                               (str "Bearer " (:authToken parsed-body)))
                     app
                     parse-body)]
          (is (http-success? me))
          (is (= "john@doe.com" (get-in me [:parsed-body :email])))))
      (is (= {:error "invalid_grant"}
             (:parsed-body (poll "the-device-code-0123456789abcdef")))
          "The token is issued only once"))))

(def ^:private expired-ctx
  (conj user-ctx
        #:cli-login{:device-code "the-device-code-0123456789abcdef"
                    :user-code "BCDF-GHJK"
                    :expires-at (t/minus (t/instant) (t/seconds 1))}))

(deftest polling-an-expired-login-is-rejected
  (with-context expired-ctx
    (let [res (poll "the-device-code-0123456789abcdef")]
      (is (http-bad-request? res))
      (is (= {:error "expired_token"} (:parsed-body res))))))

(deftest the-device-code-can-be-sent-as-edn
  (with-context approved-ctx
    (let [res (-> (request :post (path :api :cli-logins :token)
                           :body {:device-code "the-device-code-0123456789abcdef"})
                  app
                  parse-body)]
      (is (http-success? res))
      (is (string? (get-in res [:parsed-body :auth-token]))))))
