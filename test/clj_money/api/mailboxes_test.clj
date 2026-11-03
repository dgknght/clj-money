(ns clj-money.api.mailboxes-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.crypto :as crypto]
            [clj-money.entities :as entities]
            [clj-money.api.test-helper :refer [parse-body
                                               request]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user
                                            find-entity
                                            find-mailbox]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.web.test-handler :refer [app]]))

(use-fixtures :each reset-db)

; the key in env/test/config.edn
(def ^:private test-key "MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY=")

(def ^:private attributes
  #:mailbox{:host "imap.example.com"
            :port 993
            :username "receipts@example.com"
            :password "secret"
            :folder "Receipts"
            :allowed-senders ["@kroger.example.com" " "]})

(defn- stored-mailbox
  "Returns the mailbox of the Personal entity, as it's stored"
  []
  (entities/find-by #:mailbox{:entity (find-entity "Personal")}))

(defn- create-mailbox
  [email]
  (-> (request :post (path :api
                           :entities
                           (:id (find-entity "Personal"))
                           :mailboxes)
               :body attributes
               :user (find-user email))
      app
      parse-body))

(deftest a-user-can-create-a-mailbox-in-his-entity
  (with-context
    (let [{:as response :keys [parsed-body]} (create-mailbox "john@doe.com")
          retrieved (stored-mailbox)]
      (is (http-created? response))
      (is (comparable? #:mailbox{:host "imap.example.com"
                                 :port 993
                                 :username "receipts@example.com"
                                 :folder "Receipts"
                                 :allowed-senders ["@kroger.example.com"]}
                       parsed-body)
          "The response is the new mailbox, without blank senders")
      (is (not (contains? parsed-body :mailbox/password))
          "The password isn't returned")
      (is (not= "secret" (:mailbox/password retrieved))
          "The password isn't stored in plain text")
      (is (= "secret" (crypto/decrypt (:mailbox/password retrieved) test-key))
          "The password is stored encrypted"))))

(deftest a-user-cannot-create-a-mailbox-in-anothers-entity
  (with-context
    (is (http-not-found? (create-mailbox "jane@doe.com")))
    (is (nil? (stored-mailbox))
        "The mailbox isn't created")))

(def ^:private mailbox-context
  (conj basic-context
        (assoc attributes
               :mailbox/entity "Personal"
               :mailbox/password (crypto/encrypt "secret" test-key)
               :mailbox/allowed-senders ["@kroger.example.com"]
               :mailbox/uid-validity 99
               :mailbox/last-uid 42
               :mailbox/error "Authentication failed")))

(defn- get-mailboxes
  [email]
  (-> (request :get (path :api
                          :entities
                          (:id (find-entity "Personal"))
                          :mailboxes)
               :user (find-user email))
      app
      parse-body))

(deftest a-user-can-get-the-mailbox-of-his-entity
  (with-context mailbox-context
    (let [{:as response :keys [parsed-body]} (get-mailboxes "john@doe.com")]
      (is (http-success? response))
      (is (seq-of-maps-like? [#:mailbox{:host "imap.example.com"
                                        :error "Authentication failed"}]
                             parsed-body)
          "The mailbox is returned, with the error of its last check")
      (is (not-any? :mailbox/password parsed-body)
          "The password isn't returned"))))

(deftest a-user-cannot-get-the-mailbox-of-anothers-entity
  (with-context mailbox-context
    (let [{:as response :keys [parsed-body]} (get-mailboxes "jane@doe.com")]
      (is (http-success? response))
      (is (empty? parsed-body) "No mailbox is returned"))))

(defn- update-mailbox
  [email changes]
  (-> (request :patch (path :api
                            :mailboxes
                            (:id (find-mailbox "Personal")))
               :body changes
               :user (find-user email))
      app
      parse-body))

(deftest a-user-can-update-the-mailbox-of-his-entity
  (with-context mailbox-context
    (let [response (update-mailbox "john@doe.com" #:mailbox{:allowed-senders ["receipts@store.example.com"]
                                                            :password ""})
          retrieved (stored-mailbox)]
      (is (http-success? response))
      (is (comparable? #:mailbox{:allowed-senders ["receipts@store.example.com"]
                                 :last-uid 42}
                       retrieved)
          "The changes are saved, and the messages seen are kept")
      (is (= "secret" (crypto/decrypt (:mailbox/password retrieved) test-key))
          "A blank password keeps the one saved")
      (is (nil? (:mailbox/error retrieved))
          "The error is cleared, to be checked again"))))

(deftest changing-the-folder-of-a-mailbox-forgets-the-messages-seen
  (with-context mailbox-context
    (update-mailbox "john@doe.com" #:mailbox{:folder "Other"
                                             :password "changed"})
    (let [retrieved (stored-mailbox)]
      (is (nil? (:mailbox/last-uid retrieved))
          "The last message seen is forgotten")
      (is (= "changed" (crypto/decrypt (:mailbox/password retrieved) test-key))
          "The new password is saved"))))

(deftest a-user-cannot-update-the-mailbox-of-anothers-entity
  (with-context mailbox-context
    (is (http-not-found? (update-mailbox "jane@doe.com" #:mailbox{:host "evil.example.com"})))
    (is (= "imap.example.com" (:mailbox/host (stored-mailbox)))
        "The mailbox isn't changed")))

(defn- delete-mailbox
  [email]
  (-> (request :delete (path :api
                             :mailboxes
                             (:id (find-mailbox "Personal")))
               :user (find-user email))
      app))

(deftest a-user-can-delete-the-mailbox-of-his-entity
  (with-context mailbox-context
    (let [mailbox (find-mailbox "Personal")]
      (is (http-success? (delete-mailbox "john@doe.com")))
      (is (nil? (entities/find mailbox))
          "The mailbox is deleted"))))

(deftest a-user-cannot-delete-the-mailbox-of-anothers-entity
  (with-context mailbox-context
    (let [mailbox (find-mailbox "Personal")]
      (is (http-not-found? (delete-mailbox "jane@doe.com")))
      (is (entities/find mailbox)
          "The mailbox isn't deleted"))))

(deftest a-mailbox-requires-a-password
  (with-context
    (let [response (-> (request :post (path :api
                                            :entities
                                            (:id (find-entity "Personal"))
                                            :mailboxes)
                                :body (dissoc attributes :mailbox/password)
                                :user (find-user "john@doe.com"))
                       app
                       parse-body)]
      (is (not (<= 200 (:status response) 299))
          "The mailbox isn't created")
      (is (nil? (stored-mailbox))))))

