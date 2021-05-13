(ns clj-money.mailers-test
  (:require [clojure.test :refer [deftest is]]
            [dgknght.app-lib.test :refer [with-mail-capture]]
            [clj-money.mailers :as mailers]))

(def expected-text
  (slurp "resources/fixtures/mailers/user_invitation_expected.txt"))

(def expected-html
  (slurp "resources/fixtures/mailers/user_invitation_expected.html"))

(def expected-messages
  [{:to "jane@doe.com"
    :from "no-reply@clj-money.com"
    :subject "Invitation to clj-money"
    :body [:alternative
           {:type "text/plain"
            :content expected-text}
           {:type "text/html"
            :content expected-html}]}])

(deftest send-user-invitation-email
  (let [inviter {:first-name "John"
                 :last-name "Doe"
                 :email "john@doe.com"}
        invitee {:first-name "Jane"
                 :last-nmae "Doe"
                 :email "jane@doe.com"}]
    (with-mail-capture [mailbox]
      (mailers/invite-user {:from-user inviter
                            :to-user invitee
                            :url "http://clj-money.com/users/abcdef"})
      (is (= expected-messages @mailbox)
          "The correct messages are delivered"))))
