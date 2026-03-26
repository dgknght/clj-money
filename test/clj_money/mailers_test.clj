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
  (let [invitation #:invitation{:recipient "jane@doe.com"
                                 :status :unsent
                                 :token "abc123"
                                 :user #:user{:first-name "John"
                                              :last-name "Doe"
                                              :email "john@doe.com"}}]
    (with-mail-capture [mailbox]
      (mailers/send-invitation invitation)
      (is (= expected-messages @mailbox)
          "The correct messages are delivered"))))
