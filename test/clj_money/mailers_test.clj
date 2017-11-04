(ns clj-money.mailers-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clojure.string :refer [trim]]
            [postal.core :refer [send-message]]
            [clj-money.mailers :as mailers]))

(def expected-text
  (slurp "resources/fixtures/mailers/user_invitation_expected.txt"))

(def expected-html
  (-> "resources/fixtures/mailers/user_invitation_expected.html"
      slurp
      trim))

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
  (let [messages (atom [])
        inviter {:first-name "John"
                 :last-name "Doe"
                 :email "john@doe.com"}
        invitee {:first-name "Jane"
                 :last-nmae "Doe"
                 :email "jane@doe.com"}]
    (with-redefs [send-message (fn [host message]
                                 (swap! messages conj message))]
     (mailers/invite-user invitee inviter))

    (if-not (= expected-messages @messages)
      (pprint {:expected expected-messages
               :actual @messages
               :diff (diff expected-messages @messages)}))

    (is (= expected-messages
           @messages)
        "The correct messages are delivered")))
