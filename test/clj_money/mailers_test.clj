(ns clj-money.mailers-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [postal.core :refer [send-message]]
            [clj-money.mailers :as mailers]))

(def expected-body
  (slurp "resources/fixtures/mailers/user_invitation_expected.txt"))

(def expected-messages
  [{:to "jane@doe.com"
    :from "no-reply@clj-money.com"
    :subject "Invitation to clj-money"
    :body expected-body}])

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
