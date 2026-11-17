(ns clj-money.mailers-test
  (:require [clojure.test :refer [deftest is]]
            [postal.core :as postal]
            [dgknght.app-lib.test :refer [with-mail-capture]]
            [clj-money.otel-helpers :refer [capture]]
            [clj-money.mailers :as mailers]))

(def expected-text
  (slurp "fixtures/mailers/user_invitation_expected.txt"))

(def expected-html
  (slurp "fixtures/mailers/user_invitation_expected.html"))

(def invitation-message
  {:to "jane@doe.com"
   :from "no-reply@clj-money.com"
   :subject "Invitation to clj-money"
   :body [:alternative
          {:type "text/plain"
           :content expected-text}
          {:type "text/html"
           :content expected-html}]})

(def ^:private invitation
  #:invitation{:recipient "jane@doe.com"
               :status :unsent
               :token "abc123"
               :invited-by #:user{:first-name "John"
                                  :last-name "Doe"
                                  :email "john@doe.com"}})

(def ^:private mailer-config
  {:enabled? true
   :host "testmailer.com"
   :from "no-reply@clj-money.com"
   :app-name "clj-money"
   :site-url "https://www.mymoney.com"})

(deftest send-user-invitation-email
  (with-mail-capture [mailbox]
    (mailers/send-invitation invitation mailer-config)
    (let [[m :as ms] @mailbox]
      (is (= 1 (count ms))
          "One message is delivered")
      (is (= invitation-message m)
          "The invitation message is delivered"))))

(defn- send-with-result
  [result & {:as config}]
  (with-redefs [postal/send-message (fn [_server _message]
                                      (if (instance? Exception result)
                                        (throw result)
                                        result))]
    (capture #(mailers/send-invitation invitation
                                       (merge mailer-config config)))))

(defn- duration-points
  [metrics]
  (get-in metrics ["email.send.duration" :points]))

(deftest trace-a-successful-delivery
  (let [{:keys [result spans metrics]}
        (send-with-result {:code 0 :error :SUCCESS :message "messages sent"})
        [span :as ss] spans
        [point :as ps] (duration-points metrics)]
    (is (= :SUCCESS (:error result))
        "The result of the delivery is returned")
    (is (= 1 (count ss))
        "One span is recorded")
    (is (= {:name "email/send"
            :status :unset
            :attributes {"email.mailer" "invitation"
                         "email.enabled" true
                         "email.delivery.status" "SUCCESS"}}
           (select-keys span [:name :status :attributes]))
        "The span identifies the email and its delivery status")
    (is (= 1 (count ps))
        "One duration is recorded")
    (is (= {:count 1
            :attributes {"email.mailer" "invitation"}}
           (select-keys point [:count :attributes]))
        "The duration is recorded for the mailer")))

(deftest trace-a-rejected-delivery
  (let [{:keys [spans metrics]}
        (send-with-result {:code 550 :error :SMTPAddressFailedException :message "no such user"})
        [span] spans
        [point] (duration-points metrics)]
    (is (= {:status :error
            :status-description "no such user"
            :delivery-status "SMTPAddressFailedException"}
           {:status (:status span)
            :status-description (:status-description span)
            :delivery-status (get-in span [:attributes "email.delivery.status"])})
        "The span reports the failure")
    (is (= {"email.mailer" "invitation"
            "error.type" "SMTPAddressFailedException"}
           (:attributes point))
        "The duration is recorded with the error type")))

(deftest trace-a-delivery-that-throws
  (let [{:keys [exception spans metrics]}
        (send-with-result (java.net.ConnectException. "Connection refused"))
        [span] spans
        [point] (duration-points metrics)]
    (is (instance? java.net.ConnectException exception)
        "The exception is rethrown")
    (is (= [:error "Connection refused" ["exception"]]
           [(:status span)
            (:status-description span)
            (mapv :name (:events span))])
        "The span records the exception")
    (is (= {"email.mailer" "invitation"
            "error.type" "java.net.ConnectException"}
           (:attributes point))
        "The duration is recorded with the error type")))

(deftest trace-a-disabled-mailer
  (let [{:keys [spans metrics]} (send-with-result nil :enabled? false)
        [span :as ss] spans]
    (is (= 1 (count ss))
        "One span is recorded")
    (is (= {"email.mailer" "invitation"
            "email.enabled" false}
           (:attributes span))
        "The span shows that the mailer is disabled")
    (is (empty? (duration-points metrics))
        "No duration is recorded")))
