(ns clj-money.ingestion.mail-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.java.io :as io]
            [integrant.core :as ig]
            [dgknght.app-lib.test-assertions]
            [clj-money.entities.ref]
            [clj-money.images.sql]
            [clj-money.db.ref]
            [clj-money.util :as util]
            [clj-money.io :refer [read-bytes]]
            [clj-money.crypto :as crypto]
            [clj-money.entities :as entities]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.mail :as mail]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-entity
                                            find-accounts
                                            find-mailbox]]))

(use-fixtures :each reset-db)

(def ^:private test-key "MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY=")

(def ^:private receipt
  {:date "2026-11-02"
   :location-name "Kroger"
   :total 6.48M
   :tax 0M
   :tax-rate 0M
   :payment-account "Checking"
   :line-items [{:description "Groceries"
                 :amount 6.48M
                 :account "Groceries"}]})

(def ^:private reader
  (reify
    ing/ReceiptReader
    (read-receipt [_ _source _entity _opts] receipt)
    ing/Closeable
    (close [_])))

(defn- email
  [filename]
  (read-bytes (io/file "fixtures/emails" filename)))

(defn- client
  "Returns a mail client that returns the messages and records the UIDs it
  marks read in the marked atom, and the passwords it's given in the
  passwords atom"
  [{:keys [messages uid-validity marked passwords]
    :or {uid-validity 99}}]
  (reify mail/MailClient
    (fetch-messages [_ _mailbox password]
      (when passwords (swap! passwords conj password))
      {:uid-validity uid-validity
       :messages messages})
    (mark-read [_ _mailbox _password uids]
      (swap! marked into uids))))

(def ^:private failing-client
  (reify mail/MailClient
    (fetch-messages [_ _mailbox _password]
      (throw (ex-info "Authentication failed" {})))
    (mark-read [_ _mailbox _password _uids])))

(def ^:private mailbox-context
  (conj basic-context
        #:mailbox{:entity "Personal"
                  :host "imap.example.com"
                  :port 993
                  :username "receipts@example.com"
                  :password (crypto/encrypt "secret" test-key)
                  :folder "Receipts"
                  :allowed-senders ["@kroger.example.com"]}))

(defn- configure-ingestion! []
  (let [entity (find-entity "Personal")
        [checking groceries] (find-accounts "Checking" "Groceries")]
    (entities/put (-> entity
                      (assoc-in [:entity/settings :settings/expense-accounts]
                                #{(util/->entity-ref groceries)})
                      (assoc-in [:entity/settings :settings/payment-methods]
                                #{(util/->entity-ref checking)})))))

(defn- ingestions []
  (entities/select #:receipt-ingestion{:entity (find-entity "Personal")}))

(deftest read-the-receipts-in-a-mailbox
  (with-context mailbox-context
    (configure-ingestion!)
    (let [marked (atom [])
          passwords (atom [])
          checked (mail/check-mailbox
                    (find-mailbox "Personal")
                    {:client (client {:messages [{:uid 5 :content (email "plain.eml")}
                                                 {:uid 6 :content (email "attachment.eml")}]
                                      :marked marked
                                      :passwords passwords})
                     :reader reader
                     :key test-key})
          [ingestion :as all] (ingestions)]
      (is (= ["secret"] @passwords)
          "The client is given the decrypted password")
      (is (= 1 (count all))
          "The message from a sender that isn't allowed isn't read")
      (is (comparable? #:receipt-ingestion{:status :complete
                                           :message-id "<plain-1@kroger.example.com>"}
                       ingestion)
          "The receipt is read and the message is recorded")
      (is (= :pending (:transaction/review-status
                        (entities/find (:receipt-ingestion/transaction ingestion))))
          "The transaction awaits review")
      (is (= "message/rfc822"
             (:image/content-type (entities/find (:receipt-ingestion/image ingestion))))
          "The email is saved")
      (is (= [5] @marked)
          "The message that was read is marked read, and the other is left alone")
      (is (comparable? #:mailbox{:uid-validity 99
                                 :last-uid 6}
                       (entities/find checked))
          "The last message seen is recorded")
      (is (nil? (:mailbox/error (entities/find checked)))
          "No error is recorded")
      (is (:mailbox/checked-at (entities/find checked))
          "The time of the check is recorded"))))

(deftest a-message-is-read-only-once
  (with-context mailbox-context
    (configure-ingestion!)
    (let [marked (atom [])
          opts {:client (client {:messages [{:uid 5 :content (email "plain.eml")}]
                                 :marked marked})
                :reader reader
                :key test-key}]
      (mail/check-mailbox (find-mailbox "Personal") opts)
      ; e.g., after the folder's UIDVALIDITY changed
      (mail/check-mailbox (find-mailbox "Personal") opts)
      (is (= 1 (count (ingestions)))
          "The message is read once")
      (is (= [5 5] @marked)
          "The message is marked read again"))))

(deftest a-failed-check-is-recorded
  (with-context mailbox-context
    (let [checked (mail/check-mailbox (find-mailbox "Personal")
                                      {:client failing-client
                                       :reader reader
                                       :key test-key})]
      (is (comparable? #:mailbox{:error "Authentication failed"}
                       (entities/find checked))
          "The error is recorded")
      (is (nil? (:mailbox/last-uid (entities/find checked)))
          "No message is recorded as seen"))))

(deftest a-password-that-cant-be-decrypted-is-reported
  (with-context mailbox-context
    (let [checked (mail/check-mailbox (find-mailbox "Personal")
                                      {:client (client {:messages []
                                                        :marked (atom [])})
                                       :reader reader
                                       :key "ZmVkY2JhOTg3NjU0MzIxMGZlZGNiYTk4NzY1NDMyMTA="})]
      (is (:mailbox/error (entities/find checked))
          "The error is recorded"))))

(deftest check-every-mailbox
  (with-context mailbox-context
    (configure-ingestion!)
    (mail/check-mailboxes {:client (client {:messages [{:uid 5 :content (email "plain.eml")}]
                                            :marked (atom [])})
                           :reader reader
                           :key test-key})
    (is (= 1 (count (ingestions)))
        "The receipt in the mailbox is read")))

(deftest the-poller-is-off-unless-enabled
  (is (nil? (ig/init-key ::ing/mail-poller {}))))

(deftest the-poller-checks-the-mailboxes-on-an-interval
  (let [executor (ig/init-key ::ing/mail-poller {:enabled? true
                                                 :interval-minutes 60})]
    (is (instance? java.util.concurrent.ScheduledExecutorService executor)
        "The checks are scheduled")
    (ig/halt-key! ::ing/mail-poller executor)
    (is (.isShutdown ^java.util.concurrent.ExecutorService executor)
        "The checks stop when the system is halted")))
