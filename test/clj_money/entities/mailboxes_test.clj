(ns clj-money.entities.mailboxes-test
  (:require [clojure.test :refer [is]]
            [java-time.api :as t]
            [dgknght.app-lib.test]
            [dgknght.app-lib.test-assertions]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-entity
                                            find-mailbox]]
            [clj-money.test-helpers :refer [dbtest]]))

(def ^:private attributes
  #:mailbox{:host "imap.example.com"
            :port 993
            :username "receipts@example.com"
            :password "encrypted"
            :folder "Receipts"
            :allowed-senders ["store@example.com" "@example.net"]})

(dbtest create-a-mailbox
  (with-context
    (let [entity (find-entity "Personal")
          created (entities/put (assoc attributes
                                       :mailbox/entity (util/->entity-ref entity)))]
      (is (comparable? (assoc attributes
                              :mailbox/entity (util/->entity-ref entity))
                       (entities/find created))
          "The mailbox can be retrieved, with its list of senders"))))

(def ^:private mailbox-context
  (conj basic-context
        (assoc attributes :mailbox/entity "Personal")))

(dbtest record-a-check-of-a-mailbox
  (with-context mailbox-context
    (let [checked-at (t/truncate-to (t/instant) :millis)
          updated (entities/put (assoc (find-mailbox "Personal")
                                       :mailbox/uid-validity 1234567890
                                       :mailbox/last-uid 42
                                       :mailbox/checked-at checked-at
                                       :mailbox/error "Authentication failed"))]
      (is (comparable? #:mailbox{:uid-validity 1234567890
                                 :last-uid 42
                                 :checked-at checked-at
                                 :error "Authentication failed"}
                       (entities/find updated))
          "The state of the last check is kept"))))

(dbtest a-mailbox-requires-a-host
  (with-context
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Validation failed"
                          (entities/put (-> attributes
                                            (assoc :mailbox/entity (util/->entity-ref
                                                                     (find-entity "Personal")))
                                            (dissoc :mailbox/host)))))))
