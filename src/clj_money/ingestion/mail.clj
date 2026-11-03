(ns clj-money.ingestion.mail
  "Checks the mailboxes the entities have set up (see
  clj-money.entities.mailboxes) for emailed receipts, and reads each one as
  if its email had been uploaded (see clj-money.ingestion.receipt-processing).

  The ::ing/mail-poller component checks every mailbox on an interval. It's
  off unless :mail-poller is configured with :enabled? true:

    :mail-poller {:enabled? true
                  :interval-minutes 5}

  The first check of a mailbox reads its unread messages. After that, it
  reads the messages that arrived since the last one it saw (by UID), read
  or not. A message from a sender the mailbox doesn't allow is left as it
  is. Each message it reads is marked read, and a message it has already
  read (by Message-ID) isn't read again."
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [integrant.core :as ig]
            [java-time.api :as t]
            [clj-money.util :as util]
            [clj-money.crypto :as crypto]
            [clj-money.db :as db]
            [clj-money.entities :as entities]
            [clj-money.entities.images :as img]
            [clj-money.images :as images]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.receipt-processing :as processing]
            [clj-money.mime :as mime]
            [clj-money.otel :as otel])
  (:import java.io.ByteArrayOutputStream
           java.util.Properties
           [java.util.concurrent Executors ScheduledExecutorService TimeUnit]
           [jakarta.mail Flags$Flag Folder Message Session Store UIDFolder]
           [jakarta.mail.search FlagTerm]
           jakarta.mail.Flags))

(defprotocol MailClient
  (fetch-messages [_ mailbox password]
    "Returns the folder's :uid-validity and its new :messages, each with its
    :uid and :content (the message, as bytes): those after the mailbox's
    :mailbox/last-uid or, on the first check or if the folder's
    UIDVALIDITY has changed, the unread ones")
  (mark-read [_ mailbox password uids]
    "Marks the messages with the given UIDs read"))

(defn- with-folder
  [{:mailbox/keys [host port username folder]} password mode f]
  (let [session (Session/getInstance
                  (doto (Properties.)
                    (.put "mail.imaps.connectiontimeout" "30000")
                    (.put "mail.imaps.timeout" "60000")))
        ^Store store (.getStore session "imaps")]
    (.connect store host (int port) username password)
    (try
      (let [^Folder f* (.getFolder store ^String folder)]
        (.open f* (int mode))
        (try
          (f f*)
          (finally
            (.close f* false))))
      (finally
        (.close store)))))

(defn- ->bytes
  [^Message message]
  (let [out (ByteArrayOutputStream.)]
    (.writeTo message out)
    (.toByteArray out)))

(defn- new-messages
  [^Folder folder {:mailbox/keys [last-uid uid-validity]}]
  (let [uid-folder ^UIDFolder folder]
    (if (and last-uid (= uid-validity (.getUIDValidity uid-folder)))
      ; a range ending in LASTUID always includes the last message, even
      ; one before the range starts, so it's filtered out
      (->> (.getMessagesByUID uid-folder (inc last-uid) UIDFolder/LASTUID)
           (filter #(< last-uid (.getUID uid-folder %))))
      (.search folder (FlagTerm. (Flags. Flags$Flag/SEEN) false)))))

(def imap-client
  "Reads the mailboxes with IMAP, over TLS (IMAPS)"
  (reify MailClient
    (fetch-messages [_ mailbox password]
      (with-folder mailbox password Folder/READ_ONLY
        (fn [^Folder folder]
          (let [uid-folder ^UIDFolder folder]
            {:uid-validity (.getUIDValidity uid-folder)
             :messages (mapv (fn [^Message message]
                               {:uid (.getUID uid-folder message)
                                :content (->bytes message)})
                             (new-messages folder mailbox))}))))
    (mark-read [_ mailbox password uids]
      (when (seq uids)
        (with-folder mailbox password Folder/READ_WRITE
          (fn [^Folder folder]
            (let [uid-folder ^UIDFolder folder]
              (doseq [^Message message (.getMessagesByUID uid-folder
                                                          (long-array uids))
                      :when message]
                (.setFlag message Flags$Flag/SEEN true)))))))))

(defn- allowed-sender?
  "Returns true if the mailbox allows messages from one of the senders: it
  allows any sender when it lists none, and an entry that starts with @
  allows the whole domain"
  [{:mailbox/keys [allowed-senders]} senders]
  (or (empty? allowed-senders)
      (some (fn [sender]
              (some (fn [allowed]
                      (let [allowed (str/lower-case (str/trim allowed))]
                        (if (str/starts-with? allowed "@")
                          (str/ends-with? sender allowed)
                          (= sender allowed))))
                    allowed-senders))
            senders)))

(defn- already-read?
  [{:mailbox/keys [entity]} message-id]
  (and message-id
       (entities/find-by #:receipt-ingestion{:entity entity
                                             :message-id message-id})))

(defn- ingest
  "Saves the email and reads the receipt in it"
  [{:mailbox/keys [entity]} content {:keys [subject message-id]} reader]
  (let [{:entity/keys [user]} (entities/find entity)
        image (img/find-or-create
                #:image{:user user
                        :original-filename (str (or (not-empty subject) "receipt")
                                                ".eml")
                        :content-type "message/rfc822"
                        :content content})]
    (processing/process
      (entities/put #:receipt-ingestion{:entity entity
                                        :image (util/->entity-ref image)
                                        :message-id message-id
                                        :status :pending})
      reader)))

(def ^:private messages-read
  (otel/histogram "clj_money.mailbox.messages"
                  :description "Messages found by each check of a mailbox, by what was done with them"
                  :unit "{message}"))

(defn- handle-message
  "Reads the receipt in the message, unless it's from a sender the mailbox
  doesn't allow (:skipped), or has already been read (:duplicate). Returns
  what was done with it."
  [mailbox {:keys [content]} reader]
  (let [{:keys [senders message-id] :as parsed} (mime/parse content)]
    (cond
      (not (allowed-sender? mailbox senders)) :skipped
      (already-read? mailbox message-id) :duplicate
      :else (do (ingest mailbox content parsed reader)
                :read))))

(defn check-mailbox
  "Reads the receipts in the new messages of the mailbox, marks them read,
  and records the check on the mailbox. A check that fails is recorded with
  the error. Returns the updated mailbox."
  [{:mailbox/keys [password] :as mailbox} {:keys [client reader key]}]
  (try
    (let [password (crypto/decrypt password key)
          {:keys [uid-validity messages]} (fetch-messages client mailbox password)
          results (mapv (fn [{:keys [uid] :as message}]
                          [uid (try
                                 (handle-message mailbox message reader)
                                 (catch Exception e
                                   (log/errorf e "[mail] unable to read message %s from mailbox %s"
                                               uid
                                               (:id mailbox))
                                   :failed))])
                        messages)]
      (mark-read client
                 mailbox
                 password
                 (keep (fn [[uid result]]
                         (when (#{:read :duplicate} result) uid))
                       results))
      (doseq [[result n] (frequencies (map second results))]
        (otel/record messages-read n {"result" result}))
      (entities/put (assoc mailbox
                           :mailbox/uid-validity uid-validity
                           :mailbox/last-uid (reduce max
                                                     (or (:mailbox/last-uid mailbox) 0)
                                                     (map first results))
                           :mailbox/checked-at (t/instant)
                           :mailbox/error nil)))
    (catch Exception e
      (log/errorf e "[mail] unable to check mailbox %s" (:id mailbox))
      (entities/put (assoc mailbox
                           :mailbox/checked-at (t/instant)
                           :mailbox/error (or (ex-message e)
                                              (.getName (class e))))))))

(defn check-mailboxes
  "Checks every mailbox for receipts (see check-mailbox)"
  [opts]
  (doseq [mailbox (entities/select #:mailbox{:host [:!= nil]})]
    (check-mailbox mailbox opts)))

(defn- check-in-system
  [{:keys [storage publisher image-storage] :as opts}]
  (binding [db/*storage* storage
            entities/*publisher* publisher
            images/*storage* image-storage]
    (try
      (check-mailboxes opts)
      (catch Throwable e
        ; an exception would stop the scheduled checks
        (log/error e "[mail] unable to check the mailboxes")))))

(defmethod ig/init-key ::ing/mail-poller
  [_ {:keys [enabled? interval-minutes services]
       :or {interval-minutes 5}
       :as config}]
  (when enabled?
    (log/infof "[mail] checking mailboxes every %s minutes" interval-minutes)
    (doto (Executors/newSingleThreadScheduledExecutor)
      (.scheduleWithFixedDelay ^Runnable #(check-in-system
                                            (assoc config
                                                   :client imap-client
                                                   :key (:mailbox-key services)))
                               1
                               (long (* 60 interval-minutes))
                               TimeUnit/SECONDS))))

(defmethod ig/halt-key! ::ing/mail-poller
  [_ executor]
  (when executor
    (.shutdown ^ScheduledExecutorService executor)
    (.awaitTermination ^ScheduledExecutorService executor 30 TimeUnit/SECONDS)))
