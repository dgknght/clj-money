(ns clj-money.api.mailboxes
  (:refer-clojure :exclude [update])
  (:require [clj-money.api :as api :refer [add-error-handler]]
            [clj-money.state :refer [current-entity]]))

(def ^:private editable-keys
  [:mailbox/host
   :mailbox/port
   :mailbox/username
   :mailbox/password
   :mailbox/folder
   :mailbox/allowed-senders])

(defn select
  "Retrieves the mailbox of the current entity, if it has one, in a list"
  [& {:as opts}]
  (api/get (api/path :entities
                     (:id @current-entity)
                     :mailboxes)
           {}
           (add-error-handler opts "Unable to retrieve the mailbox: %s")))

(defn- create
  [mailbox opts]
  (api/post (api/path :entities
                      (:id @current-entity)
                      :mailboxes)
            mailbox
            (add-error-handler opts "Unable to save the mailbox: %s")))

(defn- update
  [{:keys [id] :as mailbox} opts]
  (api/patch (api/path :mailboxes id)
             (select-keys mailbox editable-keys)
             (add-error-handler opts "Unable to save the mailbox: %s")))

(defn save
  "Saves the mailbox. A blank password keeps the one already saved."
  [mailbox & {:as opts}]
  (if (:id mailbox)
    (update mailbox opts)
    (create (select-keys mailbox editable-keys) opts)))

(defn delete
  [{:keys [id]} & {:as opts}]
  (api/delete (api/path :mailboxes id)
              (add-error-handler opts "Unable to remove the mailbox: %s")))
