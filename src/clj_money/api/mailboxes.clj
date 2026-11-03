(ns clj-money.api.mailboxes
  "Manages the mailbox an entity's receipts are emailed to (see
  clj-money.ingestion.mail). The password is given in plain text, stored
  encrypted, and never returned."
  (:refer-clojure :exclude [update])
  (:require [clojure.string :as str]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.api :as api]
            [clj-money.authorization
             :as auth
             :refer [+scope
                     authorize]]
            [clj-money.crypto :as crypto]
            [clj-money.entities :as entities]
            [clj-money.web.system :as system]
            [clj-money.authorization.mailboxes]))

(def ^:private conceal
  #(dissoc % :mailbox/password))

(defn- ->response
  [mailbox]
  (if (map? mailbox)
    (conceal mailbox)
    mailbox))

(defn- index
  [{:keys [authenticated] {:keys [entity-id]} :params}]
  (api/response
    (mapv conceal
          (entities/select (+scope #:mailbox{:entity {:id entity-id}}
                                   :mailbox
                                   authenticated)))))

(defn- encrypt-password
  "Encrypts the password given, or leaves it out when none is given, so that
  an update keeps the one already saved"
  [mailbox req]
  (let [password (:mailbox/password mailbox)]
    (if (str/blank? password)
      (dissoc mailbox :mailbox/password)
      (assoc mailbox
             :mailbox/password
             (crypto/encrypt password
                             (:mailbox-key (system/component req :services)))))))

(defn- extract-mailbox
  [{:keys [params] :as req}]
  (-> params
      (select-keys [:mailbox/host
                    :mailbox/port
                    :mailbox/username
                    :mailbox/password
                    :mailbox/folder
                    :mailbox/allowed-senders])
      (update-in-if [:mailbox/port] #(if (string? %) (parse-long %) %))
      (update-in-if [:mailbox/allowed-senders]
                    (fn [senders]
                      (->> senders
                           (map str/trim)
                           (remove str/blank?)
                           vec)))
      (encrypt-password req)))

(defn- create
  [{:keys [authenticated] {:keys [entity-id]} :params :as req}]
  (-> req
      extract-mailbox
      (assoc :mailbox/entity {:id entity-id})
      (update-in [:mailbox/folder] #(or (not-empty %) "INBOX"))
      (authorize ::auth/create authenticated)
      entities/put
      ->response
      api/creation-response))

(defn- find-and-auth
  [{:keys [params authenticated]} action]
  (some-> params
          (select-keys [:id])
          (+scope :mailbox authenticated ::auth/manage)
          entities/find-by
          (authorize action authenticated)))

(def ^:private location-keys
  [:mailbox/host :mailbox/username :mailbox/folder])

(defn- merge-update
  "Applies the update to the mailbox. When it points somewhere else, the
  messages already seen are forgotten, so the next check reads the new
  folder's unread messages."
  [mailbox changes]
  (let [updated (merge mailbox changes)]
    (if (= (select-keys mailbox location-keys)
           (select-keys updated location-keys))
      updated
      (assoc updated
             :mailbox/uid-validity nil
             :mailbox/last-uid nil))))

(defn- update
  [req]
  (if-let [mailbox (find-and-auth req ::auth/update)]
    (-> mailbox
        (merge-update (extract-mailbox req))
        (assoc :mailbox/error nil)
        entities/put
        ->response
        api/update-response)
    api/not-found))

(defn- delete
  [req]
  (if-let [mailbox (find-and-auth req ::auth/destroy)]
    (do
      (entities/delete mailbox)
      (api/response))
    api/not-found))

(def routes
  [["entities/:entity-id/mailboxes" {:get {:handler index}
                                     :post {:handler create}}]
   ["mailboxes/:id" {:patch {:handler update}
                     :delete {:handler delete}}]])
