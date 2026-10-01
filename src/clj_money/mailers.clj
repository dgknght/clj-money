(ns clj-money.mailers
  (:require [clojure.tools.logging :as log]
            [postal.core :refer [send-message]]
            [selmer.parser :refer [render]]))

(defn- alt-body
  [parts context]
  (concat
   [:alternative]
   (map #(-> %
             (assoc :content (-> (:template-path %)
                                 slurp
                                 (render context)))
             (dissoc :template-path))
        parts)))

(def ^:private invite-user-parts
  [{:type "text/plain"
    :template-path "resources/templates/mailers/invite_user.txt"}
   {:type "text/html"
    :template-path "resources/templates/mailers/invite_user.html"}])

(defn- invitation-context
  [{:invitation/keys [token invited-by]} {:keys [site-url app-name]}]
  {:sender-full-name (format "%s %s"
                             (:user/first-name invited-by)
                             (:user/last-name invited-by))
   :app-name app-name
   :accept-url (str site-url "/accept-invitation/" token)
   :decline-url (str site-url "/decline-invitation/" token)})

(defn- deliver-message
  [message {:keys [enabled? host]}]
  (if enabled?
    (send-message {:host host} message)
    (log/infof "Mailer disabled. Would have sent: %s" (pr-str message))))

(defn send-invitation
  "Sends an invitation email. The config is the mailer configuration from
  clj-money.services: :enabled?, :host, :from, :app-name and :site-url."
  [invitation config]
  (deliver-message {:to (:invitation/recipient invitation)
                    :from (:from config)
                    :subject (format "Invitation to %s" (:app-name config))
                    :body (alt-body invite-user-parts
                                    (invitation-context invitation config))}
                   config))
