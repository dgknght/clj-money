(ns clj-money.mailers
  (:require [postal.core :refer [send-message]]
            [clj-money.config :refer [env]]
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
  [{:invitation/keys [token user]}]
  (let [base-url (str (env :site-protocol) "://" (env :site-host))]
    {:sender-full-name (format "%s %s"
                               (:user/first-name user)
                               (:user/last-name user))
     :app-name (env :application-name)
     :accept-url (str base-url "/accept-invitation/" token)
     :decline-url (str base-url "/decline-invitation/" token)}))

(defn send-invitation
  [invitation]
  (send-message {:host (env :mailer-host)}
                {:to (:invitation/recipient invitation)
                 :from (env :mailer-from)
                 :subject (format "Invitation to %s" (env :application-name))
                 :body (alt-body invite-user-parts
                                 (invitation-context invitation))}))
