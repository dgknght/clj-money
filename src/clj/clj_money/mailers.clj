(ns clj-money.mailers
  (:require [postal.core :refer [send-message]]
            [environ.core :refer [env]]
            [selmer.parser :refer [render]]
            [clj-money.models.users :as users]))

(defn- alt-body
  [parts context]
  (concat
    [:alternative]
    (map #(-> %
              (assoc :content (-> (:template-path %)
                                  slurp ; will this work on heroku?
                                  (render context)))
              (dissoc :template-path))
         parts)))

(def ^:private invite-user-parts
  [{:type "text/plain"
    :template-path "resources/templates/mailers/invite_user.txt"}
   {:type "text/html"
    :template-path "resources/templates/mailers/invite_user.html"}])

(defn- invite-user-context
  [to-user from-user url]
  {:recipient-first-name (:first-name to-user)
   :sender-full-name (users/full-name from-user)
   :app-name "clj-money"
   :url url})

(defn invite-user
  [{:keys [from-user to-user url]}]
  (send-message {:host (env :mailer-host)}
                {:to (:email to-user)
                 :from (env :mailer-from)
                 :subject (format "Invitation to %s" (env :application-name))
                 :body (alt-body invite-user-parts
                                 (invite-user-context to-user from-user url))}))
