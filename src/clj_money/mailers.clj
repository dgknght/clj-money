(ns clj-money.mailers
  (:require [clojure.pprint :refer [pprint]]
            [postal.core :refer [send-message]]
            [environ.core :refer [env]]
            [clj-money.models.users :as users]))

(defn invite-user
  [to-user from-user]
  (send-message {:host (env :mailer-host)}
                {:to (:email to-user)
                 :from (env :mailer-from)
                 :subject "Invitation to clj-money"
                 :body (format "Hi %s!\n\n%s has invited you to use clj-money!\n"
                               (:first-name to-user)
                               (users/full-name from-user))}))
