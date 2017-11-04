(ns clj-money.mailers
  (:require [clojure.pprint :refer [pprint]]
            [postal.core :refer [send-message]]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [selmer.parser :refer [render]]
            [clj-money.models.users :as users]))

(defn invite-user
  [to-user from-user]
  (let [context {:recipient-first-name (:first-name to-user)
                 :sender-full-name (users/full-name from-user)
                 :app-name "clj-money"
                 :url "http://clj-money.com/users/abcdef"}]
    (send-message {:host (env :mailer-host)}
                  {:to (:email to-user)
                   :from (env :mailer-from)
                   :subject "Invitation to clj-money"
                   :body [:alternative
                          {:type "text/plain"
                           :content (-> "resources/templates/mailers/invite_user.txt"
                                        slurp ; will this work on heroku?
                                        (render context))}
                          {:type "text/html"
                           :content (-> "resources/templates/mailers/invite_user.html"
                                        slurp ; will this work on heroku?
                                        (render context))}]})))
