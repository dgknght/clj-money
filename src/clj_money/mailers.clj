(ns clj-money.mailers
  (:require [clojure.pprint :refer [pprint]]
            [postal.core :refer [send-message]]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [clj-money.models.users :as users]))

(defn invite-user
  [to-user from-user]
  (send-message {:host (env :mailer-host)}
                {:to (:email to-user)
                 :from (env :mailer-from)
                 :subject "Invitation to clj-money"
                 :body [:alternative
                        {:type "text/plain"
                         :content (format "Hi %s!\n\n%s has invited you to use clj-money! Click the link below to get started.\n\nhttp://clj-money.com/users/abcdef"
                                          (:first-name to-user)
                                          (users/full-name from-user))}
                        {:type "text/html"
                         :content (html
                                    [:html
                                     [:body
                                      [:p "Hi Jane!"]
                                      [:p
                                       "John Doe has invited you to use clj-money!"
                                       [:a {:href "http://clj-money.com/users/abcdef"}
                                        "Click here"]
                                       "to get started."]]])}]}))
