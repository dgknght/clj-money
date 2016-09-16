(ns clj-money.web.pages
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [cemerick.friend.credentials :as creds])
  (:use [clj-money.web.shared :refer :all]))

(defn home
  "Renders the home page"
  []
  (layout "Home",
          "Money"
          (html
            "Welcome to the accounting application"
            )))

(defn login
  "Renders the sign in form"
  ([] (login {:username nil :password nil}))
  ([model]
   (layout
     "Log in"
     "Log in"
     [:div.row
      [:div.col-md-6
       [:form {:action "/authenticate" :method :post}
        (text-input-field model :username {:autofocus true})
        (password-input-field model :password)
        [:input.btn.btn-primary {:type :submit :value "Log in"}]
        [:a.btn.btn-default {:href "/"} "Cancel"]]]])))


; TODO Replace this with a database implementation
(def users
  {"doug" {:username "doug"
           :password (creds/hash-bcrypt "please01")
           :roles #{::admin}}})

(defn authenticate
  "Verifies the supplied credentials"
  [username password]
  (if-let [user (get users username)]
    (if (= (creds/hash-bcrypt password)
           (:password user))
      (redirect "/accounts") ; TODO Need the originally request url
      (login {:username username :password password}))
    (login {:username username :password password})))
