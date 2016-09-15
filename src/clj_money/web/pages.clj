(ns clj-money.web.pages
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all])
  (:use [clj-money.web.shared :refer :all]))

(defn home
  "Renders the home page"
  []
  (layout "Home",
          "Money"
          (html
            "Welcome to the accounting application"
            )))

(def temp-model {:username "x"
                 :errors {:username ["This is a test error."]}})

(defn login
  "Renders the sign in form"
  []
  (layout
    "Log in"
    "Log in"
    [:div.row
     [:div.col-md-6
      [:form {:action "/authenticate" :method :post}
       (text-input-field temp-model :username)
       (password-input-field temp-model :password)
       [:input.btn.btn-primary {:type :submit :value "Log in"}]
       [:a.btn.btn-default {:href "/"} "Cancel"]]]]))
