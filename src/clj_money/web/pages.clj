(ns clj-money.web.pages
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all])
  (:use [clj-money.web.shared :refer :all]))

(defn home
  "Renders the home page"
  [_]
  (with-layout "Home" {}
    "Welcome to the accounting application"))

(defn login
  "Renders the sign in form"
  ([] (login {:params {:username nil :password nil}}))
  ([{model :params}]
   (with-layout "Log in" {}
     [:div.row
      [:div.col-md-6
       [:form {:action "/login" :method :post}
        (text-input-field model :username {:autofocus true})
        (password-input-field model :password)
        [:input.btn.btn-primary {:type :submit :value "Log in"}]
        "&nbsp;"
        [:a.btn.btn-default {:href "/"} "Cancel"]]]])))
