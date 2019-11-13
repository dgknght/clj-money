(ns clj-money.web.pages
  (:require [clj-money.web.shared :refer [with-layout
                                          form
                                          text-input-field
                                          password-input-field]]))

(defn home
  "Renders the home page"
  [_]
  (with-layout "Home" {:suppress-page-title? true}
    [:div.jumbotron
     [:h1 "Welcome!"]
     [:p
      "Welcome to the accounting application. This is an implementation
      of the classic double-entry accounting system."]]))

(defn login
  "Renders the sign in form"
  ([] (login {:params {:username nil :password nil}}))
  ([{model :params}]
   (with-layout "Log in" {}
     [:div.row
      [:div.col-md-6
       (form "/login" {}
             (text-input-field model :username {:autofocus true})
             (password-input-field model :password)
             [:input.btn.btn-primary {:type :submit :value "Log in"}]
             "&nbsp;"
             [:a.btn.btn-default {:href "/"} "Cancel"])]])))
