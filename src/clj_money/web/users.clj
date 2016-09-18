(ns clj-money.web.users
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [clojure.tools.logging :as log]
            )
  (:use [clj-money.web.shared]))

(defn new-user
  "Renders the sign up form"
  ([] (new-user {}))
  ([user]
   (layout
     "Sign up"
     "Sign up"
     [:div.row
      [:div.col-md-6
       [:form {:action "/users" :method :post}
        (text-input-field user :first-name {:autofocus true})
        (text-input-field user :last-name)
        (text-input-field user :email)
        (password-input-field user :password)]]])))
