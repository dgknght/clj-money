(ns clj-money.web.users
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [clojure.tools.logging :as log])
  (:use [clj-money.web.shared]
        [clj-money.models.users :as users]))

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
        (password-input-field user :password)
        [:input.btn.btn-primary {:type :submit :value "Save"}]]]])))

(def create
  "Creates the user, redirects on success"
  [params]
  (let [validated (validate-user params)]
    (if (seq (:errors validate))
      (new-user validated)
      (do
        (users/create (env :db) validated)
        (redirect "/")))))
