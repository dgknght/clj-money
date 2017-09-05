(ns clj-money.web.users
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [clojure.tools.logging :as log]
            [ring.util.response :refer :all]
            [environ.core :refer [env]])
  (:use [clj-money.web.shared]
        [clj-money.models.users :as users]
        [clj-money.validation :as validation]))

(defn new-user
  "Renders the sign up form"
  ([] (new-user {}))
  ([req] (new-user req{}))
  ([{user :params} options]
   (with-layout "Sign up" options
     [:div.row
      [:div.col-md-6
       (form "/users"
             (text-input-field user :first-name {:autofocus true})
             (text-input-field user :last-name)
             (text-input-field user :email)
             (password-input-field user :password)
             [:input.btn.btn-primary {:type :submit :value "Save"}])]])))

(defn create-user
  "Creates the user, redirects on success"
  [{params :params}]
  (let [created (users/create (env :db) (select-keys params [:first-name
                                                             :last-name
                                                             :email
                                                             :password]))]
    (if (validation/has-error? created)
      (new-user created)
      (redirect "/login"))))
