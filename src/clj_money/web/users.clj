(ns clj-money.web.users
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [clojure.tools.logging :as log]
            [ring.util.response :refer :all]
            [environ.core :refer [env]])
  (:use [clj-money.web.shared]
        [clj-money.models.users :as users]
        [clj-money.schema :as schema]))

(defn new-user
  "Renders the sign up form"
  ([] (new-user {}))
  ([user] (new-user user []))
  ([user options]
   (layout
     "Sign up"
     options
     [:div.row
      [:div.col-md-6
       [:form {:action "/users" :method :post}
        (text-input-field user :first-name {:autofocus true})
        (text-input-field user :last-name)
        (text-input-field user :email)
        (password-input-field user :password)
        [:input.btn.btn-primary {:type :submit :value "Save"}]]]])))

(defn create-user
  "Creates the user, redirects on success"
  [params]
  (let [user (select-keys params [:first-name
                                  :last-name
                                  :email
                                  :password])]
    (try
      (users/create (env :db) user)
      (redirect "/")
      (catch clojure.lang.ExceptionInfo e
        (new-user (schema/append-errors user (ex-data e))))
      (catch java.lang.Exception e
        (log/error "Unable to create the user." e)
        (new-user user {:alerts [{:type :danger :message (.getMessage e)}]})))))
