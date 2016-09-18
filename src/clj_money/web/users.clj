(ns clj-money.web.users
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [clojure.tools.logging :as log]
            [ring.util.response :refer :all]
            [environ.core :refer [env]])
  (:use [clj-money.web.shared]
        [clj-money.models.users :as users]))

(defn new-user
  "Renders the sign up form"
  ([] (new-user {}))
  ([user] (new-user user []))
  ([user alerts]
   (layout
     "Sign up"
     {:alerts alerts}
     [:div.row
      [:div.col-md-6
       [:form {:action "/users" :method :post}
        (text-input-field user :first_name {:autofocus true})
        (text-input-field user :last_name)
        (text-input-field user :email)
        (password-input-field user :password)
        [:input.btn.btn-primary {:type :submit :value "Save"}]]]])))

(defn create-user
  "Creates the user, redirects on success"
  [params]
  (let [user (select-keys params [:first_name
                                  :last_name
                                  :email
                                  :password])]
    (try
      (users/create (env :db) user)
      (redirect "/")
      (catch clojure.lang.ExceptionInfo e
        (log/debug "The user is not valid." (-> e ex-data :error))
        (new-user (append-schema-errors user (ex-data e))))
      (catch java.lang.Exception e
        (log/error "Unable to create the user." e)
        (new-user user [{:type :danger :message (.getMessage e)}])))))
