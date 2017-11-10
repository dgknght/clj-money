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
       (form "/users" {}
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

(defn new-password
  ([req]
   (new-password req {}))
  ([{{:keys [token password password-confirmation]} :params} options]
   (with-layout "Set your password" options
     [:div.row
      [:div.col-md-6
       (let [user {:password password
                   :password-confirmation password-confirmation}]
         (form (format "/users/%s/password" token) {}
               (password-input-field user :password {:autofocus true})
               (password-input-field user :password-confirmation)
               [:input.btn.btn-primary {:type :submit :value "Save"}]))]])))

(defn set-password
  [{{:keys [token password password-confirmation]} :params :as req}]
  (if (= password password-confirmation)
    (do
      (users/reset-password (env :db) token password)
      (redirect "/login"))
    (new-password req {:alerts [{:type :danger :message "The passwords must match"}]})))
