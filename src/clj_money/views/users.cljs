(ns clj-money.views.users
  (:require [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [dgknght.app-lib.dom :refer [set-focus]]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.forms-validation :as v]
            [clj-money.icons :refer [icon-with-text]]
            [clj-money.html :refer [google-g]]
            [clj-money.state :as state :refer [app-state
                                               +busy
                                               -busy]]
            [clj-money.api.users :as users]
            [clj-money.api.entities :as entities]))

; TODO: fix this duplication from clj-money.core
(defn- receive-entities
  [[entity :as entities]]
  (state/set-entities entities)
  (secretary/dispatch!
    (if entity
      "/scheduled/autorun"
      "/entities")))

(defn- fetch-entities []
  (+busy)
  (entities/select {}
                   :callback -busy
                   :on-success receive-entities))

(defn- authenticate
  [page-state]
  (+busy)
  (users/authenticate (:credentials @page-state)
                      :callback -busy
                      :on-success (fn [{:keys [user auth-token]}]
                                    (swap! app-state assoc
                                           :current-user user
                                           :auth-token auth-token)
                                    (fetch-entities))))

(defn- login []
  (let [page-state (r/atom {:credentials {}})
        credentials (r/cursor page-state [:credentials])]
    (set-focus "email")
    (fn []
      [:div.mt-5
       [:div.row
        [:div.col-md-6
         [:h1 "Login"]
         [:form {:no-validate true
                 :on-submit (fn [e]
                              (.preventDefault e)
                              (v/validate credentials)
                              (when (v/valid? credentials)
                                (authenticate page-state)))}
          [forms/email-field credentials [:email] {:validations #{::v/required}}]
          [forms/password-field credentials [:password] {:validations #{::v/required}}]
          [:button.btn.btn-primary {:type :submit
                                    :title "Click here to sign in."}
           (icon-with-text :box-arrow-in-left "Sign in")]]]
        [:div.col-md-6
         [:h3.mt-3 "Other sign in options:"]
         [:ul.list-group
          [:li.list-group-item.d-flex.justify-content-center
           [:a#login.btn.btn-light {:href "/auth/google/start"
                                    :title "Click here to sign in with a Google account"}
            (google-g)
            [:span "Sign in with Google"]]]]]]])))

(secretary/defroute "/login" []
  (swap! app-state assoc :page #'login))
