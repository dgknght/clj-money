(ns clj-money.views.users
  (:require [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.forms-validation :as v]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [clj-money.html :refer [google-g]]
            [clj-money.state :as state :refer [app-state]]
            [clj-money.api.users :as users]
            [clj-money.api.entities :as entities]))

; TODO: fix this duplication from clj-money.core
(defn- receive-entities
  [[entity :as entities]]
  (state/set-entities entities)
  (if entity
    (secretary/dispatch! "/scheduled/autorun")
    (secretary/dispatch! "/entities")))

(defn- fetch-entities []
  (entities/select
    (map receive-entities)))

(defn- authenticate
  [page-state]
  (users/authenticate (:credentials @page-state)
                      (map (fn [{:keys [user auth-token]}]
                             (swap! app-state assoc
                                    :current-user user
                                    :auth-token auth-token)
                             (fetch-entities)))))

(defn- login []
  (let [page-state (r/atom {:credentials {}})
        credentials (r/cursor page-state [:credentials])]
    (html/set-focus "email")
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
           (bs/icon-with-text :box-arrow-in-left "Sign in")]]]
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
