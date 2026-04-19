(ns clj-money.views.users
  (:require [clojure.string :as string]
            [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [accountant.core :as accountant]
            [dgknght.app-lib.dom :refer [set-focus]]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.forms-validation :as v]
            [dgknght.app-lib.inflection :refer [humanize title-case]]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [clj-money.icons :refer [icon-with-text]]
            [clj-money.config :refer [env]]
            [clj-money.html :as html]
            [clj-money.components :refer [spinner]]
            [clj-money.state :refer [app-state +busy -busy]]
            [clj-money.app :refer [fetch-entities]]
            [clj-money.api.users :as users]
            [clj-money.views.invitations :as invs]))

(defn- authenticate
  [page-state]
  (+busy)
  (users/authenticate
    (:credentials @page-state)
    :callback -busy
    :on-success (fn [{:keys [user auth-token]}]
                  (swap! app-state assoc
                         :current-user user
                         :auth-token auth-token)
                  (fetch-entities :on-complete #(accountant/navigate! "/")))))

(defn- oauth-button
  [provider]
  ^{:key (name provider)}
  [:a.btn.btn-secondary.d-flex.justify-content-center.mb-2
   {:href  (str "/auth/" (name provider) "/start")
    :title (str "Click here to sign in with a "
                (string/capitalize (name provider))
                " account")}
   (html/logo provider)
   [:div.ms-2 (str "Sign in with " (string/capitalize (name provider)))]])

(defn- login []
  (let [page-state  (r/atom {:credentials {}})
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
        (when (seq (env :oauth-providers))
          [:div.col-md-6
           [:h3.mt-3 "Other sign in options:"]
           [:div.d-flex.flex-column
            (doall (map oauth-button (env :oauth-providers)))]])]])))

(secretary/defroute "/login" []
  (swap! app-state assoc :page #'login))

(defn- load-users
  [page-state]
  (+busy)
  (users/select
    :callback -busy
    :on-success #(swap! page-state assoc :users %)))

(defn- user-row
  [{:user/keys [first-name last-name email roles]}]
  ^{:key email}
  [:tr
   [:td (str first-name " " last-name)]
   [:td email]
   [:td (->> roles (map (comp humanize name)) (interpose ", ") (apply str))]])

(defn- users-table
  [page-state]
  (let [site-users (r/cursor page-state [:users])]
    (fn []
      [:table.table.mt-3
       [:thead
        [:tr
         [:th "Name"]
         [:th "Email"]
         [:th "Roles"]]]
       [:tbody
        (cond
          (seq @site-users)
          (doall (map user-row @site-users))

          @site-users
          [:tr [:td.text-body-tertiary {:col-span 3} "There are no users"]]

          :else
          [:tr [:td {:col-span 3} [spinner]]])]])))

(def ^:private tab-types
  [:users :invitations])

(defn- tab-nav-item-fn
  [page-state]
  (fn [id]
    {:id id
     :label (title-case (humanize id))
     :active? (= id (get-in @page-state [:selected-nav]))
     :nav-fn #(swap! page-state assoc :selected-nav id)}))

(defn- index []
  (let [page-state  (r/atom {:selected-nav :users})
        selected-nav (r/cursor page-state [:selected-nav])]
    (load-users page-state)
    (fn []
      [:div.mt-3
       [:h1 "Users"]
       (bs/nav-tabs (map (tab-nav-item-fn page-state) tab-types))
       (case @selected-nav
         :users
         [users-table page-state]

         :invitations
         [invs/index])])))

(secretary/defroute "/users" []
  (swap! app-state assoc :page #'index :active-nav :users))
