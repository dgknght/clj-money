(ns clj-money.core
  (:require [reagent.core :as r]
            [reagent.cookies :as cookies]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [dgknght.app-lib.inflection :refer [humanize]]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [dgknght.app-lib.notifications :as notify]
            [dgknght.app-lib.api :as api]
            [clj-money.state :as state :refer [app-state
                                               current-user
                                               current-entity]]
            [clj-money.html :refer [google-g]]
            [clj-money.views.entities]
            [clj-money.views.imports]
            [clj-money.views.commodities]
            [clj-money.views.accounts]
            [clj-money.views.transactions]
            [clj-money.views.users]
            [clj-money.views.budgets]
            [clj-money.views.receipts]
            [clj-money.views.reports]
            [clj-money.views.scheduled]
            [clj-money.views.dashboard :refer [dashboard]]
            [clj-money.cached-accounts :refer [watch-entity]]
            [clj-money.api.entities :as entities]
            [clj-money.api.users :as users]))

(swap! forms/defaults assoc-in [::forms/decoration ::forms/framework] ::bs/bootstrap-5)
(swap! api/defaults assoc :extract-body :before)

(defn home-page []
  [:div.jumbotron.mt-3
   [:div.d-flex
    [:img {:src "/images/logo.svg"
           :alt "abacus logo"
           :width 64
           :height 64}]
    [:h1.display-5.ms-3 "clj-money"]]
   [:p "This is a double-entry accounting application that aims to be available anywhere."]
   [:a#login.btn.btn-light {:href "/auth/google/start"
                            :title "Click here to sign in with a Google account"}
    (google-g)
    [:span "Sign in with Google"]]])

(secretary/defroute "/" []
  (swap! app-state assoc :page (if @current-user
                                 #'dashboard
                                 #'home-page)))

(def authenticated-nav-items
  [{:id :commodities}
   {:id :accounts}
   {:id :budgets}
   {:id :receipts
    :tool-tip "Click here to enter receipts"}
   {:id :reports
    :tool-tip "Click here to view reports"}
   {:id :scheduled
    :tool-tip "Click here to manage schedule transactions"}
   {:id :logout
    :tool-tip "Click here to sign out of the system"
    :nav-fn (fn []
              (state/logout)
              (cookies/remove! :auth-token)
              (secretary/dispatch! "/"))}])

(def unauthenticated-nav-items
  [{:id :login
    :path "/login"
    :tool-tip "Click here to sign into the system"}])

(defn- default-nav-item
  [{:keys [id]} active-nav]
  {:label (humanize id)
   :toggle "#primary-nav"
   :path (str "/" (name id))
   :active? (= id active-nav)
   :tool-tip (str "Click here to manage "
                  (humanize id)
                  ".")})

(defn- available?
  [nav-item]
  (or (:path nav-item)
      (:nav-fn nav-item)
      @current-entity))

(defn- nav-items
  [active-nav]
  (->> (if @current-user
           authenticated-nav-items
           unauthenticated-nav-items)
         (filter available?)
         (map #(merge (default-nav-item % active-nav)
                   %))))

(defn navbar
  [items {:keys [profile-photo-url]}]
  [:nav.navbar.navbar-expand-lg.navbar-light.bg-light.d-print-none
   [:div.container
    [:a.navbar-brand {:href "/"}
     [:img {:src "/images/logo.svg"
            :alt "abacus logo"
            :width 24
            :height 24}]]
    (when-let [entity (:name @current-entity)]
      [:a.navbar-brand {:href "#entity-selection"
                        :data-bs-toggle "offcanvas"
                        :role :button
                        :aria-controls "entity-selection"}
       entity])
    [:button.navbar-toggler {:type :button
                             :data-bs-toggle :collapse
                             :data-bs-target "#primary-nav"
                             :aria-controls "primary-nav"
                             :aria-expanded false
                             :aria-label "Toggle Navigation"}
     [:span.navbar-toggler-icon]]
    (when (seq items)
      [:div#primary-nav.collapse.navbar-collapse
       [:ul.navbar-nav.me-auto.mb-2.mb-lg-0
        (->> items
             (map bs/nav-item)
             doall)]])

    (when profile-photo-url ; TODO: Fetch this when authenticating via google
      [:img.rounded-circle.d-none.d-lg-block
       {:src profile-photo-url
        :style {:max-width "32px"}
        :alt "Profile Photo"}])]])

(defn- nav []
  (let [active-nav (r/cursor app-state [:active-nav])]
    (fn []
      (let [items (nav-items @active-nav)]
        (navbar
          items
          {:brand "clj-money"
           :brand-path "/"})))))

(defn- alerts []
  (fn []
    (when (seq @notify/notifications)
      [:div#alerts
       (doall (for [n @notify/notifications]
                (bs/alert n)))])))

(defn- render-toasts []
  (when (seq @notify/toasts)
    (js/setTimeout bs/init-toasts 200)
    [:div.toast-container
     (->> @notify/toasts
          (map bs/toast)
          doall) ]))

(defn- entity-selection []
  (let [entities (r/cursor app-state [:entities])]
    (fn []
      [:div#entity-selection.offcanvas.offcanvas-top {:tab-index -1}
       [:div.offcanvas-header
        [:h5 "Entities"]
        [:button.btn-close.text-reset {:data-bs-dismiss :offcanvas
                                       :aria-label "Close" }]]
       [:div.offcanvas-body
        [:div.row
         [:div.col-md-3.offset-md-3
          [:div.list-group
           (let [current @current-entity]
             (->> @entities
                  (map (fn [entity]
                         ^{:key (str "entity-selection-" (:id entity))}
                         [:button.list-group-item
                          {:on-click #(swap! app-state assoc :current-entity entity)
                           :data-bs-toggle :offcanvas
                           :class (when (= (:id entity)
                                           (:id current))
                                    "active")}
                          (:name entity)]))))]]
         [:div.col-md-3
          [:ul.nav.nav-pills.nav-fill.flex-md-column
           [:li.nav-item
            [:a.nav-link {:href "/entities"
                          :data-bs-toggle :offcanvas}
             "Manage Entities"]]
           [:li.nav-item
            [:a.nav-link {:href "/imports"
                          :data-bs-toggle :offcanvas}
             "Manage Imports"]]]]]]])))

(defn- current-page []
  (let [page (r/cursor app-state [:page])]
    (fn []
      [:div.h-100
       [nav]
       [entity-selection]
       [:div.container.h-100
        [alerts]
        [render-toasts]
        [@page]]])))

(defn mount-root []
  (let [mounted? (r/cursor app-state [:mounted?])]
    (when-not @mounted?
      (swap! app-state assoc :mounted? true :page #'home-page)
      (r/render [current-page] (.getElementById js/document "app")))))

(defn- receive-entities
  [[entity :as entities]]
  (state/set-entities entities)
  (if entity
    (secretary/dispatch! "/scheduled/autorun")
    (secretary/dispatch! "/entities")))

(defn- fetch-entities []
  (entities/select
    (map receive-entities)))

(defn- fetch-current-user []
  (users/me (map #(swap! app-state assoc :current-user %))))

(defn- sign-in-from-cookie []
  (when-not @current-user
    (when-let [auth-token (cookies/get :auth-token)]
      (swap! app-state assoc :auth-token auth-token)
      (fetch-current-user)
      (fetch-entities))))

(defn init! []
  (accountant/configure-navigation!
   {:nav-handler #(secretary/dispatch! %)
    :path-exists? #(secretary/locate-route %)})
  (accountant/dispatch-current!)
  (sign-in-from-cookie)
  (mount-root))

(init!)

(add-watch current-entity ::fetch-accounts watch-entity)
