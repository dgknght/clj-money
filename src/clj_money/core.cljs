(ns ^:figwheel-hooks clj-money.core
  (:require [clojure.string :as string]
            [cljs.pprint :refer [pprint]]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [reagent.cookies :as cookies]
            [reagent.dom :refer [render]]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [dgknght.app-lib.inflection :refer [humanize]]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [dgknght.app-lib.notifications :as notify]
            [dgknght.app-lib.api :as api]
            [clj-money.state :as state :refer [app-state
                                               +busy
                                               -busy
                                               current-user
                                               current-entity]]
            [clj-money.util :as util]
            [clj-money.views.entities]
            [clj-money.views.imports]
            [clj-money.views.commodities]
            [clj-money.views.accounts]
            [clj-money.views.transactions]
            [clj-money.views.users]
            [clj-money.views.invitations]
            [clj-money.views.budgets]
            [clj-money.views.receipts]
            [clj-money.views.reports]
            [clj-money.views.scheduled]
            [clj-money.views.dashboard :as dashboard]
            [clj-money.cached-accounts :refer [watch-entity]]
            [clj-money.api]
            [clj-money.app :refer [fetch-entities]]
            [clj-money.api.users :as users]))

(swap! forms/defaults assoc-in [::forms/decoration ::forms/framework] ::bs/bootstrap-5)
(swap! api/defaults assoc :extract-body :before)

(defn- not-found []
  [:div.mt-3
   [:h1 "Page Not Found"]
   [:p "The page you requested does not exist."]
   [:a.btn.btn-secondary {:href "/"} "Return home"]])


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
   {:id :users
    :tool-tip "Click here to manage users"
    :required-role :admin}
   {:id :logout
    :tool-tip "Click here to sign out of the system"
    :path "#"
    :nav-fn (fn []
              (state/logout)
              (cookies/remove! :auth-token)
              (accountant/navigate! "/login"))}])

(def unauthenticated-nav-items
  [{:id :login
    :path "/login"
    :tool-tip "Click here to sign into the system"}])

(defn- default-nav-item
  [{:keys [id]} active-nav]
  {:label (humanize id)
   :path (str "/" (name id))
   :active? (= id active-nav)
   :tool-tip (str "Click here to manage "
                  (humanize id)
                  ".")})

(defn- authorized?
  [{:user/keys [roles]}]
  (fn [{:keys [required-role]}]
    (or (nil? required-role)
        ((or roles {}) required-role))))

(defn- nav-items
  [active-nav current-user current-entity]
  (if current-user
    (let [entity-filter (if current-entity
                          (constantly true)
                          (some-fn :path :nav-fn))]
      (->> authenticated-nav-items
           (filter (every-pred entity-filter
                               (authorized? current-user)))
           (map #(merge (default-nav-item % active-nav) %))))
    (->> unauthenticated-nav-items
         (map #(merge (default-nav-item % active-nav) %)))))

(defn navbar
  [items entity-name {:keys [profile-photo-url]}]
  [:nav.navbar.navbar-expand-lg.bg-body-tertiary.d-print-none
   [:div.container
    [:a.navbar-brand {:href "/"}
     [:img {:src "/images/logo.svg"
            :alt "abacus logo"
            :width 24
            :height 24}]]
    (when entity-name
      [:a.navbar-brand {:data-bs-toggle "offcanvas"
                        :data-bs-target "#entity-selection"
                        :role :button
                        :aria-controls "entity-selection"}
       entity-name])
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

(defmulti ^:private decorate-nav-item
  (fn [{:keys [id]} _opts]
    id))

(defmethod decorate-nav-item :default [item _opts] item)

(defmethod decorate-nav-item :scheduled
  [item {:keys [pending-scheduled-count]}]
  (cond-> item
    (pos? pending-scheduled-count)
    (assoc :badge pending-scheduled-count
           :badge-class "bg-info text-bg-info")))

(defn- nav []
  (let [active-nav (r/cursor app-state [:active-nav])
        items (make-reaction
                (fn []
                  (map #(decorate-nav-item %
                         {:pending-scheduled-count @state/pending-scheduled-count})
                       (nav-items @active-nav
                                  @current-user
                                  @current-entity))))]
    (fn []
      (navbar
        @items
        (:entity/name @current-entity)
        {:brand "clj-money"
         :brand-path "/"}))))

(defn- alerts []
  (fn []
    (when (seq @notify/notifications)
      [:div#alerts.m-3
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
                           :data-bs-dismiss :offcanvas
                           :class (when (= (:id entity)
                                           (:id current))
                                    "active")}
                          (:entity/name entity)]))))]]
         [:div.col-md-3
          [:ul.nav.nav-pills.nav-fill.flex-md-column
           [:li.nav-item
            [:a.nav-link {:href "/entities"
                          :data-bs-dismiss :offcanvas}
             "Manage Entities"]]
           [:li.nav-item
            [:a.nav-link {:href "/imports"
                          :data-bs-dismiss :offcanvas}
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
      (swap! app-state assoc :mounted? true :page #'dashboard/index)
      (render [current-page] (.getElementById js/document "app")))))


(defn- fetch-current-user []
  (+busy)
  (users/me :callback -busy
            :on-success #(swap! app-state assoc :current-user %)))

(defn- sign-in-from-cookie []
  (if @current-user
    (do
      (if (util/entity-ref? @current-user)
        (users/me :on-success #(reset! current-user %))
        (fetch-entities))
      (fetch-entities))
    (when-let [auth-token (cookies/get :auth-token)]
      (swap! app-state assoc :auth-token auth-token)
      (fetch-current-user)
      (fetch-entities))))

(def ^:private error-messages
  {"oauth_failed"
   "Sign in failed. Please try again."

   "github_email_required"
   (str "Unable to retrieve your GitHub email address. "
        "Please make an email address public in your GitHub profile settings, "
        "then revoke and re-authorize this app from GitHub Settings "
        "\u2192 Applications \u2192 Authorized OAuth Apps.")})

(defn- check-url-error []
  (let [params (js/URLSearchParams. js/window.location.search)
        error  (.get params "error")]
    (when error
      (notify/danger (get error-messages error "An unexpected error occurred."))
      (.replaceState js/window.history nil "" js/window.location.pathname))))

(def ^:private server-path-prefixes
  ["/api/" "/oapi/" "/auth/" "/app/"])

(defn- dispatch-or-not-found
  [path]
  (if (secretary/locate-route path)
    (secretary/dispatch! path)
    (swap! app-state assoc :page #'not-found)))

(defn init! []
  (accountant/configure-navigation!
   {:nav-handler dispatch-or-not-found
    :path-exists? (fn [path]
                    (not-any? #(string/starts-with? path %) server-path-prefixes))})
  (mount-root)
  (sign-in-from-cookie)
  (check-url-error)
  (accountant/dispatch-current!))

(init!)

(add-watch current-entity ::fetch-accounts watch-entity)
