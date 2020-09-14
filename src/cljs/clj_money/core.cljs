(ns clj-money.core
  (:require [clojure.string :as string]
            [reagent.core :as r]
            [reagent.cookies :as cookies]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [clj-money.inflection :refer [humanize]]
            [clj-money.state :refer [app-state
                                     current-user
                                     current-entity
                                     logout]]
            [clj-money.notifications :as notify]
            [clj-money.html :as html]
            [clj-money.views.entities]
            [clj-money.views.imports]
            [clj-money.views.commodities]
            [clj-money.views.accounts]
            [clj-money.views.transactions]
            [clj-money.views.users]
            [clj-money.views.budgets]
            [clj-money.views.reports]
            [clj-money.views.dashboard :refer [dashboard]]
            [clj-money.api.entities :as entities]
            [clj-money.dom :refer [app-element]]
            [clj-money.bootstrap :as bootstrap]
            [clj-money.api.users :as users]))

(defn home-page []
  [:div.jumbotron.mt-3
   [:h1.display-5 "clj-money"]
   [:p "This is a double-entry accounting application that aims to be available anywhere."]
   [:a#login.btn.btn-light {:href "/auth/google/start"
                              :title "Click here to sign in with a Google account"}
    (html/google-g)
    [:span "Sign in with Google"]]])

(secretary/defroute "/" []
    (swap! app-state assoc :page (if @current-user
                                   #'dashboard
                                   #'home-page)))

(defn- nil-page []
  (html/space))

(defn- entity->nav-item
  [{:keys [id name] :as entity}]
  {:id id
   :caption name
   :on-click (fn []
               (let [page (get-in @app-state [:page])]
                 (swap! app-state assoc :page #'nil-page)
                 (js/setTimeout
                     #(swap! app-state assoc
                             :current-entity entity
                             :page page)
                     5)))})

(def authenticated-nav-items
  [{:id :commodities}
   {:id :accounts}
   {:id :budgets}
   {:id :reports
    :tool-tip "Click here to view reports"}])

(defn- assoc-if-nil
  [m k v]
  (if (get-in m [k])
    m
    (assoc m k v)))

(defn- nav-items
  [current-user current-entity active-nav]
  (if current-user
    (if current-entity
      (map (fn [{:keys [id] :as item}]
             (-> item
                 (assoc-if-nil :caption (humanize id))
                 (assoc-if-nil :url (str "/" (name id)))
                 (assoc-if-nil :active? (= id active-nav))
                 (assoc-if-nil :tool-tip (str "Click here to manage "
                                              (humanize id)
                                              "."))))
           authenticated-nav-items)
      [])
    [{:id :login
      :url "/login"
      :caption "Login"
      :tool-top "Click here to sign into the system"}]))

(defn- secondary-nav-items
  [current-user entities current-entity]
  (when current-user
    (let [items (if (seq entities)
                  [{:id :entities
                    :role :dropdown
                    :caption (:name current-entity)
                    :children (concat (map entity->nav-item
                                           entities)
                                      [{:role :separator
                                        :id "entity-separator"}
                                       {:id "manage-entities"
                                        :url "/entities"
                                        :caption "Manage Entities"
                                        :tool-tip "Click here to manage your entities."}
                                       {:id "manage-imports"
                                        :url "/imports"
                                        :caption "Manage Imports"
                                        :tool-tip "Click here to manage your imports."}])}]
                  [])]
      (concat items
              [{:id :current-user
                :caption (->> ((juxt :first-name :last-name) current-user)
                              (string/join " "))}
               {:id :logout
                :caption "Logout"
                :on-click (fn []
                            (logout)
                            (cookies/remove! :auth-token)
                            (secretary/dispatch! "/"))}]))))

(defn- nav []
  (let [active-nav (r/cursor app-state [:active-nav])
        entities (r/cursor app-state [:entities])]
    (fn []
      (let [items (nav-items @current-user @current-entity @active-nav)
            secondary-items (secondary-nav-items @current-user @entities @current-entity)]
        (bootstrap/navbar
          {:title "clj-money"
           :title-url "/"
           :items items
           :secondary-items secondary-items})))))

(defn- alerts []
  (fn []
    (when (seq @notify/notifications)
      [:div#alerts
       (doall (for [n @notify/notifications]
                (bootstrap/alert n #(notify/unnotify n))))])))

(defn- current-page []
  (let [page (r/cursor app-state [:page])]
    (fn []
      [:div
       [nav]
       [:div.container
        [alerts]
        [@page]]])))

(defn mount-root []
  (let [mounted? (r/cursor app-state [:mounted?])]
    (when-not @mounted?
      (swap! app-state assoc :mounted? true :page #'home-page)
      (r/render [current-page] (app-element)))))

(defn- set-default-entity
  [state entity]
  (if entity
    (assoc state :current-entity entity)
    (dissoc state :current-entity)))

(defn- sign-in-from-cookie []
  (when-not @current-user
    (when-let [auth-token (cookies/get :auth-token)]
      (swap! app-state assoc :auth-token auth-token)
      (users/me
        #(swap! app-state assoc :current-user %)
        (notify/danger-fn "Unable to get information for the user: %s"))
      (entities/select
        (fn [[entity :as result]]
          (swap! app-state (fn [s]
                             (-> s
                                 (assoc :entities result)
                                 (set-default-entity entity))))
          (if entity
            (secretary/dispatch! "/")
            (secretary/dispatch! "/entities")))
        (notify/danger-fn "Unable to get the entities: %s")))))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler #(secretary/dispatch! %)
     :path-exists? #(secretary/locate-route %)})
  (accountant/dispatch-current!)
  (sign-in-from-cookie)
  (mount-root))

(init!)
