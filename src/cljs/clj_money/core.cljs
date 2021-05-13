(ns clj-money.core
  (:require [clojure.string :as string]
            [reagent.core :as r]
            [reagent.cookies :as cookies]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [dgknght.app-lib.inflection :refer [humanize]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.bootstrap-4 :as bs]
            [dgknght.app-lib.notifications :as notify]
            [clj-money.j-query :as jq]
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
            [clj-money.api.entities :as entities]
            [clj-money.bootstrap :as bootstrap]
            [clj-money.api.users :as users]))

(swap! forms/defaults assoc-in [::forms/decoration ::forms/framework] ::bs/bootstrap-4)

(defn home-page []
  [:div.jumbotron.mt-3
   [:h1.display-5 "clj-money"]
   [:p "This is a double-entry accounting application that aims to be available anywhere."]
   [:a#login.btn.btn-light {:href "/auth/google/start"
                            :title "Click here to sign in with a Google account"}
    (google-g)
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
   {:id :receipts
    :tool-tip "Click here to enter receipts"}
   {:id :reports
    :tool-tip "Click here to view reports"}
   {:id :scheduled
    :tool-tip "Click here to manage schedule transactions"}])

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
                            (state/logout)
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

(defn- render-toasts []
  (when (seq @notify/toasts)
    (.setTimeout js/window #(jq/toast ".toast") 100)
    [:div.toast-container
     (for [toast @notify/toasts
           :let [elem-id (str "toast-" (:id toast))]]
       ^{:key elem-id}
       [:div.toast {:id elem-id
                    :data-animation true
                    :data-autohide true
                    :data-delay notify/toast-delay
                    :role "alert"
                    :aria-live "assertive"
                    :aria-atomic true}
        [:div.toast-header
         [:strong.mr-auto (:title toast)]
         [:button.ml-2.mb-1.close {:type :button
                                   :data-dismiss :toast
                                   :aria-label "Close"}
          [:span {:aria-hidden true} (html/special-char :times)]]]
        [:div.toast-body
         (:body toast)]])]))

(defn- current-page []
  (let [page (r/cursor app-state [:page])]
    (fn []
      [:div
       [nav]
       [:div.container
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
    receive-entities
    (notify/danger-fn "Unable to get the entities: %s")))

(defn- fetch-current-user []
  (users/me
    #(swap! app-state assoc :current-user %)
    (notify/danger-fn "Unable to get information for the user: %s")))

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
