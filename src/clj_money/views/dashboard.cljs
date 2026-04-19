(ns clj-money.views.dashboard
  (:require [clojure.string :as string]
            [cljs.pprint :refer [pprint]]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [reagent.format :refer [currency-format]]
            [secretary.core :as secretary :include-macros true]
            [dgknght.app-lib.inflection :refer [title-case]]
            [dgknght.app-lib.dom :refer [set-focus]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [dgknght.app-lib.forms-validation :as v]
            [clj-money.util :refer [id=]]
            [clj-money.components :refer [button]]
            [clj-money.html :refer [google-logo
                                    github-logo]]
            [clj-money.icons :refer [icon]]
            [clj-money.state :refer [current-user
                                     current-entity
                                     accounts
                                     accounts-by-id
                                     app-state
                                     +busy
                                     -busy]]
            [clj-money.accounts :refer [find-by-path]]
            [clj-money.api.entities :as entities]
            [clj-money.api.reports :as reports]))

(defn- load-monitors
  [state]
  (if @current-entity
    (do (+busy)
        (reports/budget-monitors :callback -busy
                                 :on-success #(swap! state assoc :monitors %)))
    (swap! state dissoc :monitors)))

(defn- save-monitor
  [state]
  (+busy)
  (swap! current-entity
         update-in
         [:entity/settings :settings/monitored-accounts]
         (fnil conj #{})
         {:id (get-in @state [:new-monitor :account :id])})
  (entities/save @current-entity
                 :callback -busy
                 :on-success (fn []
                               (load-monitors state)
                               (swap! state dissoc :new-monitor))))

(defn- monitor-form
  [state]
  (let [new-monitor (r/cursor state [:new-monitor])]
    (fn []
      [:form {:no-validate true
              :on-submit (fn [e]
                           (.preventDefault e)
                           (v/validate new-monitor)
                           (when (v/valid? new-monitor)
                             (save-monitor state)))}
       [:div
        [forms/typeahead-field
         new-monitor
         [:account :id]
         {:search-fn (fn [input callback]
                       (callback (find-by-path input @accounts)))
          :caption-fn #(string/join "/" (:account/path %))
          :caption "Account"
          :value-fn :id
          :find-fn (fn [id callback]
                     (callback (@accounts-by-id id)))}]
        [:button.btn.btn-primary {:type :submit
                                  :title "Click here to add this new monitor"
                                  :disabled (not (get-in @new-monitor [:account :id]))}
         "Save"]
        (html/space)
        [:button.btn.btn-secondary {:on-click #(swap! state dissoc :new-monitor)
                                    :title "Click here to close this form without creating a new monitor"}
         "Cancel"]]])))

(defn- monitor-svg
  [{:report/keys [percentage actual-percent actual prorated-budget]} opts]
  (let [bar-width (str (* 96 actual-percent) "%")
        [bar-fill
         line-stroke] (if (< percentage actual-percent)
                        ["var(--bs-danger)" "var(--bs-white)"]
                        ["var(--bs-success)" "var(--bs-primary)"])
        line-x (str (* 99 percentage) "%")]
    [:svg (merge opts {:version "1.1"
                       :xmlns "http://www.w3.org/2000/svg"})
     [:rect {:x 0
             :y 0
             :rx "8"
             :ry "8"
             :width "100%"
             :height "100%"
             :opacity "0.5"
             :fill "var(--bs-secondary)"}]
     [:rect {:x 1
             :y 1
             :rx "8"
             :ry "8"
             :width bar-width
             :height "94%"
             :fill bar-fill
             :stroke-width 0}]
     [:line {:x1 line-x
             :y1 1
             :x2 line-x
             :y2 "100%"
             :stroke line-stroke
             :stroke-width 4}]
     [:text {:x 4
             :y "70%"
             :font-size "90%"
             :fill "var(--bs-light)"}
      (str (currency-format actual)
           " ("
           (currency-format (- prorated-budget actual))
           ")")]]))

(defn- remove-monitor
  [{:report/keys [account]} state]
  (swap! current-entity
         update-in
         [:entity/settings :settings/monitored-accounts]
         disj
         (:id account))
  (swap! state
         update-in
         [:monitors]
         (fn [monitors]
           (->> monitors
                (remove #(id= account (:report/account %)))
                (into []))))
  (+busy)
  (entities/save @current-entity
                 :callback -busy))

(defn- monitor
  [{:report/keys [scope account message] :as monitor} state]

  ^{:key (str "budget-monitor-" (:id account))}
  [:div.d-flex.align-items-start
   [:div.budget-monitor.my-2
    [:figure
     (if message
       [:div.alert.alert-warning message]
       (monitor-svg (get-in monitor [(keyword "report" (name scope))]) {:style {:width "100%"
                                                                                :height "2em"}}))
     [:figcaption (string/join "/" (:account/path account))]]]
   [:div
    {:on-click #(remove-monitor monitor state)
     :style {:margin-left "0.5em"
             :margin-top "0.6em"
             :cursor "pointer"}
     :title "Click here to remove this budget monitor."}
    (icon :x-circle :size :small)]])

(defn- monitor-nav-tab
  [scope current state]
  {:id scope
   :label (title-case (name scope))
   :active? (= scope current)
   :nav-fn #(swap! state assoc :monitor-scope scope)})

(defn- monitors []
  (let [state (r/atom {:monitor-scope :period})
        scope (r/cursor state [:monitor-scope])
        monitors (r/cursor state [:monitors])
        new-monitor (r/cursor state [:new-monitor])
        monitors-with-detail (make-reaction
                              (fn []
                                (when (and @monitors @accounts-by-id)
                                  (->> @monitors
                                       (map (fn [m]
                                              (-> m
                                                  (update-in [:report/account] (comp @accounts-by-id
                                                                                     :id))
                                                  (assoc :report/scope @scope))))
                                       (sort-by #(get-in % [:report/account :account/path]))
                                       (into [])))))]
    (load-monitors state)
    (add-watch current-entity ::monitors (fn [_ _ prev current]
                                           (if current
                                             (when-not (id= prev current)
                                               (load-monitors state))
                                             (swap! state dissoc :monitors))))
    (fn []
      [:div
       [:h3 "Monitors"]
       (when (seq @monitors)
         [bs/nav-tabs (map #(monitor-nav-tab % @scope state) [:period :budget])])
       (if @monitors-with-detail
         (->> @monitors-with-detail
              (map #(monitor % state))
              doall)
         [:div.my-3
          [:div.spinner-border {:role :status}
           [:div.visually-hidden "loading..."]]])
       (if @new-monitor
         [monitor-form state]
         [button {:html {:on-click (fn []
                                     (swap! state assoc :new-monitor {})
                                     (set-focus "account-id"))
                         :class "btn-secondary"
                         :title "Click here to add a new budget monitor"}
                  :caption "Add"
                  :icon :plus}])])))

(defn- dashboard []
  [:div.row.mt-3
   [:div.col-md-9
    (if @current-entity
      "probably put a simplified balance sheet here"
      [:a {:href "/entities"} "Create an entity"])]
   [:div.col-md-3
    [monitors]]])

(defn- welcome []
  [:div.jumbotron.mt-3
   [:div.d-flex
    [:img {:src "/images/logo.svg"
           :alt "abacus logo"
           :width 64
           :height 64}]
    [:h1.display-5.ms-3 "clj-money"]]
   [:p "This is a double-entry accounting application that aims to be available anywhere."]
   [:div.d-flex.justify-content-center
    [:ul.list-group
     [:li.list-group-item.d-flex.justify-content-center
      [:a#login.btn.btn-secondary {:href "/auth/google/start"
                                   :title "Click here to sign in with a Google account"}
       (google-logo)
       [:span "Sign in with Google"]]]
     [:li.list-group-item.d-flex.justify-content-center
      [:a#login.btn.btn-secondary {:href "/auth/github/start"
                                   :title "Click here to sign in with a Github account"}
       (github-logo)
       [:span "Sign in with Github"]]]]]])

(defn- index
  []
  (if @current-user
    [dashboard]
    [welcome]))

(secretary/defroute "/" []
  (swap! app-state assoc :page #'index))
