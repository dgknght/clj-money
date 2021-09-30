(ns clj-money.views.dashboard
  (:require [clojure.string :as string]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [reagent.format :refer [currency-format]]
            [dgknght.app-lib.inflection :refer [title-case]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.notifications :as notify]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [clj-money.state :refer [current-entity
                                     accounts
                                     accounts-by-id]]
            [clj-money.accounts :refer [find-by-path]]
            [clj-money.api.entities :as entities]
            [clj-money.api.reports :as reports]))

(defn- load-monitors
  [state]
  (reports/budget-monitors #(swap! state assoc :monitors %)
                           (notify/danger-fn "Unable to load the monitors: %s")))

(defn- save-monitor
  [state]
  (swap! current-entity
         update-in
         [:settings :monitored-account-ids]
         (fnil conj [])
         (get-in @state [:new-monitor :account-id]))
  (entities/save @current-entity
                 (fn []
                   (load-monitors state)
                   (swap! state dissoc :new-monitor))
                 (notify/danger-fn "Unable to save the new monitor: %s")))

(defn- monitor-form
  [state]
  (let [new-monitor (r/cursor state [:new-monitor])]
    (fn []
      [:div
       [forms/typeahead-field
        new-monitor
        [:account-id]
        {:search-fn (fn [input callback]
                      (callback (find-by-path input @accounts)))
         :caption-fn #(string/join "/" (:path %))
         :value-fn :id
         :find-fn (fn [id callback]
                    (callback (@accounts-by-id id)))}]
       [:button.btn.btn-primary {:on-click #(save-monitor state)
                                 :title "Click here to add this new monitor"
                                 :disabled (not (:account-id @new-monitor))}
        "Save"]
       (html/space)
       [:button.btn.btn-secondary {:on-click #(swap! state dissoc :new-monitor)
                                   :title "Click here to close this form without creating a new monitor"}
        "Cancel"]])))

(defn- monitor-svg
  [{:keys [percentage actual-percent actual prorated-budget]} opts]
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
  [{:keys [account-id]} state]
  (swap! current-entity
         update-in
         [:settings :monitored-account-ids]
         disj
         account-id)
  (swap! state
         update-in
         [:monitors]
         (fn [monitors]
           (remove #(= account-id
                       (:account-id %))
                   monitors)))
  (entities/update @current-entity
                   identity
                   (notify/danger-fn "Unable to remove the budget monitor: %s")))

(defn- monitor
  [{:keys [scope account] :as monitor} state]

  (.log js/console (prn-str {::monitor monitor}))

  ^{:key (str "budget-monitor-" (:id account))}
  [:div.d-flex.align-items-start
   [:div.budget-monitor.my-2
    (if (:message monitor)
      [:p (:message monitor)]
      [:figure
       (monitor-svg (get-in monitor [scope]) {:style {:width "100%"
                                                      :height "2em"}})
       [:figcaption (string/join "/" (:path account))]])]
   [:div
    {:on-click #(remove-monitor monitor state)
     :style {:margin "0.5em"
             :cursor "pointer"}
     :title "Click here to remove this budget monitor."}
    (bs/icon :x-circle {:size :small})]])

(defn- monitor-nav-tab
  [scope state]
  {:elem-key scope
   :caption (title-case (name scope))
   :active? (= scope (get-in @state [:monitor-scope]))
   :on-click #(swap! state assoc :monitor-scope scope)})

(defn- monitors []
  (let [state (r/atom {:monitor-scope :period})
        scope (r/cursor state [:monitor-scope])
        monitors (r/cursor state [:monitors])
        new-monitor (r/cursor state [:new-monitor])
        monitors-with-detail (make-reaction (fn []
                                              (when (and @monitors @accounts-by-id)
                                                (->> @monitors
                                                     (map #(assoc %
                                                                  :account (@accounts-by-id (:account-id %))
                                                                  :scope @scope))
                                                     (sort-by #(get-in % [:account :path]))))))]
    (load-monitors state)
    (add-watch current-entity ::monitors (fn [& _] (load-monitors state)))
    (fn []
      [:div
       [:h3 "Monitors"]
       (when (seq @monitors)
         [bs/nav-tabs (map #(monitor-nav-tab % state) [:period :budget])])
       (if (and @monitors @accounts)
         (->> @monitors-with-detail
              (map #(monitor % state))
              doall)
         [:div.my-3
          [:div.spinner-border {:role :status}
           [:div.visually-hidden "loading..."]]])
       (if @new-monitor
         [monitor-form state]
         [:button.btn.btn-secondary {:on-click (fn []
                                                 (swap! state assoc :new-monitor {})
                                                 (html/set-focus "account-id"))
                                     :title "Click here to add a new budget monitor"}
          "Add"])])))

(defn dashboard []
  [:div.row.mt-3
   [:div.col-md-9
    "probably put a simplified balance sheet here"]
   [:div.col-md-3
    [monitors]]])
