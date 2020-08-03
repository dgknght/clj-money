(ns clj-money.views.dashboard
  (:require [clojure.string :as string]
            [reagent.core :as r]
            [reagent.format :refer [currency-format]]
            [clj-money.state :refer [current-entity]]
            [clj-money.inflection :refer [title-case]]
            [clj-money.bootstrap :as bs]
            [clj-money.plain-forms :as forms]
            [clj-money.notifications :as notify]
            [clj-money.html :as html]
            [clj-money.accounts :refer [nest unnest]]
            [clj-money.api.accounts :as accounts]
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
  (let [new-monitor (r/cursor state [:new-monitor])
        accounts (r/cursor state [:accounts])]
    (fn []
      [:div
       [forms/typeahead-field
        new-monitor
        [:account-id]
        {:search-fn (fn [input callback]
                      (let [term (string/lower-case input)]
                        (->> @accounts
                                        vals
                                        (filter #(string/includes? (string/lower-case (:path %))
                                                                     term))
                                        callback)))
         :caption-fn :path
         :value-fn :id
         :find-fn (fn [id callback]
                    (->> @accounts
                         vals
                         (filter #(= id (:id %)))
                         first
                         callback))}]
       [:button.btn.btn-primary {:on-click #(save-monitor state)
                                 :title "Click here to add this new monitor"
                                 :disabled (not (:account-id @new-monitor))}
        "Save"]
       (html/space)
       [:button.btn.btn-secondary {:on-click #(swap! state dissoc :new-monitor)
                                   :title "Click here to close this form without creating a new monitor"}
        "Cancel"]])))

(defn- load-accounts
  [state]
  (accounts/get-all #(swap! state assoc :accounts (->> %
                                                       nest
                                                       unnest
                                                       (map (juxt :id identity))
                                                       (into {})))
                    (notify/danger-fn "Unable to load the accounts: %s")))

(defn- monitor-svg
  [{:keys [percentage actual-percent actual prorated-budget]} opts]
  (let [bar-width (str (* 96 actual-percent) "%")
        [bar-fill
         line-stroke] (if (< percentage actual-percent)
                        ["url(#negative-fill)" "var(--white)"]
                        ["url(#positive-fill)" "var(--primary)"])
        line-x (str (* 100 percentage) "%")]
    [:svg (merge opts {:version "1.1"
                       :xmlns "http://www.w3.org/2000/svg"})
     [:defs
      [:linearGradient {:id :positive-fill
                        :x1 0 :y1 0
                        :x2 0 :y2 1}
       [:stop {:offset "0%" :stop-color "var(--success)"}]
       [:stop {:offset "40%" :stop-color "#30c953"}]
       [:stop {:offset "100%" :stop-color "var(--success)"}]]
      [:linearGradient {:id :negative-fill
                        :x1 0 :y1 0
                        :x2 0 :y2 1}
       [:stop {:offset "0%" :stop-color "var(--danger)"}]
       [:stop {:offset "40%" :stop-color "#fa5a6a"}]
       [:stop {:offset "100%" :stop-color "var(--danger)"}]]]
     [:rect {:x 0
             :y 0
             :width "100%"
             :height "100%"
             :opacity "0.5"
             :fill "var(--secondary)"
             :stroke "var(--dark)"
             :stroke-width 2}]
     [:rect {:x 1
             :y 1
             :width bar-width
             :height "94%"
             :fill bar-fill
             :stroke-width 0}]
     [:line {:x1 line-x
             :y1 1
             :x2 line-x
             :y2 "100%"
             :stroke line-stroke
             :stroke-width 2}]
     [:text {:x 4
             :y "70%"
             :font-size "90%"
             :stroke "var(--light)"
             :fill "var(--light)"}
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
  [monitor state]
  (let [scope (get-in @state [:monitor-scope])
        account (get-in @state [:accounts (:account-id monitor)])]
    ^{:key (str "budget-monitor-" (:account-id monitor))}
    [:div.d-flex.align-items-start
     [:div.budget-monitor.my-2
      (if (:message monitor)
        [:p (:message monitor)]
        [:figure
         (monitor-svg (get-in monitor [scope]) {:style {:width "100%"
                                                        :height "2em"}})
         [:figcaption (:path account)]])]
     [:div
      {:on-click #(remove-monitor monitor state)
       :style {:margin "0.5em"
               :cursor "pointer"}
       :title "Click here to remove this budget monitor."}
      (bs/icon :x-circle)]]))

(defn- monitor-nav-tab
  [scope state]
  {:elem-key scope
   :caption (title-case (name scope))
   :active? (= scope (get-in @state [:monitor-scope]))
   :on-click #(swap! state assoc :monitor-scope scope)})

(defn- monitors []
  (let [state (r/atom {:monitor-scope :period})
        monitors (r/cursor state [:monitors])
        accounts (r/cursor state [:accounts])
        new-monitor (r/cursor state [:new-monitor])]
    (load-accounts state)
    (load-monitors state)
    (fn []
      [:div
       [:h3 "Monitors"]
       (when (seq @monitors)
         [bs/nav-tabs (map #(monitor-nav-tab % state) [:period :budget])])
       (if (and @monitors @accounts)
         (->> @monitors
              (sort-by #(get-in @accounts [(:account-id %) :path]))
              (map #(monitor % state))
              doall)
         [:div.my-3
          [:div.spinner-border {:role :status}
           [:div.sr-only "loading..."]]])
       (if @new-monitor
         [monitor-form state]
         [:button.btn.btn-secondary {:on-click (fn []
                                                 (swap! state assoc :new-monitor {})
                                                 (html/set-focus "account-id"))
                                     :title "Click here to add a new budget monitor"}
          "Add"])])))

(defn dashboard []
  [:div.row.mt-5
   [:div.col-md-9
    "probably put a simplified balance sheet here"]
   [:div.col-md-3
    [monitors]]])
