(ns clj-money.views.imports
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.pprint :refer [pprint]]
            [cljs.core.async :refer [timeout
                                     <!]]
            [cljs-time.core :as t]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [secretary.core :as secretary :include-macros true]
            [dgknght.app-lib.core :refer [present?]]
            [dgknght.app-lib.web :refer [format-percent
                                         format-decimal
                                         format-date
                                         format-date-time]]
            [dgknght.app-lib.dom :refer [set-focus]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.forms :refer [text-field]]
            [dgknght.app-lib.notifications :as notify]
            [dgknght.app-lib.forms-validation :as v]
            [clj-money.dates :as dates]
            [clj-money.icons :refer [icon icon-with-text]]
            [clj-money.components :refer [button]]
            [clj-money.dnd :as dnd]
            [clj-money.state :as state :refer [app-state
                                               +busy
                                               -busy
                                               busy?]]
            [clj-money.api.imports :as imports]))

(defn- load-imports
  [page-state]
  (+busy)
  (imports/select {}
                  :callback -busy
                  :on-success #(->> %
                                    (sort-by :import/entity-name)
                                    (swap! page-state assoc :imports))))

(defn- delete-import
  [imp page-state]
  (+busy)
  (imports/delete imp
                  :callback -busy
                  :on-success #(load-imports page-state)))

(defn- append-dropped-files
  [event]
  (fn
    [import-data]
    (update-in import-data
               [:files]
               (fnil concat [])
               (dnd/data-files event))))

(defn- file-list
  [import-data]
  (when (seq (:files import-data))
    [:ul.list-group.list-group-flush
     (for [file (:files import-data)]
       ^{:key (.-name file)}
       [:li.list-group-item (.-name file)])]))

(defn- import-title
  [page-state]
  (let [imp (r/cursor page-state [:active])]
    (fn []
      [:strong (str "Import: " (:import/entity-name @imp))])))

(defn- progress-time-elapsed
  ([{:keys [started-at completed-at completed]}]
   (progress-time-elapsed started-at completed-at completed))
  ([started-at completed-at completed]
   (when (and started-at (< 0 completed))
     (let [s (dates/unserialize-instant started-at)
           c (dates/unserialize-instant completed-at)]
       [:span (dates/format-interval (t/interval s (or c
                                                       (t/now))))]))))

(defn- progress-row
  [[progress-type {:keys [total completed] :as p}]]
  ^{:key (str "progress-" (name progress-type))}
  [:tr
   [:td.col-sm-6
    [:div.d-flex.flex-column
     [:span (name progress-type)]
     (progress-time-elapsed p)]]
   [:td.col-sm-6.text-center
    (let [perc (if (< 0 total)
                 (* 100 (/ completed total))
                 0)
          formatted-perc (format-percent (/ perc 100) {:fraction-digits 0})]
      [:div.progress
       [:div.progress-bar
        {:aria-valuenow completed
         :aria-valuemax total
         :aria-valuemin 0
         :role "progressbar"
         :class (cond
                  (> perc 100)
                  "bg-danger progress-bar-striped"
                  (= perc 100)
                  "bg-success"
                  :else
                  "progress-bar-striped")
         :style {"width" (str (if (> perc 100)
                                100
                                perc)
                              "%")}}
        (when (<= 50 perc)
          formatted-perc)]
       (when (> 50 perc)
         [:span.ps-1
          formatted-perc])])
    [:span.text-body-secondary.fw-lighter
     {:style {:font-size "0.8em"}}
     (str (format-decimal completed {:fraction-digits 0})
          "/"
          (format-decimal total {:fraction-digits 0}))]]])

(defn- progress-table
  [page-state]
  (let [progress (r/cursor page-state [:progress])
        started-at (r/cursor progress [:started-at])
        completed-at (r/cursor progress [:completed-at])
        processes (r/cursor progress [:processes])]
    (fn []
      [:table.table.table-hover
       [:tbody
        [:tr
         [:th "Record Type"]
         [:th.text-center "Progress"]]
        (->> @processes
             (sort-by (comp name first))
             (map (comp progress-row
                        #(update-in %
                                    [1 :completed-at]
                                    (fnil identity @completed-at))))
             doall)]
       [:tfoot
        [:tr
         [:td.text-end {:col-span 2}
          "Total: "
          (progress-time-elapsed @started-at
                                 @completed-at
                                 1)]]]])))

(def auto-refresh (r/atom false))

(declare load-import)

(defn- load-progress
  [page-state]
  (let [import (get-in @page-state [:active])]
    (imports/progress import
                      :on-success (fn [res]
                                    (when (:finished res)
                                      (reset! auto-refresh false))
                                    (when @auto-refresh
                                      (go
                                        (<! (timeout 1000))
                                        (load-progress page-state)))
                                    (swap! page-state assoc :progress res)))))

(defn- receive-import
  ([page-state] (partial receive-import page-state))
  ([page-state received]
   (swap! page-state assoc :active received)
   (load-progress page-state)))

(defn- load-import
  [page-state]
  ; Don't set busy because this happens constantly during import
  (imports/get (get-in @page-state [:active :id])
               :on-success (receive-import page-state)))

(defn- start-import
  [imp page-state]
  (swap! page-state assoc :active imp)
  (+busy)
  (imports/start imp
                 :callback -busy
                 :on-failure (fn [& args]
                               (pprint {::failure args}))
                 :on-success (fn [{:keys [import]}]
                               (reset! auto-refresh true)
                               (receive-import page-state import))))

(defn- import-row
  [{:import/keys [created-at entity-name entity-exists?] :as imp} page-state busy?]
  ^{:key (str "import-row-" (:id imp))}
  [:tr
   [:td entity-name]
   [:td
    [:span.d-none.d-md-inline (format-date-time created-at)]
    [:span.d-md-none (format-date (:created-at imp) "M/d")]]
   [:td
    [:div.btn-group
     [:button.btn.btn-success.btn-sm {:disabled entity-exists?
                                      :on-click #(start-import imp page-state)
                                      :title "Click here to start the import."}
      (icon :play :size :small)]
     [:button.btn.btn-secondary.btn-sm {:on-click (fn []
                                                (swap! page-state assoc :active imp)
                                                (load-progress page-state)
                                                (reset! auto-refresh true)
                                                (load-import page-state))
                                    :disable busy?
                                    :title "Click here to view this import."}
      (icon :eye :size :small)]
     [:button.btn.btn-danger.btn-sm {:on-click #(when (js/confirm (str "Are you sure you want to delete the import \"" (:entity-name imp) "\"?"))
                                                  (delete-import imp page-state))
                                     :disable busy?
                                     :title "Click here to remove this import."}
      (icon :x-circle :size :small)]]]])

(defn- import-table
  [page-state]
  (let [imports (r/cursor page-state [:imports])]
    (fn []
      [:table.table.table-striped
       [:tbody
        [:tr
         [:th "Entity Name"]
         [:th "Uploaded"]
         [:th (html/space)]]
        (if @imports
          (doall (map #(import-row % page-state (str @busy?)) @imports))
          [:tr [:td.status {:colSpan 3} [:span.inline-status "Loading..."]]])]])))

(defn- refresh-button
  [page-state]
  (let [css-class (make-reaction #(if @auto-refresh "btn-danger" "btn-success"))
        title (make-reaction #(if @auto-refresh
                                "Click here to stop the auto-refresh."
                                "Click here to auto-refresh the page."))
        caption (make-reaction #(if @auto-refresh "Stop" "Auto Refresh"))
        icon-image (make-reaction #(if @auto-refresh :stop :arrow-repeat))]
    (fn []
      [:button.btn
       {:type :button
        :class @css-class
        :title @title
        :on-click #(when (swap! auto-refresh not)
                     (load-import page-state))}
       (icon-with-text @icon-image @caption)])))

(defn- progress-card
  [page-state]
  (fn []
    [:div.card
     [:div.card-header [import-title page-state]]
     [progress-table page-state]
     [:div.card-footer
      [button {:html {:title "Click here to return the list of imports."
                      :class "btn-secondary me-2"
                      :on-click #(swap! page-state dissoc :active)}
               :icon :x
               :caption "Cancel"}]
      [refresh-button page-state]]]))

(defn- notification-elem
  [[{:keys [message severity id]} count]]
  ^{:key (str "simple-notification-" id)}
  [:div.alert
   {:role :alert
    :class (case severity
             ("fatal" :fatal)     "alert-danger"
             ("error" :error)     "alert-warning"
             ("warning" :warning) "alert-secondary"
             "alert-info")}
   message
   (when-not (= 0 count)
     [:span.badge.position-absolute.top-0.start-100.translate-middle.rounded-pill.text-bg-secondary
      count])])

(defn- notifications-card
  [page-state]
  (let [raw-errors (r/cursor page-state [:progress :warnings])
        errors (make-reaction #(->> @raw-errors
                                    (group-by identity)
                                    (map (fn [[m c]]
                                           [{:message m
                                             :severity :error} c]))))
        failure-reason (r/cursor page-state [:progress :failure-reason])
        notifications (make-reaction #(if-let [f @failure-reason]
                                        (cons {:message f
                                               :severity :fatal}
                                              @errors)
                                        @errors))]
    (fn []
      (when (seq @notifications)
        [:div.card.mt-2
         [:div.card-header "Alerts"]
         [:div.card-body
          (->> @notifications
               (map (comp notification-elem
                          #(vector % 1)))
               doall)]]))))

(defn- import-activity
  [page-state]
  (fn []
    [:div
     [progress-card page-state]
     [notifications-card page-state]]))

(defn- start-after-save
  [page-state]
  (fn [result]
    (state/add-entity (:import/entity result))
    (reset! auto-refresh true)
    (swap! page-state #(-> %
                           (dissoc :import-data)
                           (update-in [:imports] conj (:import result))
                           (assoc :active (:import result))))
    (load-import page-state)))

(defn- remove-empty-vals
  [m]
  (->> m
       (filter (fn [[_ v]]
                 (present? v)))
       (into {})))

(defn- save-and-start-import
  [page-state]
  (+busy)
  (-> (:import-data @page-state)
      (dissoc ::v/validation)
      (update-in [:options] remove-empty-vals)
      (imports/create :callback -busy
                      :on-failure (notify/danger-fn "Unable to start the import: %s")
                      :on-success (start-after-save page-state))))

(defn- file-drop
  [import-data]
  (fn [event]
    (.preventDefault event)
    (try
      (swap! import-data (append-dropped-files event))
      (catch js/Error err
        (.error js/console err)))))

(defn- import-form
  [page-state]
  (let [import-data (r/cursor page-state [:import-data])]
    (fn []
      [:form {:no-validate true
              :on-submit (fn [e]
                           (.preventDefault e)
                           (save-and-start-import page-state))}
       [:div.card
        [:div.card-header [:strong "Import Entity"]]
        [:div.card-body
         [text-field import-data [:entity-name] {:validate [:required]}]
         [text-field import-data [:options :lt-capital-gains-account] {:caption "Long-term Capital Gains Account"}]
         [text-field import-data [:options :st-capital-gains-account] {:caption "Short-term Capital Gains Account"}]
         [text-field import-data [:options :lt-capital-loss-account-id] {:caption "Long-term Capital Loss Account"}]
         [text-field import-data [:options :st-capital-loss-account-id] {:caption "Short-term Capital Loss Account"}]
         [:div#import-source.drop-zone.bg-primary.text-light
          {:on-drag-over #(.preventDefault %)
           :on-drop (file-drop import-data)}
          [:div "Drop files here"]]]
        (file-list @import-data)
        [:div.card-footer
         [button {:html {:type :submit
                         :class "btn-success"
                         :title "Click here to begin the import."}
                  :icon :upload
                  :caption "Import"}]
         [button {:html {:on-click #(swap! page-state dissoc :import-data)
                         :class "btn-secondary ms-2"
                         :type :button
                         :title "Click here to discard this import."}
                  :icon :x
                  :caption "Cancel"}]]]])))

(defn- import-list []
  (let [page-state (r/atom {})
        import-data (r/cursor page-state [:import-data])
        active (r/cursor page-state [:active])]
    (load-imports page-state)
    (fn []
      [:<>
       [:h1.mt-3 "Imports"]
       [:div.row
        [:div.col-md-6 {:class (when @import-data "d-none d-md-block")}
         [import-table page-state]
         [button
          {:html {:title "Click here to import a new entity from another system."
                  :class "btn-primary"
                  :on-click (fn []
                              (swap! page-state assoc
                                     :import-data {:options {:lt-capital-gains-account "Investment Income/Long Term Gains"
                                                             :st-capital-gains-account "Investment Income/Short Term Gains"}})
                              (set-focus "entity-name"))}
           :icon :plus
           :caption "Add"}]]
        (when @import-data
          [:div.col-md-6
           [import-form page-state]])
        (when @active
          [:div.col-md-6.mt-2.mt-md-0
           [import-activity page-state]])]])))

(secretary/defroute "/imports" []
  (swap! app-state assoc :page #'import-list))
