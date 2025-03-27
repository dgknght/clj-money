(ns clj-money.views.imports
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.pprint :refer [pprint]]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [secretary.core :as secretary :include-macros true]
            [cljs.core.async :refer [timeout
                                     <!]]
            [dgknght.app-lib.web :refer [format-percent
                                         format-date
                                         format-date-time]]
            [dgknght.app-lib.dom :refer [set-focus]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.forms :refer [text-field]]
            [dgknght.app-lib.notifications :as notify]
            [dgknght.app-lib.forms-validation :as v]
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
                  :on-success #(swap! page-state assoc :imports %)))

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

(defn- progress-row
  [[progress-type {:keys [total completed]}]]
  ^{:key (str "progress-" (name progress-type))}
  [:tr
   [:td (name progress-type)]
   [:td.text-center
    (let [perc (if (< 0 total)
                 (* 100 (/ completed total))
                 0)]
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
          (format-percent (/ perc 100)))]
       (when (> 50 perc)
         [:span.ps-1
          (format-percent (/ perc 100))])])]])

(defn- progress-table
  [page-state]
  (let [progress (r/cursor page-state [:active :import/progress])]
    (fn []
      [:table.table.table-hover
       [:tbody
        [:tr
         [:th "Record Type"]
         [:th.text-center "Progress"]]
        (->> @progress
             (filter (comp map? second))
             (map progress-row)
             doall)]])))

(def auto-refresh (r/atom false))

(declare load-import)

(defn- receive-import
  ([page-state] (partial receive-import page-state))
  ([page-state {{:keys [finished]} :import/progress
                :as received}]
   (when finished
     (reset! auto-refresh false))
   (when @auto-refresh
     (go
       (<! (timeout 1000))
       (load-import page-state)))
   (swap! page-state assoc :active received)))

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

(defn- errors-card
  [page-state]
  (let [notifications (r/cursor page-state [:active :import/progress :notifications])]
    (fn []
      (when (seq @notifications)
        [:div.card
         [:div.card-header "Alerts"]
         [:div.card-body
          (->> @notifications
               (take 20)
               (map-indexed (fn [index n]
                              ^{:key (str "import-error-" index)}
                              [:div.alert
                               {:role :alert
                                :class (case (:notification/severity n)
                                        "error" "alert-danger"
                                         "warning" "alert-warning"
                                         "alert-info")}
                               (:notification/message n)]))) ]]))))

(defn- import-activity
  [page-state]
  (fn []
    [:div
     [progress-card page-state]
     [:div.mt-2
      [errors-card page-state]]]))

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

(defn- save-and-start-import
  [event page-state]
  (.preventDefault event)
  (+busy)
  (try
    (imports/create (dissoc (get-in @page-state [:import-data]) ::v/validation)
                    :callback -busy
                    :on-success (start-after-save page-state))
    (catch js/Error e
      (.dir js/console e)
      (notify/danger (str "Unable to start the import: " (.-name e) " - " (.-message e))))))

(defn- file-drop
  [import-data]
  (fn [event]
    (.preventDefault event)
    (try
      (swap! import-data (append-dropped-files event))
      (catch js/Error err
        (.error js/console err)))))

(defn- present?
  [{:keys [user-id]}]
  (not (nil? user-id)))

(defn- import-form
  [page-state]
  (let [import-data (r/cursor page-state [:import-data])]
    (when (present? @import-data)
      [:form {:no-validate true
              :on-submit #(save-and-start-import % page-state)}
       [:div.card
        [:div.card-header [:strong "Import Entity"]]
        [:div.card-body
         [text-field import-data [:entity-name] {:validate [:required]}]
         [text-field import-data [:options :lt-capital-gains-account-id] {:caption "Long-term Capital Gains Account"}]
         [text-field import-data [:options :st-capital-gains-account-id] {:caption "Short-term Capital Gains Account"}]
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
  (let [page-state (r/atom {:import-data {:options {:lt-capital-gains-account-id "Investment Income/Long Term Gains"
                                                    :st-capital-gains-account-id "Investment Income/Short Term Gains"}}})
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
                                     :import-data {:user-id (:id @state/current-user)
                                                   :options {:lt-capital-gains-account-id "Investment Income/Long Term Gains"
                                                             :st-capital-gains-account-id "Investment Income/Short Term Gains"}})
                              (set-focus "entity-name"))}
           :icon :plus
           :caption "Add"}]]
        (when (present? @import-data)
          [:div.col-md-6
           [import-form page-state]])
        (when @active
          [:div.col-md-6.mt-2.mt-md-0
           [import-activity page-state]])]])))

(secretary/defroute "/imports" []
  (swap! app-state assoc :page #'import-list))
