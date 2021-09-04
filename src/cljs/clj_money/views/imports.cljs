(ns clj-money.views.imports
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [secretary.core :as secretary :include-macros true]
            [cljs.core.async :refer [timeout
                                     <!]]
            [dgknght.app-lib.core :refer [trace]]
            [dgknght.app-lib.web :refer [format-percent
                                         format-date-time]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.notifications :as notify]
            [dgknght.app-lib.forms :refer [text-field]]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [dgknght.app-lib.busy :refer [busy +busy -busy]]
            [clj-money.views.util :refer [handle-error]]
            [clj-money.dnd :as dnd]
            [clj-money.state :as state :refer [app-state]]
            [clj-money.api.imports :as imports]))

(defn- load-imports
  [page-state]
  (+busy page-state)
  (imports/select (fn [result]
                    (swap! page-state #(-> %
                                           -busy
                                           (assoc :imports result))))
                  (handle-error page-state "Unable to load the imports: %s")))

(defn- delete-import
  [imp page-state]
  (+busy page-state)
  (imports/delete imp
                  (fn []
                    (-busy page-state)
                    (load-imports page-state))
                  (handle-error page-state "Unable to delete the import: s")))

(defn- append-dropped-files
  [event import-data]
  (update-in import-data [:files] #(concat % (dnd/files event))))

(defn- file-list
  [import-data]
  (when (seq (:files @import-data))
    [:ul.list-group.list-group-flush
     (for [file (:files @import-data)]
       ^{:key (.-name file)}
       [:li.list-group-item (.-name file)])]))

(defn- import-title
  [page-state]
  (let [imp (r/cursor page-state [:active])]
    (fn []
      [:strong (str "Import " (:entity-name @imp))])))

(defn- progress-row
  [[progress-type {:keys [total imported]}]]
  ^{:key (str "progress-" (name progress-type))}
  [:tr
   [:td (name progress-type)]
   [:td.text-center
    (let [perc (* 100 (/ imported total))]
      [:div.progress
       [:div.progress-bar
        {:aria-valuenow imported
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
  (let [progress (r/cursor page-state [:active :progress])]
    (fn []
      [:table.table.table-hover
       [:tbody
        [:tr
         [:th "Record Type"]
         [:th.text-center "Progress"]]
        (->> @progress
             (filter #(map? (second %)))
             (map progress-row)
             doall)]])))

(def auto-refresh (r/atom false))

(declare load-import)
(defn- receive-import
  [{{:keys [errors finished]} :progress :as received} page-state]
  (swap! page-state #(assoc % :active received))
  (when (seq errors)
    (trace {:errors errors}))
  (when finished
    (reset! auto-refresh false))
  (when @auto-refresh
    (go
      (<! (timeout 1000))
      (load-import page-state))))

(defn- load-import
  [page-state]
  (imports/get (get-in @page-state [:active :id])
               #(receive-import % page-state)
               (notify/danger-fn "Unable to load the import: %s")))

(defn- start-import
  [imp page-state]
  (+busy page-state)
  (imports/start imp
                 (fn []
                   (reset! auto-refresh true)
                   (swap! page-state #(-> %
                                          -busy
                                          (assoc :active imp))))
                 notify/danger))

(defn- import-row
  [imp page-state busy?]
  ^{:key (str "import-row-" (:id imp))}
  [:tr
   [:td (:entity-name imp)]
   [:td (format-date-time (:created-at imp))]
   [:td
    [:div.btn-group
     [:button.btn.btn-success.btn-sm {:disabled (:entity-exists? imp)
                                      :on-click #(start-import imp page-state)
                                      :title "Click here to start the import."}
      (bs/icon :play {:size :small})]
     [:button.btn.btn-light.btn-sm {:on-click (fn []
                                                (swap! page-state assoc :active imp)
                                                (reset! auto-refresh true)
                                                (load-import page-state))
                                    :disable busy?
                                    :title "Click here to view this import."}
      (bs/icon :eye {:size :small})]
     [:button.btn.btn-danger.btn-sm {:on-click #(when (js/confirm (str "Are you sure you want to delete the import \"" (:entity-name imp) "\"?"))
                                                  (delete-import imp page-state))
                                     :disabled busy?
                                     :title "Click here to remove this import."}
      (bs/icon :x-circle {:size :small})]]]])

(defn- import-table
  [page-state]
  (let [imports (r/cursor page-state [:imports])
        busy? (busy page-state)]
    (fn []
      [:table.table.table-striped
       [:tbody
        [:tr
         [:th "Entity Name"]
         [:th "Uploaded On"]
         [:th (html/space)]]
        (if @imports
          (doall (map #(import-row % page-state @busy?) @imports))
          [:tr [:td.status {:colSpan 3} [:span.inline-status "Loading..."]]])]])))

(defn- refresh-button
  [page-state]
  (let [busy? (busy page-state)
        css-class (make-reaction #(if @auto-refresh "btn-danger" "btn-success"))
        title (make-reaction #(if @auto-refresh
                                "Click here to stop the auto-refresh."
                                "Click here to auto-refresh the page."))
        icon-image (make-reaction #(if @auto-refresh :stop :arrow-repeat))]
    (fn []
      ; TODO: either don't use busy-button, or change it to allow class, title, and icon to change
      [bs/busy-button {:html {:class @css-class
                              :title @title
                              :on-click (fn []
                                          (swap! auto-refresh not)
                                          (when @auto-refresh
                                            (load-import page-state)))}
                       :icon @icon-image
                       :busy? busy?}])))

(defn- progress-card
  [page-state]
  (fn []
    [:div.card
     [:div.card-header [import-title page-state]]
     [progress-table page-state]
     [:div.card-footer
      [:button.btn.btn-light.me-2 {:title "Click here to return the list of imports."
                                   :on-click #(swap! page-state dissoc :active)}
       (bs/icon-with-text :x "Cancel")]
      [refresh-button page-state]]]))

(defn- errors-card
  [page-state]
  (let [errors (r/cursor page-state [:active :progress :errors])]
    (fn []
      (when (seq @errors)
        [:div.card
         [:div.card-header "Errors"]
         [:div.card-body
          (->> @errors
               (map-indexed (fn [index error]
                              ^{:key (str "import-error-" index)}
                              [:div.alert.alert-danger (:message error)])))]]))))

(defn- import-activity
  [page-state]
  (fn []
    [:div
     [progress-card page-state]
     [errors-card page-state]]))

(defn- import-click
  [event page-state]
  (.preventDefault event)
  (+busy page-state)
  (imports/create (get-in @page-state [:import-data])
                  (fn [result]
                    (state/add-entity (:entity result))
                    (reset! auto-refresh true)
                    (swap! page-state #(-> %
                                           -busy
                                           (dissoc :import-data)
                                           (update-in [:imports] conj (:import result))
                                           (assoc :active (:import result))))
                    (load-import page-state))
                  (handle-error page-state "Unable to create the import: %s")))

(defn- file-drop
  [import-data event]
  (.preventDefault event)
  (try
    (swap! import-data #(append-dropped-files event %))
    (catch js/Error err
      (.log js/console "Error: " (prn-str err)))))

(defn- import-form
  [page-state]
  (let [import-data (r/cursor page-state [:import-data])
        busy? (busy page-state)]
    [:div.card
     [:div.card-header [:strong "Import Entity"]]
     [:div.card-body
      [:form
       [text-field import-data [:entity-name] {:validate [:required]}]
       [text-field import-data [:options :lt-capital-gains-account-id] {:caption "Long-term Capital Gains Account"}]
       [text-field import-data [:options :st-capital-gains-account-id] {:caption "Short-term Capital Gains Account"}]
       [text-field import-data [:options :lt-capital-loss-account-id] {:caption "Long-term Capital Loss Account"}]
       [text-field import-data [:options :st-capital-loss-account-id] {:caption "Short-term Capital Loss Account"}]]
      [:div#import-source.drop-zone.bg-primary.text-light
       {:on-drag-over #(.preventDefault %)
        :on-drop #(file-drop import-data %)}
       [:div "Drop files here"]]]
     [file-list import-data]
     [:div.card-footer
      [bs/busy-button {:html {:on-click #(import-click % page-state)
                              :class "btn-success"
                              :title "Click here to begin the import."}
                       :busy? busy?
                       :icon :file-arrow-up
                       :caption "Import"}]

      [bs/busy-button {:html {:on-click #(swap! page-state dissoc :import-data)
                              :class "btn-secondary ms-2"
                              :title "Click here to discard this import."}
                       :busy? busy?
                       :icon :x
                       :caption "Cancel"}]]]))

(defn- import-list []
  (let [page-state (r/atom {})
        import-data (r/cursor page-state [:import-data])
        active (r/cursor page-state [:active])
        busy? (busy page-state)]
    (load-imports page-state)
    (fn []
      [:div.row.mt-5
       [:div.col-md-6
        [:h1 "Imports"]
        [import-table page-state]
        [bs/busy-button {:html {:title "Click here to import a new entiry from another system."
                                :class "btn-primary"
                                :on-click (fn []
                                            (swap! page-state assoc
                                                   :import-data {:user-id (:id @state/current-user)})
                                            (html/set-focus "entity-name"))}
                         :busy? busy?
                         :icon :plus
                         :caption "Add"}]]
       (when @import-data
         [:div.col-md-6
          [import-form page-state]])
       (when @active
         [:div.col-md-6
          [import-activity page-state]])])))

(secretary/defroute "/imports" []
  (swap! app-state assoc :page #'import-list))
