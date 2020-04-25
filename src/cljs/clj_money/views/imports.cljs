(ns clj-money.views.imports
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r]
            [secretary.core :as secretary :include-macros true]
            [cljs.core.async :refer [timeout
                                     <!]]
            [clj-money.bootstrap :as bs]
            [clj-money.util :as util]
            [clj-money.state :as state :refer [app-state]]
            [clj-money.api.imports :as imports]
            [clj-money.notifications :as notify]
            [clj-money.plain-forms :refer [text-field]]))

(defn- load-imports
  [page-state]
  (imports/get-all #(swap! page-state assoc :imports %)
                   (notify/danger-fn "Unable to load the imports: %s")))

(defn- delete-import
  [imp page-state]
  (imports/delete imp
                  #(load-imports page-state)
                  notify/danger))

(defn- append-dropped-files
  [event import-data]
  (let [file-list (-> event .-dataTransfer .-files)
        file-count (.-length file-list)
        files (mapv #(.item file-list %) (range file-count))]
    (update-in import-data [:files] #(concat % files))))

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
    [:div.progress
     (let [perc (* 100 (/ imported total))]
       [:div.progress-bar.text-center {:aria-valuenow imported
                                       :aria-valuemax total
                                       :aria-valuemin 0
                                       :role "progressbar"
                                       :class (cond
                                                (> perc 100)
                                                "progress-bar-danger progress-bar-striped active"
                                                (= perc 100)
                                                "progress-bar-info"
                                                :else
                                                "progress-bar-info progress-bar-striped active")
                                       :style {"width" (str (if (> perc 100)
                                                              100
                                                              perc)
                                                            "%")}}
        (util/format-percent perc)])]]])

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
             (map progress-row))]])))

(def auto-refresh (r/atom false))

(declare load-import)
(defn- receive-import
  [{{:keys [error finished]} :progress :as received} page-state]
  (swap! page-state assoc :active received)
  (when error
    (notify/danger (:message error))
    (.log js/console "error" (prn-str error)))
  (when (or finished
            error)
    (reset! auto-refresh false))
  (when @auto-refresh
    (go
      (<! (timeout 1000))
      (load-import page-state))))

(defn- load-import
  [page-state]
  (imports/get-one (get-in @page-state [:active :id])
                   #(receive-import % page-state)
                   notify/danger))

(defn- start-import
  [imp page-state]
  (imports/start imp
                 (fn []
                   (reset! auto-refresh true)
                   (swap! page-state assoc :active imp))
                 notify/danger))

(defn- import-row
  [imp page-state]
  ^{:key (str "import-row-" (:id imp))}
  [:tr
   [:td (:entity-name imp)]
   [:td (util/format-date-time (:created-at imp))]
   [:td
    [:div.btn-group
     [:button.btn.btn-success.btn-sm {:disabled (:entity-exists? imp)
                                      :on-click #(start-import imp page-state)
                                      :title "Click here to start the import."}
      (bs/icon :play)]
     [:button.btn.btn-info.btn-sm {:on-click (fn []
                                               (swap! page-state assoc :active imp)
                                               (reset! auto-refresh true)
                                               (load-import page-state))
                                   :title "Click here to view this import."}
      (bs/icon :eye)]
     [:button.btn.btn-danger.btn-sm {:on-click #(when (js/confirm (str "Are you sure you want to delete the import \"" (:entity-name imp) "\"?"))
                                                  (delete-import imp page-state))
                                     :title "Click here to remove this import."}
      (bs/icon :x-circle)]]]])

(defn- import-table
  [page-state]
  (let [imports (r/cursor page-state [:imports])]
    (fn []
      [:table.table.table-striped
       [:tbody
        [:tr
         [:th "Entity Name"]
         [:th "Uploaded On"]
         [:th (util/space)]]
        (if @imports
          (doall (map #(import-row % page-state) @imports))
          [:tr [:td.status {:colSpan 3} [:span.inline-status "Loading..."]]])]])))

(defn- refresh-button
  [page-state]
  (let [attr (if @auto-refresh
                  {:class "btn-danger"
                   :title "Click here to stop the auto-refresh."}
                  {:class "btn-success"
                   :title "Click here to auto-refresh the page."})]
    [:button.btn (assoc attr :on-click (fn []
                                         (swap! auto-refresh not)
                                         (when @auto-refresh
                                           (load-import page-state))))
     (bs/icon (if @auto-refresh
                :stop
                :arrow-repeat))]))

(defn- import-activity
  [page-state]
  (fn []
    [:div.card
     [:div.card-header [import-title page-state]]
     [progress-table page-state]
     [:div.card-footer
      [:button.btn.btn-light {:title "Click here to return the list of imports."
                              :on-click #(swap! page-state dissoc :active)}
       (bs/icon-with-text :x "Cancel")]
      (util/space)
      [refresh-button page-state]]]))

(defn- import-click
  [event page-state]
  (.preventDefault event)
  (imports/create (get-in @page-state [:import-data])
                  (fn [result]
                    (state/add-entity (:entity result))
                    (reset! auto-refresh true)
                    (swap! page-state #(-> %
                                           (dissoc :import-data)
                                           (update-in [:imports] conj (:import result))
                                           (assoc :active (:import result))))
                    (load-import page-state))
                  notify/danger))

(defn- file-drop
  [import-data event]
  (.preventDefault event)
  (try
    (swap! import-data #(append-dropped-files event %))
    (catch js/Error err
      (.log js/console "Error: " (prn-str err)))))

(defn- import-form
  [page-state]
  (let [import-data (r/cursor page-state [:import-data])]
    [:div.card
     [:div.card-header [:strong "Import Entity"]]
     [:div.card-body
      [:form
       [text-field import-data [:entity-name] {:validate [:required]}]]
      [:div#import-source.drop-zone.bg-primary.text-light
       {:on-drag-over #(.preventDefault %)
        :on-drop #(file-drop import-data %)}
       [:div "Drop files here"]]]
     [file-list import-data]
     [:div.card-footer
      [:button.btn.btn-success {:on-click #(import-click % page-state)
                                :title "Click here to begin the import."}
       (bs/icon-with-text :file-arrow-up "Import")]
      (util/space)
      [:button.btn.btn-danger {:on-click #(swap! page-state dissoc :import-data)
                               :title "Click here to discard this import."}
       (bs/icon-with-text :x "Cancel")]]]))

(defn- import-list []
  (let [page-state (r/atom {})
        import-data (r/cursor page-state [:import-data])
        active (r/cursor page-state [:active])]
    (load-imports page-state)
    (fn []
      [:div.row.mt-5
       [:div.col-md-6
        [:h1 "Imports"]
        [import-table page-state]
        [:button.btn.btn-primary {:title "Click here to import a new entiry from another system."
                                  :on-click (fn []
                                              (swap! page-state assoc
                                                     :import-data {:user-id (:id @state/current-user)})
                                              (util/set-focus "entity-name"))}
         (bs/icon-with-text :plus "Add")]]
       (when @import-data
         [:div.col-md-6
          [import-form page-state]])
       (when @active
         [:div.col-md-6
          [import-activity page-state]])])))

(secretary/defroute "/imports" []
  (swap! app-state assoc :page #'import-list))
