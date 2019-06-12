(ns clj-money.views.imports
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as r]
            [reagent-forms.core :refer [bind-fields]]
            [secretary.core :as secretary :include-macros true]
            [cljs.core.async :refer [timeout
                                     <!]]
            [clj-money.util :as util]
            [clj-money.state :as state]
            [clj-money.api.entities :as entities]
            [clj-money.api.imports :as imports]
            [clj-money.notifications :as notify]
            [clj-money.dom :refer [app-element]]
            [clj-money.layout :refer [with-layout]]
            [clj-money.forms :refer [text-input
                                     radio-buttons
                                     required]]))

(defn- delete-import
  [imp]
  (imports/delete imp
                  #(secretary/dispatch! "/imports")
                  notify/danger))

(defn- append-dropped-files
  [event import-data]
  (let [file-list (-> event .-dataTransfer .-files)
        file-count (.-length file-list)
        files (mapv #(.item file-list %) (range file-count))]
    (update-in import-data [:files] #(concat % files))))

(def ^:private import-form
  [:form
   (text-input :entity-name :required)])

(defn- file-list
  [import-data]
  (when (not (empty? (:files @import-data)))
    [:section
     [:h2 "Files"]
     [:ul.list-group
      (for [file (:files @import-data)]
        ^{:key (.-name file)}
        [:li.list-group-item (.-name file)])]]))

(defn- import-title
  [import-ref]
  [:h1 (str "Import " (:entity-name @import-ref))])

(defn- progress-row
  [[progress-type {:keys [total imported]}]]
  ^{:key (str "progress-" (name progress-type))}
  [:tr
     [:td.col-sm-3 (name progress-type)]
     [:td.col-sm-9.text-center
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
  [import-ref]
  [:table.table.table-striped
   [:tbody
    [:tr
     [:th.col-sm-3 "Record Type"]
     [:th.col-sm-9.text-center "Progress"]]
    (map progress-row (:progress @import-ref))]])

(def auto-refresh (r/atom false))

(defn- start-import
  [{id :id :as imp}]
  (imports/start imp
                 (fn []
                   (reset! auto-refresh true)
                   (secretary/dispatch! (str "/imports/" id)))
                 notify/danger))

(defn- import-row
  [imp]
  ^{:key (str "import-row-" (:id imp))}
  [:tr
   [:td (:entity-name imp)]
   [:td (util/format-date-time (:created-at imp))]
   [:td
    [:div.btn-group
     (util/button nil
                  #(start-import imp)
                  {:icon :play
                   :disabled (:entity-exists? imp)
                   :class "btn btn-success btn-xs"
                   :title "Click here to start the import."})
     (util/link-to nil
                   (util/path :imports (:id imp))
                   {:icon :eye-open
                    :class "btn btn-info btn-xs"
                    :title "Click here to view this import."})
     (util/button nil
                  #(when (js/confirm (str "Are you sure you want to delete the import \"" (:entity-name imp) "\"?"))
                      (delete-import imp))
                  {:icon :remove
                   :class "btn btn-danger btn-xs"
                   :title "Click here to remove this import."})]]])

(defn- import-table
  [imports]
  [:table.table.table-striped
   [:tbody
   [:tr
    [:th "Entity Name"]
    [:th "Uploaded On"]
    [:th (util/space)]]
   (if @imports
     (map import-row @imports)
     [:tr [:td.status {:colSpan 3} [:span.inline-status "Loading..."]]])]])

(defn- import-list []
  (let [imports (r/atom nil)]
    (imports/get-all #(reset! imports %)
                     notify/danger)
    (with-layout
      [:div.row
       [:div.col-md-6
        [:h1 "Imports"]
        [import-table imports]
        (util/link-to "Add" "/imports/new" {:class "btn btn-primary"
                                            :title "Click here to import a new entity from another system."}) ]])))

(declare load-import)
(defn- receive-import
  [import-ref {{:keys [transaction error finished]} :progress :as received}]
  (reset! import-ref received)
  (when error
    (notify/danger (:message error))
    (.log js/console "error" (prn-str error)))
  (when (or finished
            error
            (and (:total transaction)
                 (not= 0 (:total transaction))
                 (= (:total transaction)
                    (:imported transaction))))
    (reset! auto-refresh false))
  (when @auto-refresh
    (go
      (<! (timeout 1000))
      (load-import (:id received) import-ref))))

(defn- load-import
  [id import-ref]
  (imports/get-one id
                   #(receive-import import-ref %)
                   notify/danger))

(defn- refresh-button
  [id import-ref]
  (let [options (if @auto-refresh
                  {:icon :stop
                  :class "btn btn-danger"
                  :title "Click here to stop the auto-refresh."}
                  {:icon :refresh
                  :class "btn btn-success"
                  :title "Click here to auto-refresh the page."})]
    (util/button nil
                 (fn []
                   (swap! auto-refresh not)
                   (when @auto-refresh
                     (load-import id import-ref)))
                 options)))

(defn- show-import
  [id]
  (let [imp (r/atom {})]
    (load-import id imp)
    (with-layout
      [:section
       [:div.row
        [:div.col-md-6
         [import-title imp]
         [progress-table imp]
         [:p
          (util/link-to "Back" "/imports" {:class "btn btn-primary"})
          (util/space)
          [refresh-button id imp]]]]])))

(defn- import-click
  [import-data event]
  (.preventDefault event)
  (imports/create @import-data
                  (fn [result]
                    (state/add-entity (:entity result))
                    (reset! auto-refresh true)
                    (secretary/dispatch! (str "/imports/" (-> result :import :id))))
                  notify/danger))

(defn- file-drop
  [import-data event]
  (.preventDefault event)
  (try
    (swap! import-data #(append-dropped-files event %))
    (catch js/Error err
      (.log js/console "Error: " (prn-str err)))))

(defn- new-import []
  (let [import-data (r/atom {})]
    (util/set-focus "entity-name")
    (with-layout
      [:section
       [:div.row
        [:div.col-md-6
         [:h1 "Import Entity"]]]
       [:div.row
        [:div.col-md-6
         [bind-fields import-form import-data]
         [:div#import-source.drop-zone.bg-primary
          {:on-drag-over #(.preventDefault %)
           :on-drop #(file-drop import-data %)}
          [:div "Drop files here"]]
         (util/button "Import"
                      #(import-click import-data %)
                      {:class "btn btn-primary"
                       :icon :ok})]
        [:div.col-md-6
         [file-list import-data]]]])))

(secretary/defroute imports-path "/imports" []
  (r/render [import-list] (app-element)))

(secretary/defroute new-import-path "/imports/new" []
  (r/render [new-import] (app-element)))

(secretary/defroute import-path "/imports/:id" [id]
  (r/render [show-import id] (app-element)))
