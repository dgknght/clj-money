(ns clj-money.views.imports
  (:require [reagent.core :as r]
            [reagent-forms.core :refer [bind-fields]]
            [secretary.core :as secretary :include-macros true]
            [clj-money.util :as util]
            [clj-money.api.entities :as entities]
            [clj-money.api.imports :as imports]
            [clj-money.notifications :as notify]
            [clj-money.dom :refer [app-element]]
            [clj-money.layout :refer [with-layout]]
            [clj-money.forms :refer [text-input
                                     radio-buttons
                                     required]]))

(defn- import-row
  [imp]
  ^{:key (str "import-row-" (:id imp))}
  [:tr
   [:td (:entity-name imp)]
   [:td (util/format-date-time (:created-at imp))]
   [:td
    (util/link-to nil
                  (util/path :imports (:id imp))
                  {:icon :eye-open
                   :class "btn btn-info btn-xs"
                   :title "Click here to view this import."})]])

(defn- import-table
  [imports]
  [:table.table.table-striped
   [:tbody
   [:tr
    [:th "Entity Name"]
    [:th "Uploaded On"]
    [:th (util/space)]]
   (if (empty? @imports)
     [:tr [:td {:colspan 3} "Loading..."]]
     (map import-row @imports))]])

(defn- import-list []
  (let [imports (r/atom [])]
    (imports/get-all #(reset! imports %)
                     notify/danger)
    (with-layout
      [:div.row
       [:div.col-md-6
        [:h1 "Imports"]
        [import-table imports]]])))

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
  [imp]
  [:h1 (str "Import " (:entity-name @imp))])

(defn- progress-table
  [imp]
  [:table.table.table-striped
   [:tbody
    [:tr
     [:th.col-sm-3 "Record Type"]
     [:th.col-sm-3.text-right "Total"]
     [:th.col-sm-3.text-right "Imported"]
     [:th.col-sm-3.text-center "Progress"]]
    [:tr
     [:td.col-sm-3 "Accounts"]
     [:td.col-sm-3.text-right [:pre (prn-str (js-keys (:progress @imp)))]]
     [:td.col-sm-3.text-right "imported"]
     [:td.col-sm-3.text-center
      "progress bar here"
      [:div.progress-bar {:style {:width "100%"}}]]]]])

(defn- show-import
  [id]
  (let [imp (r/atom {})]
    (imports/get-one id
                     #(reset! imp %)
                     notify/danger)
    (with-layout
      [:section
       [:div.row
        [:div.col-md-6
         [import-title imp]
         [progress-table imp]
         [:p
          (util/link-to "Back" "/imports" {:class "btn btn-primary"})]]]])))

(defn- new-import []
  (let [import-data (r/atom {})]
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
           :on-drop (fn [e]
                      (.preventDefault e)
                      (try
                        (swap! import-data #(append-dropped-files e %))
                        (catch js/Error err
                          (.log js/console "Error: " (prn-str err)))))}
          [:div "Drop files here"]]
         (util/button "Import"
                      (fn [e]
                        (.preventDefault e)
                        (imports/create @import-data
                                        #(secretary/dispatch! (str "/imports/" (:id %)))
                                        notify/danger))
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
