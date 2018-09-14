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

(defn- append-dropped-files
  [event import-data]
  (let [file-list (-> event .-dataTransfer .-files)
        file-count (.-length file-list)
        files (mapv #(.item file-list %) (range file-count))]
    (update-in import-data [:files] #(concat % files))))

(def ^:private import-form
  [:form
   (text-input :name :required)])

(defn- file-list
  [import-data]
  (when (not (empty? (:files @import-data)))
    [:section
     [:h2 "Files"]
     [:ul.list-group
      (for [file (:files @import-data)]
        ^{:key (.-name file)}
        [:li.list-group-item (.-name file)])]]))

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
                      (try
                        (swap! import-data #(append-dropped-files e %))
                        (catch js/Error err
                          (.log js/console "Error: " (prn-str err))))
                      (.preventDefault e))}
          [:div "Drop files here"]]
         (util/button "Import"
                      (fn [e]
                        (.log js/console "start the import")
                        (imports/create @import-data
                                        #(.log js/console "started " (prn-str %))
                                        notify/danger)
                        (.preventDefault e))
                      {:class "btn btn-primary"
                       :icon :ok})]
        [:div.col-md-6
         [file-list import-data]]]])))

(secretary/defroute new-import-path "/import/new" []
  (r/render [new-import] (app-element)))
