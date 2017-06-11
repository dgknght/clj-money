(ns clj-money.web.imports
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [ring.util.response :refer :all]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [clj-money.web.shared :refer :all]))

(defn new-import
  [req]
  (with-layout "Import entity" {}
    [:script {:src "/js/angular.min.js"}]
    [:script {:src "/js/import.js"}]
    [:div {:ng-app "clj-money-import" :ng-controller "ImportController"}
     [:div.row {:ng-hide "activeImport"}
      [:div.col-md-6
       [:form {:action "/entities/import"
               :method :post
               :enctype "multipart/form-data"}
        [:div.form-group
         [:label.control-label {:for "entity-name"} "Name"]
         [:input.form-control {:type :text
                               :name "entity-name"
                               :id "entity-name"
                               :ng-model "entityName"
                               :autofocus true}]]
        [:div.form-group
         [:label.control-label {:for "source-file"} "Source file"]
         [:input.form-control {:type :file
                               :file-model "sourceFile"
                               :name "source-file"
                               :id "source-file"}]]]
       [:button.btn.btn-primary
        {:title "Click here to upload the specified file and import the data into a new entity."
         :ng-click "startImport();"
         }
        "Import"]]]
     [:div.row {:ng-hide "activeImport == null"}
      [:div.col-md-6
       [:h2
        "Importing&nbsp;"
        [:em "{{ activeImport['entity-name'] }}"] ]
       [:table.table.table-striped
        [:tr
         [:th "Record Type"]
         [:th "Total"]
         [:th "Imported"] ]
        [:tr {:ng-repeat "entry in activeImport['record-counts']"}
         [:td "{{ entry['record-type'] }}"]
         [:td "{{ entry['record-count'] }}"]
         [:td "{{ entry['imported-count'] }}"]]]]]]))
