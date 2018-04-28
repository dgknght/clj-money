(ns clj-money.web.imports
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [ring.util.response :refer :all]
            [ring.middleware.anti-forgery :refer [*anti-forgery-token*]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [clj-money.web.shared :refer :all]))

(defn new-import
  [req]
  (with-layout "Import entity" {}
    [:script {:src "/js/angular.min.js"}]
    [:script {:src "/js/underscore-min.js"}]
    [:script {:src "/js/import.js"}]
    [:div {:ng-app "clj-money-import"
           :ng-controller "ImportController"
           :ng-init (format "antiForgeryToken = '%s';" *anti-forgery-token*)}
     [:div.row {:ng-hide "alerts.length == 0 && statusMessage == null"}
      [:div.col-md-12
       [:div.alert
        {:ng-repeat "alert in alerts"
         :role "alert"
         :class "alert-{{ alert.level }}"}
        "{{ alert.message }}"]
       [:div.alert.alert-info {:ng-hide "statusMessage == null"}
        "{{ statusMessage }}"]]]
     [:div.row {:ng-hide "activeImport"}
      [:div.col-md-6
       [:form
        [:div.form-group
         [:label.control-label {:for "entity-name"} "Name"]
         [:input.form-control {:type :text
                               :name "entity-name"
                               :id "entity-name"
                               :ng-model "entityName"
                               :autofocus true}]]
        (map
          (fn [index]
            (let [[snake-name
                   camel-name
                   label] (map #(format % index)
                               ["source-file-%s"
                                "sourceFile%s"
                                "Source file %s"])]
              [:div.form-group
               [:label.control-label {:for snake-name}
                label]
               [:input.form-control {:type :file
                                     :file-model camel-name
                                     :name snake-name
                                     :id snake-name}]]))
          (range 10))]
       [:button.btn.btn-primary
        {:title "Click here to upload the specified file and import the data into a new entity."
         :ng-click "startImport();"}
        "Import"]]]
     [:div.row {:ng-hide "activeImport == null"}
      [:div.col-md-6
       [:h2 "{{ activeImport['entity-name'] }}" ]
       [:table.table.table-striped
        [:tr
         [:th.col-sm-3 "Record Type"]
         [:th.col-sm-3.text-right "Total"]
         [:th.col-sm-3.text-right "Imported"]
         [:th.col-sm-3.text-center "Progress"]]
        [:tr {:ng-repeat "(recordType, stats) in activeImport.progress"}
         [:td.col-sm-3 "{{ recordType }}"]
         [:td.col-sm-3.text-right "{{ stats.total }}"]
         [:td.col-sm-3.text-right "{{ stats.imported }}"]
         [:td.col-sm-3.text-center
          [:div.progress-bar {:id "progress-{{ recordType }}" :style "width: 100%;"}]]]]]]]))
