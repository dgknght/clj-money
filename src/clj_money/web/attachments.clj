(ns clj-money.web.attachments
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [ring.util.codec :refer [url-encode]]
            [clj-money.pagination :as pagination]
            [clj-money.validation :as validation]
            [clj-money.models.attachments :as attachments])
  (:use [clj-money.web.shared :refer :all]))

(defn- attachment-row
  [attachment]
  [:tr
   [:td (:caption attachment)]
   [:td "&nbsp"]])

(defn index
  [{{transaction-id :transaction-id} :params}]
  (let [attachments (attachments/search
                      (env :db)
                      {:transaction-id (Integer. transaction-id)})]
    (with-layout "Attachments" {}
      [:div.row
       [:div.col-md-6
        [:table.table.table-striped
         [:tr
          [:th "Caption"]
          [:th "&nbsp;"]]
         (map attachment-row attachments)]]]
      [:a.btn.btn-primary {:href (format "/transactions/%s/attachments/new" transaction-id)}
       "Add"])))

(defn new-attachment
  [{{transaction-id :transaction-id} :params}]
  (with-layout "New attachment" {}
    [:div.row
     [:div.col-md-6
      [:form {:action (format "/transactions/%s/attachments" transaction-id)
              :method :post}
       [:div.form-group
        [:label.control-label {:for "attachment-caption"} "Caption"]
        [:input#attachment-caption.form-control {:type :text
                                                 :name :caption
                                                 :autofocus true}]]
       [:div.form-group
        [:label.control-label {:for "attachment-file"} "Attachment file"]
        [:input#source-file.form-control {:type :file
                                          :name "source-file"}]]
       [:button.btn.btn-primary {:type :submit}
        "Submit"]]]]))

(defn create
  [req]
  "create")

(defn edit
  [req]
  "edit")

(defn update
  [req]
  "update")

(defn delete
  [req]
  "delete")
