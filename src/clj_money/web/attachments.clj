(ns clj-money.web.attachments
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clojure.string :refer [blank?]]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [ring.util.codec :refer [url-encode]]
            [cemerick.friend :as friend]
            [clj-money.io :refer [read-bytes]]
            [clj-money.pagination :as pagination]
            [clj-money.validation :as validation]
            [clj-money.models.images :as images] 
            [clj-money.models.attachments :as attachments])
  (:use [clj-money.web.shared :refer :all]))

(defn- attachment-row
  [attachment]
  [:tr
   [:td
    [:a {:href (format "/images/%s" (:image-id attachment))
         :target "_blank"}
     (:caption attachment)]]
   [:td.text-center
    [:div.btn-group
     [:a.btn.btn-xs.btn-danger
      {:href (format "/attachments/%s/delete" (:id attachment))
       :title "Click here to remove this attachment"
       :data-method "POST"
       :data-confirm "Are you sure you want to remove this attachment?" }
      [:span.glyphicon.glyphicon-remove {:aria-hidden true}]]]]])

(defn index
  [{{transaction-id :transaction-id} :params}]
  (let [attachments (attachments/search
                      (env :db)
                      {:transaction-id (Integer. transaction-id)})]
    (with-layout "Attachments" {}
      [:div.row
       [:div.col-md-4
        [:table.table.table-striped
         [:tr
          [:th "Caption"]
          [:th "&nbsp;"]]
         (map attachment-row attachments)]]]
      [:a.btn.btn-primary {:href (format "/transactions/%s/attachments/new" transaction-id)}
       "Add"])))

(defn new-attachment
  ([{{transaction-id :transaction-id} :params :as req}]
   (new-attachment req {:transaction-id (Integer. transaction-id)}))
  ([_ attachment]
   (with-layout "New attachment" {}
     [:div.row
      [:div.col-md-6
       [:pre (prn-str attachment)]
       [:form {:action (format "/transactions/%s/attachments"
                               (:transaction-id attachment))
               :method :post
               :enctype "multipart/form-data"}
        (text-input-field attachment :caption {:autofocus true})
        (file-input-field attachment :source-file)
        [:button.btn.btn-primary {:type :submit}
         "Submit"]]]])))

(defn- prepare-file-data
  [params]
  (let [user (friend/current-authentication)
        image (images/find-or-create (env :db)
                                     {:user-id (:id user)
                                      :original-filename (-> params
                                                             :source-file
                                                             :filename)
                                      :body (-> params
                                                :source-file
                                                :tempfile
                                                read-bytes)})]
    (cond-> params
      true
      (assoc :image-id (:id image)
             :content-type (-> params :source-file :content-type))

      (blank? (:caption params))
      (assoc :caption (-> params :source-file :filename)))))

(defn create
  [{params :params}]
  (let [attachment (->> params
                        prepare-file-data
                        (attachments/create (env :db)))]
    (if (seq (validation/error-messages attachment))
      (new-attachment nil attachment)
      (redirect (format "/transactions/%s/attachments"
                        (:transaction-id params))))))

(defn edit
  [req]
  "edit")

(defn update
  [req]
  "update")

(defn delete
  [{params :params}]
  (let [attachment (attachments/find-by-id (env :db) (Integer. (:id params)))]
    (attachments/delete (env :db) attachment)
    (redirect (format "/transactions/%s/attachments"
                      (:transaction-id attachment)))))
