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
            [clj-money.authorization :refer [apply-scope
                                             authorize
                                             allowed?
                                             tag-resource]]
            [clj-money.permissions.attachments]
            [clj-money.models.images :as images] 
            [clj-money.models.transactions :as transactions]
            [clj-money.models.attachments :as attachments])
  (:use [clj-money.web.shared :refer :all]))

(defn- attachment-row
  [attachment]
  [:tr
   [:td
    (if (allowed? :show attachment)
      [:a {:href (format "/images/%s" (:image-id attachment))
           :target "_blank"}
       (:caption attachment)]
      (:caption attachment))]
   [:td.text-center
    [:div.btn-group
     (when (allowed? :delete attachment)
       [:a.btn.btn-xs.btn-danger
        {:href (format "/attachments/%s/delete" (:id attachment))
         :title "Click here to remove this attachment"
         :data-method "POST"
         :data-confirm "Are you sure you want to remove this attachment?" }
        [:span.glyphicon.glyphicon-remove {:aria-hidden true}]])]]])

(defn index
  [{{transaction-id :transaction-id} :params}]
  ; TODO once the scope bit is figured out, we won't need to authorize the transaction
  (let [transaction (authorize (transactions/find-by-id (env :db) transaction-id) :show)
        attachments (attachments/search
                      (env :db)
                      {:transaction-id (:id transaction)}
                      ; TODO re-apply the scope once a better method is worked out
                      #_(apply-scope {:transaction-id (:id transaction)} :attachment))]
    (with-layout "Attachments" {}
      [:div.row
       [:div.col-md-4
        [:table.table.table-striped
         [:tr
          [:th "Caption"]
          [:th "&nbsp;"]]
         (map attachment-row attachments)]]]
      (when (allowed? :create (-> {:transaction-id transaction-id}
                                  (tag-resource :attachment)))
        [:a.btn.btn-primary {:href (format "/transactions/%s/attachments/new" transaction-id)}
         "Add"])
      "&nbsp;"
      ; TODO Fix this hack, we need to know the correct account to go back to
      [:a.btn.btn-default
       {:href (format "/accounts/%s"
                      (-> transaction :items first :account-id))}
       "Back"])))

(defn new-attachment
  ([{{transaction-id :transaction-id} :params :as req}]
   (new-attachment req (-> {:transaction-id transaction-id}
                           (tag-resource :attachment)
                           (authorize :create))))
  ([_ attachment]
   (with-layout "New attachment" {}
     (when (validation/has-error? attachment)
       [:div.alert.alert-danger
        "Unable to save the attachment"
        [:ul
         (map #(vector :li %) (validation/flat-error-messages attachment))]])
     [:div.row
      [:div.col-md-6
       (form (format "/transactions/%s/attachments"
                     (:transaction-id attachment))
             {:enctype "multipart/form-data"}

             (text-input-field attachment :caption {:autofocus true})
             (file-input-field attachment :source-file)
             [:button.btn.btn-primary {:type :submit}
              "Submit"])]])))

(defn- prepare-file-data
  [{source-file :source-file :as params}]
  (let [user (friend/current-authentication)
        image (images/find-or-create (env :db)
                                     {:user-id (:id user)
                                      :original-filename (:filename source-file)
                                      :content-type (:content-type source-file)
                                      :body (-> source-file
                                                :tempfile
                                                read-bytes)})]
    (cond-> params
      true
      (select-keys [:caption
                    :source-file
                    :transaction-id])

      true
      (assoc :image-id (:id image))

      (blank? (:caption params))
      (assoc :caption (:filename source-file)))))

(defn create
  [{params :params}]
  (let [attachment (->> (-> params
                            (tag-resource :attachment)
                            (authorize :create))
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
