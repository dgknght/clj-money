(ns clj-money.views.attachments
  (:require [cljs.pprint :refer [pprint]]
            [clojure.core.async :as a]
            [reagent.core :as r]
            [dgknght.app-lib.web :refer [format-date
                                         path]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.dom :as dom]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [cljs-http.client :as http]
            [clj-money.object-url :as obj-url]
            [clj-money.util :as util]
            [clj-money.icons :refer [icon]]
            [clj-money.state :refer [+busy
                                     -busy
                                     auth-token]]
            [clj-money.api.attachments :as atts]))

(defn- post-delete
  [page-state]
  (fn [{:keys [id] :attachment/keys [transaction]}]
    (swap! page-state
           update-in
           [:attachments (:id transaction)]
           (fn [a] (remove #(= id (:id %)) a)))))

(defn- delete-attachment
  [attachment page-state]
  (when (js/confirm "Are you sure you want to delete the attachment?")
    (+busy)
    (atts/delete attachment
                 :callback -busy
                 :on-success (post-delete page-state))))

(defn- view-image
  [{:keys [id]}]
  (a/go
    (let [{:keys [body]} (a/<! (http/get (path :app :images id)
                              {:with-credentials? false
                               :response-type :blob
                               :headers {"Authorization" (str "Bearer " @auth-token)}}))
          url (obj-url/create body)]
      (.open js/window url "_blank"))))

(defn- attachment-row
  [{:as attachment :attachment/keys [caption created-at image]} page-state]
  ^{:key (str "attachment-row-" (:id attachment))}
  [:tr
   [:td (or caption created-at "unnamed")]
   [:td
    [:div.btn-group
     [:button.btn.btn-sm.btn-primary {:title "Click here to view this attachment."
                                      :on-click #(view-image image)}
      (icon :eye {:size :small})]
     [:button.btn.btn-sm.btn-secondary {:title "Click here to edit this attachment"
                                   :on-click (fn []
                                               (swap! page-state
                                                      assoc
                                                      :selected-attachment
                                                      attachment)
                                               (dom/set-focus "caption"))}
      (icon :pencil {:size :small})]
     [:button.btn.btn-sm.btn-danger {:title "Click here to remove this attachment"
                                     :on-click #(delete-attachment attachment page-state)}
      (icon :x-circle {:size :small})]]]])

(defn- attachments-table
  [page-state]
  (let [attachments (r/cursor page-state [:attachments])]
    (fn []
      [:table.table.table-hover
       [:thead
        [:tr
         [:th "Description"]
         [:th (html/space)]]]
       [:tbody
        (->> @attachments
             (map #(attachment-row % page-state))
             doall)]])))

(defn attachments-card
  [page-state]
  (let [item (r/cursor page-state [:attachments-item])
        selected (r/cursor page-state [:selected-attachment])]
    (fn []
      (when (and @item (not @selected))
        [:div.card
         [:div.card-header
          [:strong "Attachments"]
          (str " "
               (format-date (:transaction/transaction-date @item))
               " "
               (:transaction/description @item))]
         [attachments-table page-state]
         [:div.card-footer
          [:button.btn.btn-secondary {:on-click #(swap! page-state dissoc :attachments-item)
                                      :title "Click here to close this window."}
           "Close"]]]))))

(defn- post-save
  [page-state]
  (fn [attachment]
    (swap! page-state (fn [state]
                        (-> state
                            (dissoc :selected-attachment)
                            (update-in [:attachments]
                                       #(util/upsert-into
                                          attachment
                                          {:sort-key :attachment/caption}
                                          %)))))))

(defn- save-attachment
  [page-state]
  (+busy)
  (atts/update (get-in @page-state [:selected-attachment])
               :callback -busy
               :on-success (post-save page-state)))

(defn caption-form
  "Presentational card for capturing an attachment caption. save-fn and
  cancel-fn are no-arg callbacks; cursor and field-path locate the caption
  within the caller's page-state."
  [{:keys [cursor field-path title save-fn cancel-fn cancel-title]}]
  [:div.card.mb-2
   [:div.card-header [:strong title]]
   [:div.card-body
    [forms/text-field cursor field-path]]
   [:div.card-footer
    [:button.btn.btn-primary {:on-click save-fn
                              :title "Click here to save this attachment"}
     "Save"]
    [:button.btn.btn-secondary.ms-2 {:on-click cancel-fn
                                     :title cancel-title}
     "Cancel"]]])

(defn attachment-form
  [page-state]
  (let [attachment (r/cursor page-state [:selected-attachment])]
    (fn []
      (when @attachment
        [caption-form {:cursor attachment
                       :field-path [:attachment/caption]
                       :title "Edit Attachment"
                       :save-fn #(save-attachment page-state)
                       :cancel-fn #(swap! page-state dissoc :selected-attachment)
                       :cancel-title "Click here to cancel this edit operation."}]))))

(defn caption-modal
  "Bootstrap modal for capturing an attachment caption. save-fn and cancel-fn
  are no-arg callbacks; cursor and field-path locate the caption within the
  caller's page-state. While saving? is true, the modal stays open, shows a
  spinner in place of the Save button, and disables both buttons so a
  response can't be interrupted. A truthy error is rendered in the modal
  body and the modal remains open so the user can retry or cancel."
  [{:keys [cursor field-path title save-fn cancel-fn cancel-title saving? error]}]
  [:<>
   [:div.modal.show.d-block {:tab-index -1
                             :role :dialog}
    [:div.modal-dialog
     [:div.modal-content
      [:div.modal-header
       [:h5.modal-title title]
       [:button.btn-close {:type :button
                           :aria-label "Close"
                           :title cancel-title
                           :disabled saving?
                           :on-click cancel-fn}]]
      [:div.modal-body
       (when error
         [:div.alert.alert-danger error])
       [forms/text-field cursor field-path]]
      [:div.modal-footer
       [:button.btn.btn-secondary {:on-click cancel-fn
                                   :title cancel-title
                                   :disabled saving?}
        "Cancel"]
       [:button.btn.btn-primary {:on-click save-fn
                                 :title "Click here to save this attachment"
                                 :disabled saving?}
        (if saving?
          [bs/spinner {:size :small}]
          "Save")]]]]]
   [:div.modal-backdrop.show {:on-click (when-not saving? cancel-fn)}]])
