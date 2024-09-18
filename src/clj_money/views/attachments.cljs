(ns clj-money.views.attachments
  (:require [reagent.core :as r]
            [dgknght.app-lib.web :refer [format-date
                                         path]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [clj-money.state :refer [+busy
                                     -busy]]
            [clj-money.api.attachments :as attachments]))

(defn- post-delete
  [page-state {:keys [transaction-id id]}]
  (-busy)
  (swap! page-state
         update-in
         [:attachments transaction-id]
         (fn [a] (remove #(= id (:id %)) a))))

(defn- delete-attachment
  [attachment page-state]
  (when (js/confirm "Are you sure you want to delete the attachment?")
    (+busy)
    (attachments/delete attachment
                        (map (partial post-delete page-state)))))

(defn- attachment-row
  [attachment page-state]
  ^{:key (str "attachment-row-" (:id attachment))}
  [:tr
   [:td (or (:caption attachment)
            (:created-at attachment))]
   [:td
    [:div.btn-group
     [:a.btn.btn-sm.btn-primary {:title "Click here to view this attachment."
                                 :href (path :images
                                             (:image-id attachment))
                                 :target "_blank"}
      (bs/icon :eye {:size :small})]
     [:button.btn.btn-sm.btn-light {:title "Click here to edit this attachment"
                                   :on-click (fn []
                                               (swap! page-state
                                                      assoc
                                                      :selected-attachment
                                                      attachment)
                                               (html/set-focus "caption"))}
      (bs/icon :pencil {:size :small})]
     [:button.btn.btn-sm.btn-danger {:title "Click here to remove this attachment"
                                     :on-click #(delete-attachment attachment page-state)}
      (bs/icon :x-circle {:size :small})]]]])

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
  (let [item (r/cursor page-state [:attachments-item])]
    (fn []
      [:div.card
       [:div.card-header
        [:strong "Attachments"]
        (str " "
             (format-date (:transaction-date @item))
             " "
             (:description @item))]
       [attachments-table page-state]
       [:div.card-footer
        [:button.btn.btn-secondary {:on-click #(swap! page-state dissoc :attachments-item)
                                    :title "Click here to close this window."}
         "Close"]]])))

(defn- post-save
  [page-state attachment]
  (-busy)
  (swap! page-state (fn [state]
                      (-> state
                          (dissoc :selected-attachment)
                          (update-in [:attachments]
                                     (fn [attachments]
                                       (map #(if (= (:id attachment) (:id %))
                                               attachment
                                               %)
                                            attachments)))))))

(defn- save-attachment
  [page-state]
  (+busy)
  (attachments/update (get-in @page-state [:selected-attachment])
                      (map (partial post-save page-state))))

(defn attachment-form
  [page-state]
  (let [attachment (r/cursor page-state [:selected-attachment])]
    (fn []
      [:div.card.mb-2
       [:div.card-header [:strong "Edit Attachment"]]
       [:div.card-body
        [forms/text-field attachment [:caption]]]
       [:div.card-footer
        [:button.btn.btn-primary {:on-click #(save-attachment page-state)
                                  :title "Click here to save this attachment"}
         "Save"]
        [:button.btn.btn-secondary.ms-2 {:on-click #(swap! page-state dissoc :selected-attachment)
                                         :title "Click here to cancel this edit operation."}
         "Cancel"]]])))
