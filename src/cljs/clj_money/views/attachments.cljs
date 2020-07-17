(ns clj-money.views.attachments
  (:require [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [clj-money.util :as util]
            [clj-money.bootstrap :as bs]
            [clj-money.notifications :as notify]
            [clj-money.x-platform.util :refer [format-date
                                               path]]
            [clj-money.plain-forms :as forms]
            [clj-money.api.attachments :as attachments]))

(defn- process-deleted-attachment
  [state {:keys [transaction-id id]}]
  (update-in state
             [:attachments transaction-id]
             (fn [a] (remove #(= id (:id %)) a))))

(defn- delete-attachment
  [attachment page-state]
  (when (js/confirm "Are you sure you want to delete the attachment?")
    (attachments/delete attachment
                        (swap! page-state process-deleted-attachment attachment)
                        (notify/danger-fn "Unable to delete the attachment: %s"))))

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
      (bs/icon :eye)]
     [:button.btn.btn-sm.btn-info {:title "Click here to edit this attachment"
                                   :on-click (fn []
                                               (swap! page-state
                                                      assoc
                                                      :selected-attachment
                                                      attachment)
                                               (util/set-focus "caption"))}
      (bs/icon :pencil)]
     [:button.btn.btn-sm.btn-danger {:title "Click here to remove this attachment"
                                     :on-click #(delete-attachment attachment page-state)}
      (bs/icon :x-circle)]]]])

(defn- attachments-table
  [page-state]
  (let [item (r/cursor page-state [:attachments-item])
        all-attachments (r/cursor page-state [:attachments])
        attachments (make-reaction #(get-in @all-attachments [(:transaction-id @item)]))]
    (fn []
      [:table.table.table-hover
       [:thead
        [:tr
         [:th "Description"]
         [:th (util/space)]]]
       [:tbody
        (->> @attachments
             (map #(attachment-row % page-state))
             doall)]])))

(defn attachments-card
  [page-state]
  (let [item (r/cursor page-state [:attachments-item])]
    (fn []
      [:div.card
       [:div.card-header [:strong "Attachments"]]
       [:div.card-body
        (str (format-date (:transaction-date @item))
             " "
             (:description @item))]
       [attachments-table page-state]
       [:div.card-footer
        [:button.btn.btn-info {:on-click #(swap! page-state dissoc :attachments-item)
                               :title "Click here to close this window."}
         "Close"]]])))

(defn- process-saved-attachment
  [state attachment]
  (-> state
      (dissoc :selected-attachment)
      (update-in [:attachments (:transaction-id attachment)]
                 (fn [attachments]
                      (map #(if (= (:id attachment) (:id %))
                              attachment
                              %)
                           attachments)))))

(defn- save-attachment
  [page-state]
  (attachments/update (get-in @page-state [:selected-attachment])
                      #(swap! page-state (fn [s] (process-saved-attachment s %)))
                      (notify/danger-fn "Unable to save the attachment: %s")))

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
        (util/space)
        [:button.btn.btn-info {:on-click #(swap! page-state dissoc :selected-attachment)
                               :title "Click here to cancel this edit operation."}
         "Cancel"]]])))
