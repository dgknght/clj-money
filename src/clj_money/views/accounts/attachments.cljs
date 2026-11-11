(ns clj-money.views.accounts.attachments
  "Connects the attachments views to the register on the Accounts page:
  viewing a transaction's attachments, and keeping the attachment counts of
  the loaded :items up to date."
  (:require [clj-money.state :refer [+busy
                                     -busy]]
            [clj-money.util :refer [id=]]
            [clj-money.api.attachments :as atts]
            [clj-money.views.attachments :as atts-view]))

(defn show-attachments
  "Opens the attachments card for the given transaction item"
  [page-state item]
  (swap! page-state assoc :attachments-item item)
  (+busy)
  (atts/select {:attachment/transaction (:transaction-item/transaction item)}
               :callback -busy
               :on-success #(swap! page-state assoc :attachments %)))

(defn- update-attachment-count
  "Applies f to the attachment count of each item in :items that belongs to
  the transaction of the given attachment."
  [state attachment f]
  (update-in state
             [:items]
             (fn [items]
               (map (fn [item]
                      (if (id= (:attachment/transaction attachment)
                               (:transaction-item/transaction item))
                        (update-in item [:transaction/attachment-count] (fnil f 0))
                        item))
                    items))))

(defn attachments-card
  [page-state]
  [atts-view/attachments-card page-state
   :on-delete #(swap! page-state update-attachment-count % dec)])

(defn pending-attachment-form
  [page-state]
  [atts-view/pending-attachment-form page-state
   :on-save-success (fn [{:keys [item]} created]
                      (swap! page-state
                             (fn [state]
                               (-> state
                                   (update-in [:item-row-styles]
                                              dissoc
                                              (:id item))
                                   (update-attachment-count created inc)))))])

(defn drop-handlers
  "Lets an attachment be dropped on the row of the given item"
  [page-state item]
  (atts-view/drop-handlers page-state :item-row-styles (:id item)
                           {:item item
                            :attachment #:attachment{:transaction (:transaction-item/transaction item)
                                                     :caption ""}}))
