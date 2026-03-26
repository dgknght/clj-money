(ns clj-money.views.invitations
  (:require [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.forms-validation :as v]
            [clj-money.icons :refer [icon-with-text]]
            [clj-money.state :refer [app-state +busy -busy]]
            [clj-money.util :refer [id=]]
            [clj-money.api.invitations :as invitations]))

(def ^:private status-options
  [[:unsent "Unsent"]
   [:sent "Sent"]
   [:accepted "Accepted"]
   [:declined "Declined"]])

(defn- load-invitations
  [page-state]
  (+busy)
  (invitations/select
    :callback -busy
    :on-success #(swap! page-state assoc :invitations %)))

(defn- save-invitation
  [page-state]
  (+busy)
  (invitations/create
    (get-in @page-state [:new-invitation])
    :callback -busy
    :on-success (fn [inv]
                  (swap! page-state
                         #(-> %
                              (update-in [:invitations] conj inv)
                              (dissoc :new-invitation))))))

(defn- delete-invitation
  [inv page-state]
  (when (js/confirm "Are you sure you want to delete this invitation?")
    (+busy)
    (invitations/delete
      inv
      :callback -busy
      :on-success #(swap! page-state update-in [:invitations]
                          (partial remove (fn [i] (id= i inv)))))))

(defn- invitation-row
  [inv page-state]
  ^{:key (str "inv-" (:id inv))}
  [:tr
   [:td (:invitation/recipient inv)]
   [:td (name (:invitation/status inv))]
   [:td (:invitation/note inv)]
   [:td
    [:button.btn.btn-sm.btn-danger
     {:on-click #(delete-invitation inv page-state)
      :title "Click here to delete this invitation"}
     (icon-with-text :x-circle "Delete")]]])

(defn- invitation-form
  [page-state]
  (let [invitation (r/cursor page-state [:new-invitation])]
    (fn []
      [:form.mt-3
       {:no-validate true
        :on-submit (fn [e]
                     (.preventDefault e)
                     (v/validate invitation)
                     (when (v/valid? invitation)
                       (save-invitation page-state)))}
       [:div.row.g-2.align-items-end
        [:div.col-md-4
         [forms/email-field invitation
          [:invitation/recipient]
          {:validations #{::v/required}
           :label "Recipient"}]]
        [:div.col-md-2
         [forms/select-field invitation
          [:invitation/status]
          status-options
          {:validations #{::v/required}
           :label "Status"}]]
        [:div.col-md-4
         [forms/text-field invitation
          [:invitation/note]
          {:label "Note"}]]
        [:div.col-md-2.d-flex.align-items-end
         [:button.btn.btn-primary
          {:type :submit
           :title "Click here to send the invitation"}
          (icon-with-text :envelope "Invite")]]]])))

(defn index []
  (let [page-state (r/atom {:invitations []
                             :new-invitation {:invitation/status :unsent}})]
    (load-invitations page-state)
    (fn []
      (let [invs (:invitations @page-state)]
        [:div.mt-3
         [:h2 "Invitations"]
         [invitation-form page-state]
         (if (seq invs)
           [:table.table.mt-3
            [:thead
             [:tr
              [:th "Recipient"]
              [:th "Status"]
              [:th "Note"]
              [:th]]]
            [:tbody
             (doall (map #(invitation-row % page-state) invs))]]
           [:p.mt-3 "No invitations yet."])]))))

(secretary/defroute "/invitations" []
  (swap! app-state assoc :page #'index :active-nav :users))
