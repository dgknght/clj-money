(ns clj-money.views.invitations
  (:require [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.forms-validation :as v]
            [clj-money.components :refer [button
                                          spinner]]
            [clj-money.icons :refer [icon-with-text
                                     icon]]
            [clj-money.state :refer [app-state +busy -busy]]
            [clj-money.util :as util :refer [id=]]
            [clj-money.app :refer [init-client-app]]
            [clj-money.api.invitations :as invitations]))

(defn- load-invitations
  [page-state]
  (+busy)
  (invitations/select
    :callback -busy
    :on-success #(swap! page-state assoc :invitations (sort-by :invitation/recipient %))))

(defn- post-save-invitation
  [page-state]
  (fn [saved]
    (swap! page-state
           #(-> %
                (update-in [:invitations]
                           (fn [invs]
                             (util/upsert-into saved
                                               {:sort-key :invitation/recipient}
                                               invs)))
                (dissoc :invitation)))))

(defn- save-invitation
  [page-state]
  (+busy)
  (let [inv (get-in @page-state [:invitation])]
    (invitations/save inv
                      :callback -busy
                      :on-success (post-save-invitation page-state))))

(defn- send-invitation
  [inv page-state]
  (when (js/confirm "Are you sure you want to send this invitation?")
    (+busy)
    (invitations/send
      inv
      :callback -busy
      :on-success (fn [sent]
                    (swap! page-state update-in [:invitations]
                           #(mapv (fn [i] (if (id= i inv) sent i)) %))))))

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
   [:td
    (let [disabled (not= :unsent (:invitation/status inv))]
      [:div.btn-group
       [:button.btn.btn-sm.btn-info.d-flex.align-items-center
        {:on-click #(send-invitation inv page-state)
         :title "Click here to send this invitation"
         :disabled disabled}
        (icon :envelope :size :small)]
       [:button.btn.btn-sm.btn-secondary.d-flex.align-items-center
        {:on-click #(swap! page-state assoc :invitation inv)
         :title "Click here to edit this invitation"
         :disabled disabled}
        (icon :pencil :size :small)]
       [:button.btn.btn-sm.btn-danger.d-flex.align-items-center
        {:on-click #(delete-invitation inv page-state)
         :title "Click here to remove this invitation"
         :disabled disabled}
        (icon :x-circle :size :small)]])]])

(defn- invitation-form
  [page-state]
  (let [invitation (r/cursor page-state [:invitation])]
    (fn []
      (when @invitation
        [:form.mt-3
         {:no-validate true
          :on-submit (fn [e]
                       (.preventDefault e)
                       (v/validate invitation)
                       (when (v/valid? invitation)
                         (save-invitation page-state)))}
         [forms/email-field invitation
          [:invitation/recipient]
          {:validations #{::v/required}
           :label "Recipient"
           :disabled (boolean (:id @invitation))}]
         [forms/textarea-field invitation
          [:invitation/note]
          {:label "Note"}]
         (when-not (:id @invitation)
           [forms/checkbox-field
            invitation
            [:send?]
            {:caption "Send immediately"}])
         [:div.d-flex
          [:button.btn.btn-primary
           {:type :submit
            :title "Click here to save the invitation"}
           (icon-with-text :check "Save")]
          [:button.btn.btn-secondary.ms-2
           {:type :button
            :title "Click here to cancel"
            :on-click #(swap! page-state dissoc :invitation)}
           (icon-with-text :x "Cancel")]]]))))

(defn- invitations-table
  [page-state]
  (let [invitations (r/cursor page-state [:invitations])]
    (fn []
      [:table.table.mt-3
       [:thead
        [:tr
         [:th.col-sm-9 "Recipient"]
         [:th.col-sm-3 "Status"]
         [:th]]]
       [:tbody
        (cond
          (seq @invitations)
          (doall (map #(invitation-row % page-state) @invitations))
          
          @invitations
          [:tr [:td.text-body-tertiary {:col-span 4} "There are no invitations"]]
          
          :else
          [:tr [:td {:col-span 3} [spinner]]])]])))

(defn index []
  (let [page-state (r/atom {})
        invitation (r/cursor page-state [:invitation])
        disable-add (make-reaction #(not (not invitation)))
        hide? (make-reaction #(boolean @invitation))]
    (load-invitations page-state)
    (fn []
      [:div.mt-3
       [:h2 "Invitations"]
       [:div.row
        [:div {:class (cond-> "col-md-6"
                        @hide? (str " d-none d-lg-block"))}
         [invitations-table page-state]
         [:div.mt-2
          [button {:html {:class "btn-primary"
                          :title "Click here to invite a user to use the system."
                          :on-click (fn []
                                      (swap! page-state
                                             assoc
                                             :invitation
                                             {:send? true}))}
                   :icon :plus
                   :disabled disable-add
                   :caption "Add"}]]]
        [:div.col-md-6
         [invitation-form page-state]]]])))

(secretary/defroute "/invitations" []
  (swap! app-state assoc :page #'index :active-nav :users))

(defn- load-invitation-by-token
  [page-state token]
  (+busy)
  (invitations/find-by-token
    token
    :callback -busy
    :on-success #(swap! page-state assoc
                        :invitation %
                        :user {:user/email (:invitation/recipient %)})
    :on-error (fn [e]
                (if (= "invitation expired" (:message (ex-data e)))
                  (swap! page-state assoc :expired? true)
                  (swap! page-state assoc :not-found? true)))))

(defn- do-accept-invitation
  [page-state]
  (+busy)
  (invitations/accept
    (:token @page-state)
    (:user @page-state)
    :callback -busy
    :on-success (fn [{:keys [user auth-token]}]
                  (swap! app-state assoc
                         :current-user user
                         :auth-token auth-token)
                  (init-client-app))))

(defn- acceptance-form
  [page-state]
  (let [user (r/cursor page-state [:user])]
    (fn []
      [:form.mt-3
       {:no-validate true
        :on-submit (fn [e]
                     (.preventDefault e)
                     (v/validate user)
                     (when (v/valid? user)
                       (do-accept-invitation page-state)))}
       [:div.row.g-2
        [:div.col-md-6
         [forms/text-field user
          [:user/first-name]
          {:validations #{::v/required}
           :label "First Name"}]]
        [:div.col-md-6
         [forms/text-field user
          [:user/last-name]
          {:validations #{::v/required}
           :label "Last Name"}]]
        [:div.col-md-12
         [forms/email-field user
          [:user/email]
          {:label "Email"
           :disabled true}]]
        [:div.col-md-6
         [forms/password-field user
          [:user/password]
          {:validations #{::v/required}
           :label "Password"}]]]
       [:button.btn.btn-primary.mt-2
        {:type :submit
         :title "Click here to create your account"}
        (icon-with-text :person-plus "Create Account")]])))

(defn- decline-invitation-page
  [token]
  (let [page-state (r/atom {})]
    (+busy)
    (invitations/decline
      token
      :callback -busy
      :on-success #(swap! page-state assoc :declined? true)
      :on-error (fn [e]
                  (swap! page-state assoc
                         (if (= "invitation expired" (:message (ex-data e)))
                           :expired?
                           :not-found?)
                         true)))
    (fn []
      [:div.mt-3
       (cond
         (:declined? @page-state)
         [:p "Thank you for taking the time to respond."]

         (:expired? @page-state)
         [:p "This invitation has expired. Please contact an administrator."]

         :else
         [:p "Loading..."])])))

(defn- accept-invitation-page
  [token]
  (let [page-state (r/atom {:token token})]
    (load-invitation-by-token page-state token)
    (fn []
      [:div.mt-3
       [:h2 "Create Your Account"]
       (cond
         (:expired? @page-state)
         [:p "This invitation has expired. Please contact an administrator for a new invitation."]

         (:not-found? @page-state)
         [:p "This invitation was not found."]

         (:invitation @page-state)
         [acceptance-form page-state]

         :else
         [:p "Loading..."])])))

(secretary/defroute #"/accept-invitation/(.+)" [token]
  (swap! app-state assoc :page #(accept-invitation-page token)))

(secretary/defroute #"/decline-invitation/(.+)" [token]
  (swap! app-state assoc :page #(decline-invitation-page token)))
