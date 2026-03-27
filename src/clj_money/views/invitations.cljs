(ns clj-money.views.invitations
  (:require [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.forms-validation :as v]
            [clj-money.components :refer [button
                                          spinner]]
            [clj-money.icons :refer [icon-with-text]]
            [clj-money.state :refer [app-state +busy -busy]]
            [clj-money.util :refer [id=]]
            [clj-money.app :refer [fetch-entities]]
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
    (get-in @page-state [:invitation])
    :callback -busy
    :on-success (fn [inv]
                  (swap! page-state
                         #(-> %
                              (update-in [:invitations] conj inv)
                              (dissoc :invitation))))))

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
           :label "Recipient"}]
         [forms/textarea-field invitation
          [:invitation/note]
          {:label "Note"}]
         [:div.d-flex
          [:button.btn.btn-primary
           {:type :submit
            :title "Click here to send the invitation"}
           (icon-with-text :envelope "Invite")]
          [:button.btn.btn-secondary.ms-2
           {:type :button
            :title "Click here to cancel this invitation"
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
        disable-add (make-reaction #(not (not invitation)))]
    (load-invitations page-state)
    (fn []
      [:div.mt-3
       [:h2 "Invitations"]
       [:div.row
        [:div.col-md-6
         [invitations-table page-state]
         [:div.mt-2
          [button {:html {:class "btn-primary"
                          :title "Click here to invite a user to use the system."
                          :on-click (fn []
                                      (swap! page-state
                                             assoc
                                             :invitation
                                             {:invitation/status :unsent}))}
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
                        :user {:user/email (:invitation/recipient %)})))

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
                  (fetch-entities))))

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
      :on-success #(swap! page-state assoc :declined? true))
    (fn []
      [:div.mt-3
       (if (:declined? @page-state)
         [:p "Thank you for taking the time to respond."]
         [:p "Loading..."])])))

(defn- accept-invitation-page
  [token]
  (let [page-state (r/atom {:token token})]
    (load-invitation-by-token page-state token)
    (fn []
      [:div.mt-3
       [:h2 "Create Your Account"]
       (if (:invitation @page-state)
         [acceptance-form page-state]
         [:p "Loading..."])])))

(secretary/defroute "/accept-invitation/:token" {:as params}
  (let [token (:token params)]
    (swap! app-state assoc :page #(accept-invitation-page token))))

(secretary/defroute "/decline-invitation/:token" {:as params}
  (let [token (:token params)]
    (swap! app-state assoc :page #(decline-invitation-page token))))
