(ns clj-money.views.cli-logins
  "The page where a user approves a sign-in from the command-line app. The
  CLI opens it with the code it shows in the terminal. It needs a signed-in
  user, so someone who isn't is sent to sign in first and brought back here
  (see clj-money.return-path). See clj-money.cli-logins on the server for the
  whole flow."
  (:require [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.forms-validation :as v]
            [clj-money.components :refer [button]]
            [clj-money.state :refer [app-state +busy -busy]]
            [clj-money.api.cli-logins :as cli-logins]))

(defn- approve
  [page-state]
  (+busy)
  (cli-logins/approve (get-in @page-state [:login :code])
                      :callback -busy
                      :on-success #(swap! page-state assoc :result :approved)
                      :on-error #(swap! page-state assoc :result :failed)))

(defn- approval-form
  [page-state]
  (let [login (r/cursor page-state [:login])]
    (fn []
      [:<>
       [:p
        "Check that this code matches the one in your terminal. Approving"
        " lets the command-line app use your account."]
       [:form {:no-validate true
               :on-submit (fn [e]
                            (.preventDefault e)
                            (v/validate login)
                            (when (v/valid? login)
                              (approve page-state)))}
        [forms/text-field login [:code] {:validations #{::v/required}
                                         :html {:auto-complete :off
                                                :auto-capitalize :characters
                                                :class "font-monospace fs-4"}}]
        [:div.d-flex.gap-2
         [button {:html {:title "Click here to sign in the command-line app."
                         :class "btn-primary"
                         :type :submit}
                  :icon :check
                  :caption "Approve"}]
         [:a.btn.btn-secondary {:href "/"} "Cancel"]]]])))

(defn- approved []
  [:div.alert.alert-success
   "The command-line app is signed in. You can close this window and return"
   " to your terminal."])

(defn- failed
  [page-state]
  [:<>
   [:div.alert.alert-danger
    "That code is invalid, has expired, or was already used. Run "
    [:code "money login"]
    " again to get a new one."]
   [:button.btn.btn-secondary
    {:type :button
     :on-click #(swap! page-state dissoc :result)}
    "Try Another Code"]])

(defn- cli-login
  [code]
  (let [page-state (r/atom {:login {:code code}})]
    (fn []
      [:div.mt-3
       [:h1 "Sign In the Command-Line App"]
       [:div.row
        [:div.col-md-6
         (case (:result @page-state)
           :approved [approved]
           :failed [failed page-state]
           [approval-form page-state])]]])))

(secretary/defroute "/cli/login" [query-params]
  (swap! app-state assoc :page #(cli-login (:code query-params))))
