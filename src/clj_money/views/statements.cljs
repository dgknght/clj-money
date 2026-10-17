(ns clj-money.views.statements
  (:require [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [accountant.core :as accountant]
            [clj-money.state :refer [app-state]]
            [clj-money.views.statement-upload :refer [statement-input
                                                      reading-indicator]]))

(defn- review-reconciliation
  [account]
  ; the accounts page opens the reconciliation when it loads
  (swap! app-state assoc :pending-reconciliation account)
  (accountant/navigate! "/accounts"))

(defn- index []
  (let [reading (r/atom nil)]
    (fn []
      [:div.row
       [:div.col-md-6
        [:h1.mt-3 "Statements"]
        [:p "Upload a bank or credit card statement (PDF). The account is
            identified from the statement, the transactions on it are matched
            to those already recorded, and any that are missing are added for
            you to review. The reconciliation is saved, unfinished, for you to
            complete."]
        [statement-input reading
         :caption "Upload a statement"
         :on-complete review-reconciliation]
        [reading-indicator reading]]])))

(secretary/defroute "/statements" []
  (swap! app-state assoc :page #'index :active-nav :statements))
