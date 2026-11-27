(ns clj-money.views.accounts.transaction-export
  "A form for exporting the transactions of the account shown on the
  Accounts page as CSV (issue #423)"
  (:require [cljs-time.core :as t]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [dgknght.app-lib.forms :as forms]
            [clj-money.components :refer [button]]
            [clj-money.object-url :as obj-url]
            [clj-money.state :refer [+busy
                                     -busy
                                     busy?]]
            [clj-money.api.transaction-exports :as exports]))

(def ^:private fields
  [[:transaction-date "Date"]
   [:description "Description"]
   [:amount "Amount"]
   [:balance "Balance"]
   [:memo "Memo"]
   [:other-account "Other Account"]
   [:reconciled "Reconciled"]])

(defn new-export
  "Returns the options for exporting the account's transactions: all of
  them, with every field"
  [{:account/keys [transaction-date-range]}]
  {:since-zero-balance? false
   :start-date (first transaction-date-range)
   :end-date (t/today)
   :fields (set (map first fields))})

(defn- download
  [blob file-name]
  (let [a (.createElement js/document "a")]
    (set! (.-href a) (obj-url/create blob))
    (set! (.-download a) file-name)
    (.click a)))

(defn export-form
  "Takes an atom holding the options (see new-export) and the account whose
  transactions are exported, and calls on-done once they're downloaded or
  the user cancels"
  [export & _]
  (let [since-zero-balance? (r/cursor export [:since-zero-balance?])
        selected-fields (r/cursor export [:fields])
        disabled? (make-reaction #(or @busy? (empty? @selected-fields)))]
    (fn [_export & {:keys [account on-done]}]
      [:form {:no-validate true
              :on-submit (fn [e]
                           (.preventDefault e)
                           (+busy)
                           (exports/export account
                                           @export
                                           :callback -busy
                                           :on-success (fn [blob file-name]
                                                         (download blob file-name)
                                                         (on-done))))}
       [:h2 "Export Transactions"]
       [forms/checkbox-field
        export
        [:since-zero-balance?]
        {:caption "Since the last zero balance"}]
       (when-not @since-zero-balance?
         [forms/date-field export [:start-date] {:caption "From"}])
       [forms/date-field export [:end-date] {:caption "Through"}]
       [forms/checkboxes-field export [:fields] fields {:caption "Fields"}]
       [:div.mt-3
        [button {:html {:class "btn-primary"
                        :type :submit
                        :title "Click here to download the transactions as a CSV file."}
                 :disabled? disabled?
                 :caption "Export"
                 :icon :download}]
        [button {:html {:class "btn-secondary ms-2"
                        :type :button
                        :on-click on-done
                        :title "Click here to return to the account's transactions."}
                 :caption "Cancel"
                 :icon :x}]]])))
