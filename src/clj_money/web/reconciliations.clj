(ns clj-money.web.reconciliations
  (:require [environ.core :refer [env]]
            [clj-time.core :as t]
            [ring.util.response :refer :all]
            [clj-money.validation :as validation]
            [clj-money.util :as util]
            [clj-money.web.shared :refer [with-layout
                                          date-input-field
                                          number-input-field]]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.reconciliations :as reconciliations]))

(defn- reconciliation-item-row
  [account transaction-item]
  [:tr
   [:td.text-right (util/format-date (:transaction-date transaction-item))]
   [:td (:description transaction-item)]
   [:td.text-right (-> transaction-item
                       (accounts/polarize-amount account)
                       util/format-number)]
   [:td.text-center [:input {:type :checkbox
                             :name "item-ids"
                             :value (:id transaction-item)}]]])

(defn new-reconciliation
  ([req] (new-reconciliation req {:end-of-period (t/today)}))
  ([{params :params} reconciliation]
   (let [account-id (Integer. (:account-id params))
         account (accounts/find-by-id (env :db) account-id)]
     (with-layout "Reconcile account" {}
       [:form {:action (format "/accounts/%s/reconciliations" account-id)
               :method :post}
        [:div.row
         [:div.col-md-4
          (date-input-field reconciliation :end-of-period {:autofocus true})
          [:div.form-group
           [:label.control-label {:for :previous-balance} "Previous balance"]
           [:input.form-control {:name :previous-balance
                                 :value "0.00"
                                 :disabled true}]]
          (number-input-field reconciliation :balance)
          [:input.btn.btn-primary {:type :submit
                                   :value "Finish"
                                   :title "Click here to complete the reconciliation."}]
          "&nbsp;"
          [:input.btn.btn-default {:type :submit
                                   :value "Save"
                                   :title "Click here to save the reconciliation and complete it later."}]
          "&nbsp;"
          [:a.btn.btn-default
           {:href (format "/entities/%s/accounts" (:entity-id account))
            :title "Click here to return to the list of accounts."}
           "Back"]]
         [:div.col-md-8
          [:table.table.table-striped.table-hover
           [:tr
            [:th.col-sm-2.text-right "Date"]
            [:th.col-sm-5 "Description"]
            [:th.col-sm-3.text-right "Amount"]
            [:th.col-sm-2.text-center "Rec."]]
           (map #(reconciliation-item-row account %)
                (transactions/unreconciled-items-by-account (env :db)
                                                            account-id))]]]]))))

(defn create
  [{params :params}]
  (let [reconciliation (cond-> params
                         (= "Finish" (:submit params))
                         (assoc params :status :completed))
        result (reconciliations/create (env :db) reconciliation)]
    (if (validation/has-error? result)
      (new-reconciliation {:params params} result)
      (redirect (format "/accounts/%s" (:account-id result))))))

(defn show
  [params]
  "show")

(defn edit
  [params]
  "edit")

(defn delete
  [params]
  "delete")
