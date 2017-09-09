(ns clj-money.web.reconciliations
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [hiccup.core :refer :all]
            [ring.util.response :refer :all]
            [clj-money.validation :as validation]
            [clj-money.util :as util]
            [clj-money.web.shared :refer :all]
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
                             :checked (not (nil? (:reconciliation-id transaction-item)))
                             :value (:id transaction-item)}]]])

(defn- reconciled-item-total
  [account reconciliation]
  (if (:id reconciliation)
    (reduce +
            0M
            (->> reconciliation
                 :id
                 (transactions/select-items-by-reconciliation-id (env :db))
                 (map #(transactions/polarize-item-amount % account))
                 (map :polarized-amount)))
    0M))

(defn- reconciliation-form
  [reconciliation]
  (let [account (accounts/find-by-id (env :db) (:account-id reconciliation))
        last-completed (reconciliations/find-last-completed (env :db) (:id account))
        previous-balance (or (:balance last-completed) 0M)
        reconciled-item-total (reconciled-item-total account reconciliation)]
    (with-layout (format "Reconcile account: %s" (:name account)) {}

      (when (validation/has-error? reconciliation)
        [:pre (prn-str (validation/error-messages reconciliation))])

      (form (if (:id reconciliation)
              (format "/reconciliations/%s" (:id reconciliation))
              (format "/accounts/%s/reconciliations" (:id account))) {}
            [:div.row
             [:div.col-md-4
              (date-input-field reconciliation :end-of-period {:autofocus true})
              [:div.form-group
               [:label.control-label {:for :end-of-previous-period} "End of prev. period"]
               [:input.form-control {:name :end-of-previous-period
                                     :value (when last-completed
                                              (util/format-date (:end-of-period last-completed)))
                                     :disabled true}]]
              [:div.form-group
               [:label.control-label {:for :previous-balance} "Previous balance"]
               [:input.form-control {:name :previous-balance
                                     :value (util/format-number previous-balance)
                                     :disabled true}]]
              [:div.form-group
               [:label.control-label {:for :reconciled-balance} "Reconciled balance"]
               [:input.form-control {:name :reconciled-balance
                                     :value (util/format-number
                                              (if (= 0M reconciled-item-total)
                                                0M
                                                (+ previous-balance
                                                   reconciled-item-total)))
                                     :disabled true}]]
              (number-input-field reconciliation :balance)
              [:input.btn.btn-primary {:type :submit
                                       :name :submit
                                       :value "Finish"
                                       :title "Click here to complete the reconciliation."}]
              "&nbsp;"
              [:input.btn.btn-default {:type :submit
                                       :name :submit
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
                                                                (:id account)))]]]))))

(defn new-reconciliation
  ([req] (new-reconciliation req {:end-of-period (t/today)}))
  ([{params :params} reconciliation]
   (let [account-id (Integer. (:account-id params))
         working-rec (reconciliations/find-working (env :db) account-id)]
     (if working-rec
       (redirect (format "/reconciliations/%s/edit" (:id working-rec)))
       (reconciliation-form (assoc params :account-id account-id))))))

(defn create
  [{params :params}]
  (let [reconciliation (cond-> params
                         (= "Finish" (:submit params))
                         (assoc :status :completed))
        result (reconciliations/create (env :db) reconciliation)]
    (if (validation/has-error? result)
      (new-reconciliation {:params params} result)
      (redirect (format "/accounts/%s" (:account-id result))))))

(defn show
  [params]
  "show")

(defn edit
  ([{{id :id} :params :as req}]
   (edit req (reconciliations/find-by-id (env :db) (Integer. id))))
  ([_ reconciliation]
   (reconciliation-form reconciliation)))

(defn update
  [{params :params}]
  (let [result (reconciliations/update (env :db)
                                       (cond-> params
                                         (= "Finish" (:submit params))
                                         (assoc :status :completed)))]
    (if (validation/has-error? result)
      (edit {} result)
      (redirect (format "/accounts/%s" (:account-id result))))))

(defn delete
  [params]
  "delete")
