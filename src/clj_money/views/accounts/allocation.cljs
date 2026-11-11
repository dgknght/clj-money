(ns clj-money.views.accounts.allocation
  "The form for setting the target allocation of a trading account's value
  among its child accounts.

  The caller owns an atom (or cursor) holding the allocation, started with
  new-allocation, and renders
  [asset-allocation allocation :hide-zero-balances? ... :on-save ...
  :on-cancel ...]. The form saves the account's allocations itself and
  passes the result to :on-save."
  (:require [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [reagent.format :refer [currency-format]]
            [dgknght.app-lib.web :refer [format-percent]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.decimal :as decimal]
            [dgknght.app-lib.forms :as forms]
            [clj-money.util :refer [id=]]
            [clj-money.components :refer [button]]
            [clj-money.api.accounts :as accounts]
            [clj-money.accounts :refer [allocate]]
            [clj-money.state :refer [accounts
                                     accounts-by-id
                                     +busy
                                     -busy]]
            [clj-money.cached-accounts :refer [latest]]))

(defn- prepare-for-allocation
  "Supplies allocations to an account that has none, in proportion to the
  current values of its children"
  [{:account/keys [children-value allocations] :as account}]
  (if allocations
    account
    (let [children (filterv #(id= account
                                  (:account/parent %))
                            @accounts)]
      (if (seq children)
        (assoc account
               :account/allocations
               (reduce (fn [allocs act]
                         (assoc allocs
                                (:id act)
                                (decimal/* 100M
                                           (if (< 0M children-value)
                                             (decimal// (:account/total-value act)
                                                        children-value)
                                             (decimal// 1 (count children))))))
                       {}
                       children))
        {}))))

(defn new-allocation
  "Returns the allocation of the given account, to edit in the form"
  [account]
  (let [a (latest account)]
    {:account (prepare-for-allocation a)
     :cash (:account/value a)
     :withdrawal 0M}))

(defn- prepare-allocations
  [allocations]
  (when allocations
    (->> allocations
         (remove (fn [[_ v]] (decimal/zero? v)))
         (into {}))))

(defn- save
  [account on-save]
  (+busy)
  (accounts/save (update-in account [:account/allocations] prepare-allocations)
                 :callback -busy
                 :on-success on-save))

(defn- asset-allocation-row
  [{:keys [account
           adj-value
           target-value
           current-value
           current-percentage]}
   parent
   hide-zero-balances?]
  ^{:key (str "asset-allocation-row-" (:id account))}
  [:tr {:class (when (and (decimal/zero? (:quantity account))
                          hide-zero-balances?)
                 "d-none")}
   [:td (:account/name account)]
   [:td.text-end [forms/decimal-input parent [:account/allocations (:id account)]]]
   [:td.text-end.d-none.d-lg-table-cell (currency-format target-value)]
   [:td.text-end.d-none.d-lg-table-cell (format-percent current-percentage)]
   [:td.text-end.d-none.d-lg-table-cell (currency-format current-value)]
   [:td.text-end (currency-format adj-value)]])

(defn asset-allocation
  [allocation & _opts]
  (let [account (r/cursor allocation [:account])
        cash (r/cursor allocation [:cash])
        withdrawal (r/cursor allocation [:withdrawal])
        allocations (make-reaction #(when (and @cash @withdrawal)
                                      (sort-by (comp :account/name :account)
                                               (allocate @account
                                                         @accounts-by-id
                                                         :cash @cash
                                                         :withdrawal @withdrawal))))
        total-percent (make-reaction #(decimal// (reduce decimal/+ 0M (vals (:account/allocations @account)))
                                                 100M))
        unallocated (make-reaction #(decimal/- 1M @total-percent))
        total-percent-class (make-reaction #(if (decimal/zero? @unallocated)
                                              "text-success"
                                              "text-danger"))]
    (fn [_allocation & {:keys [hide-zero-balances? on-save on-cancel]}]
      [:form#allocation-form
       {:no-validate true
        :on-submit (fn [e]
                     (.preventDefault e)
                     (save @account on-save))}
       [:div
        [:h2 "Asset Allocation for " (:name @account) " " (currency-format (:total-value @account))]
        [:div.row
         [:div.col-md-6
          [:table.table.table-borderless
           [:tbody
            [:tr
             [:th {:scope :row} "Cash"]
             [:td (currency-format (:quantity @account))]]
            [:tr
             [:th {:scope :row} "Cash to reserve"]
             [:td
              [forms/decimal-input allocation [:cash]]]]
            [:tr
             [:th {:scope :row} "Cash to withdraw"]
             [:td
              [forms/decimal-input allocation [:withdrawal]]]]]]]]
        [:table.table
         [:thead
          [:tr
           [:th "Account"]
           [:th.text-end "Target %"]
           [:th.text-end.d-none.d-lg-table-cell "Target $"]
           [:th.text-end.d-none.d-lg-table-cell  "Current %"]
           [:th.text-end.d-none.d-lg-table-cell  "Current $"]
           [:th.text-end.text-nowrap "Adj. $"]]]
         [:tbody
          (if (seq @allocations)
            (->> @allocations
                 (map #(asset-allocation-row % account hide-zero-balances?))
                 doall)
            [:tr [:td {:col-span 6} "This account has no child accounts."]])]
         [:tfoot
          [:tr
           [:td (html/space)]
           [:td {:col-span 5 :class @total-percent-class} (format-percent @total-percent)]]]]
        [:div
         [button
          {:html {:title "Click here to save these allocations."
                  :type :submit
                  :class "btn btn-primary"}
           :icon :check
           :caption "Save"}]
         [button
          {:html {:title "Click here to to cancel and return to the account list."
                  :type :button
                  :class "btn btn-secondary ms-2"
                  :on-click on-cancel}
           :icon :x-circle
           :caption "Cancel"}]]]])))
