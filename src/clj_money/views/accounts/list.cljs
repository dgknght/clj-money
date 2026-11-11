(ns clj-money.views.accounts.list
  "The entity's accounts, grouped by type and nested beneath their parents,
  with buttons to act on each. Deleting and reindexing an account happen
  here, and Reconciliations links to the reconciliations page; the other
  buttons call back to the caller with the account:

  [accounts-table :filter ... :expanded ... :bulk-edit ...
   :on-view ... :on-edit ... :on-reconcile ... :on-allocate ...]

  :filter holds the options of clj-money.views.accounts.filter, :expanded is
  an atom holding the set of the ids of the expanded accounts (and the
  account types), and :bulk-edit is an atom (or cursor) whose :account-ids
  are the ids of the accounts selected for a bulk edit."
  (:require [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [reagent.format :refer [currency-format]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.decimal :as decimal]
            [dgknght.app-lib.notifications :as notify]
            [dgknght.app-lib.forms :as forms]
            [clj-money.icons :refer [icon]]
            [clj-money.api.accounts :as accounts]
            [clj-money.cached-accounts :refer [fetch-accounts
                                               latest]]
            [clj-money.accounts :refer [account-types
                                        system-tagged?]]
            [clj-money.state :as state :refer [+busy
                                               -busy]]
            [clj-money.views.accounts.filter :refer [account-filter-fn]]
            [clj-money.views.reconciliations :as recs]))

(defn- delete
  [account]
  (when (js/confirm (str "Are you sure you want to delete the account " (:account/name account) "?"))
    (+busy)
    (accounts/delete account
                     :callback -busy
                     :on-success fetch-accounts)))

(defn- recalculate
  [account recalculating]
  (+busy)
  (swap! recalculating conj (:id account))
  (accounts/recalculate account
                        :callback (comp -busy
                                        #(swap! recalculating disj (:id account)))
                        :on-success (fn [& _]
                                      (notify/toast "Reindexing Finished"
                                                    (str "The account \""
                                                         (:account/name account)
                                                         "\" has finished."))
                                      (fetch-accounts))))

(defn- toggle-account
  [id expanded]
  (swap! expanded (fn [ids]
                    (if (ids id)
                      (disj ids id)
                      (conj ids id)))))

(defn- account-hidden?
  [{:account/keys [parent-ids] :as account} expanded hide-zero-balances?]
  (or (and hide-zero-balances?
           (decimal/zero? (decimal/+ (:account/value account)
                                     (:account/children-value account))))
      (not (expanded (:account/type account)))
      (and (seq parent-ids)
           (not-every? expanded parent-ids))))

(defn- abbr-acct-name
  [account]
  [:span.text-truncate {:title (:account/name account)
                        :data-bs-toggle :tooltip
                        :data-bs-placement :top}
   (:account/name account)])

(defn- menu-item
  [{:keys [title on-click href]} icon-elem caption]
  [:li
   [:a.dropdown-item
    {:title title
     :href (or href "#")
     :on-click on-click}
    icon-elem
    [:span.ms-2 caption]]])

(defn- account-row-buttons
  [account {:keys [recalculating
                   on-view
                   on-edit
                   on-reconcile
                   on-allocate]}]
  [:div.d-flex
   [:div.btn-group
    [:button.btn.btn-info.btn-sm
     {:on-click #(on-view account)
      :title "Click here to view transactions for this account."}
     (icon :collection :size :small)]
    [:button.btn.btn-secondary.btn-sm
     {:on-click #(on-edit (latest account))
      :title "Click here to edit this account."}
     (icon :pencil :size :small)]]
   [:div.dropdown.ms-2
    [:button.btn.btn-dark
     {:type :button
      :data-bs-toggle :dropdown
      :aria-expanded "false"}
     (icon :three-dots-vertical :size :small)]
    [:ul.dropdown-menu
     [menu-item {:title "Click here to reconcile this account"
                 :on-click #(on-reconcile account)}
      (icon :list-check :size :small)
      "Reconcile"]
     [menu-item {:title "Click here to see the reconciliations for this account"
                 ; navigation to a link within the app is handled by accountant
                 :href (recs/list-path account)}
      (icon :journal-check :size :small)
      "Reconciliations"]
     (when (system-tagged? account :trading)
       [menu-item {:title "Click here to manage asset allocation for this account."
                   :on-click #(on-allocate account)}
        (icon :pie-chart :size :small)
        "Allocate"])
     [menu-item {:title "Click here to recalculate the balance and transaction indexes for this account."
                 :on-click #(recalculate (latest account) recalculating)}
      (if (@recalculating (:id account))
        [:div.spinner-border.spinner-border-sm {:role :state}]
        (icon :arrow-clockwise :size :small))
      "Reindex"]
     [menu-item {:title "Click here to remove this account."
                 :on-click #(delete account)}
      (icon :x-circle :size :small)
      "Delete"]]]])

(defn- account-row
  [{:keys [id] :account/keys [parent-ids] :as account} {:keys [expanded bulk-edit] :as opts}]
  ^{:key (str "account-" id)}
  [:tr.align-middle
   [:td [:span.account-depth {:class (str "account-depth-" (count parent-ids))}
         [:span.toggle-ctl {:on-click #(toggle-account id expanded)
                            :class (when-not (:account/has-children? account)
                                     "invisible")}
          (icon (if (@expanded id)
                  :chevron-down
                  :chevron-right)
                :size :small)]
         (abbr-acct-name account)]]
   [:td.text-end.d-none.d-sm-table-cell.value-depth
    [:span {:class (str "value-depth-" (count parent-ids))}
     (currency-format (:account/total-value account))]]
   [:td.text-center.d-none.d-md-table-cell
    [forms/checkbox-input bulk-edit [:account-ids] {:no-bootstrap? true
                                                    :html {:name "bulk-edit-id"}
                                                    :value id}]]
   [:td.text-center [account-row-buttons account opts]]])

(defn- account-type-row
  [account-type group expanded]
  ^{:key (str "account-type" account-type)}
  [:tr.account-type {:id (str "account-type-" account-type)}
   [:td
    [:span.toggle-ctl {:aria-hidden true
                       :on-click #(toggle-account account-type expanded)}
     (icon (if (@expanded account-type)
             :chevron-down
             :chevron-right)
           :size :small)]
    (name account-type)]
   [:td.text-end.d-none.d-sm-table-cell
    (currency-format (->> group
                          (map :account/value)
                          (reduce decimal/+)))]
   [:td {:col-span 2} (html/space)]])

(defn- compare-vec
  [v1 v2]
  (let [r (compare (first v1) (first v2))]
    (if (zero? r)
      (compare-vec (rest v1) (rest v2))
      r)))

(defn- account-rows
  [accounts filter-fn hide-zero-balances? {:keys [expanded] :as opts}]
  (->> accounts
       (sort-by :account/path compare-vec)
       (filter (every-pred filter-fn
                           #(not (account-hidden? % @expanded hide-zero-balances?))))
       (map #(account-row % opts))))

(defn- account-and-type-rows
  [_opts]
  (let [grouped (make-reaction #(group-by :account/type @state/accounts))]
    (fn [{:keys [filter expanded] :as opts}]
      (let [filter-fn (account-filter-fn filter)]
        [:tbody
         (->> account-types
              (keep (fn [account-type]
                      (when-let [group (get @grouped account-type)]
                        [account-type group])))
              (mapcat (fn [[account-type group]]
                        (cons (account-type-row account-type
                                                group
                                                expanded)
                              (account-rows group
                                            filter-fn
                                            (:hide-zero-balances? filter)
                                            opts))))
              doall)]))))

(defn accounts-table
  [& _opts]
  (let [recalculating (r/atom #{})]
    (fn [& {:as opts}]
      [:table.table.table-hover
       [:thead
        [:tr
         [:th.col-md-6 "Name"]
         [:th.col-md-3.text-end.d-none.d-sm-table-cell "Value"]
         [:th.col-md-1.d-none.d-md-table-cell (html/space)]
         [:th.col-md-2 (html/space)]]]
       (cond
         (seq @state/accounts)
         [account-and-type-rows (assoc opts :recalculating recalculating)]

         @state/accounts
         [:tbody
          [:tr
           [:td {:col-span 4} "No accounts"]]]

         :else
         [:tbody
          [:tr
           [:td {:col-span 4}
            [:div.d-flex.justify-content-center.m2
             [:div.spinner-border {:role :status}
              [:span.visually-hidden "Loading"]]]]]])])))
