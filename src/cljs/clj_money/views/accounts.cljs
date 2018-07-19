(ns clj-money.views.accounts
  (:require [reagent.core :as r]
            [reagent-forms.core :refer [bind-fields]]
            [secretary.core :as secretary :include-macros true]
            [clj-money.api.accounts :as accounts]
            [clj-money.x-platform.accounts :refer [nest]]
            [clj-money.state :as state]
            [clj-money.notifications :as notify]
            [clj-money.dom :refer [app-element]]
            [clj-money.layout :refer [with-layout]]
            [clj-money.util :as util]
            [clj-money.forms :refer [text-input
                                     select-input
                                     required]]))

(defn- delete
  [account accounts]
  (js/alert "not implemented."))

(defn- account-row
  [account depth accounts]
  ^{:key (str "account-" (:id account))}
  [:tr
   [:td [:span {:class (str "account-depth-" depth)}
         (:name account)]]
   [:td
    [:div.btn-group
     (util/link-to nil
                   (util/path :accounts (:id account) :edit)
                   {:icon :pencil
                    :class "btn btn-info btn-xs"
                    :title "Click here to edit this account."})
     (util/button nil
                  #(delete account accounts)
                  {:icon :remove
                   :class "btn btn-danger btn-xs"
                   :title "Click here to remove this account."})]]])

(defn- account-and-child-rows
  ([account accounts]
   (account-and-child-rows account accounts 0 []))
  ([account accounts depth result]
   (-> []
       (conj (account-row account depth accounts))
       (concat (map #(account-and-child-rows % accounts (+ 1 depth) result)
                    (:children account))))))

(defn- account-type-rows
  [{:keys [type accounts] :as group} all-accounts]
  (-> '()
      (conj ^{:key (str "account-type" type)}
            [:tr.account-type {:id (str "account-type-" type)}
             [:td type]
             [:td (util/space)]])
      (concat (mapcat #(account-and-child-rows % all-accounts) accounts))))

(defn- account-list
  [accounts]
  [:table.table.table-striped.table-hover
   [:tbody
    [:tr
     [:th "Name"]
     [:th (util/space)]]
    (for [row (mapcat #(account-type-rows % accounts) (nest @accounts))]
      row)]])

(defn- accounts-page []
  (with-layout
    [:section
     [:h1 "Accounts"]
     (let [accounts (r/atom [])]
       (accounts/get-all (:id @state/current-entity)
                         #(reset! accounts %)
                         notify/danger)
       [account-list accounts])
     (util/add-button "/accounts/new")]))

(secretary/defroute accounts-path "/accounts" []
  (r/render [accounts-page] (app-element)))
