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

(def ^:private *accounts* (r/atom []))

(defn- delete
  [account accounts]
  (js/alert "not implemented."))

(defn- account-row
  [account depth]
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
                  #(delete account *accounts*)
                  {:icon :remove
                   :class "btn btn-danger btn-xs"
                   :title "Click here to remove this account."})]]])

(defn- account-and-child-rows
  ([account]
   (account-and-child-rows account 0 []))
  ([account depth result]
   (-> []
       (conj (account-row account depth))
       (concat (map #(account-and-child-rows % (+ 1 depth) result)
                    (:children account))))))

(defn- account-type-rows
  [{:keys [type accounts] :as group}]
  (-> '()
      (conj ^{:key (str "account-type" type)}
            [:tr.account-type {:id (str "account-type-" type)}
             [:td type]
             [:td (util/space)]])
      (concat (mapcat #(account-and-child-rows %) accounts))))

(defn- account-list
  []
  [:table.table.table-striped.table-hover
   [:tbody
    [:tr
     [:th "Name"]
     [:th (util/space)]]
    (for [row (mapcat #(account-type-rows %) (nest @*accounts*))]
      row)]])

(defn- accounts-page []
  (accounts/get-all (:id @state/current-entity)
                    #(reset! *accounts* %)
                    notify/danger)
  (with-layout
    [:section
     [:h1 "Accounts"]
     [account-list]
     (util/add-button "/accounts/new")]))

(defn- accounts-source
  [text]
  (let [result (->> @*accounts*
                    (filter #(not= -1 (-> (:name %)
                                          (.toLowerCase)
                                          (.indexOf text))))
                    (mapv (juxt :name :id)))]
    result))

(def ^:private account-form
  [:form
   (text-input :name :required)
   [:div.form-group
    [:label.control-label {:for :parent-id} "Parent"]
    [:div {:field :typeahead
           :id :parent-id
           :input-class "form-control"
           :list-class "typeahead-list"
           :item-class "typeahead-item"
           :data-source accounts-source
           :in-fn (fn [id]
                    ((juxt :name :id) (->> @*accounts*
                                           (filter #(= id (:id %)))
                                           first)))
           :out-fn (fn [[name id]] id)
           :result-fn (fn [[name id]] name)}]]])

(defn- new-account []
  (accounts/get-all (:id @state/current-entity)
                    #(reset! *accounts* %)
                    notify/danger)
  (with-layout
    [:div.row
     [:div.col-md-6
      [:h1 "New Account"]
      (let [account (r/atom {})]
        [bind-fields account-form account])]]))

(secretary/defroute new-account-path "/accounts/new" []
  (r/render [new-account] (app-element)))

(secretary/defroute accounts-path "/accounts" []
  (r/render [accounts-page] (app-element)))
