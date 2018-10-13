(ns clj-money.views.accounts
  (:require [reagent.core :as r]
            [reagent-forms.core :refer [bind-fields]]
            [reagent.format :refer [currency-format]]
            [secretary.core :as secretary :include-macros true]
            [cljs-time.core :as t]
            [cljs-time.format :as f]
            [clj-money.api.commodities :as commodities]
            [clj-money.api.accounts :as accounts]
            [clj-money.api.transaction-items :as transaction-items]
            [clj-money.x-platform.accounts :refer [account-types
                                                   nest
                                                   unnest
                                                   polarize-item]]
            [clj-money.state :as state]
            [clj-money.notifications :as notify]
            [clj-money.dom :refer [app-element]]
            [clj-money.layout :refer [with-layout]]
            [clj-money.x-platform.util :refer [desc-periodic-seq]]
            [clj-money.util :as util]
            [clj-money.forms :refer [text-input
                                     select-input
                                     typeahead-input
                                     required]]))

(def ^:private *accounts* (r/atom []))
(def ^:private *commodities* (r/atom []))
#_(def ^:private *expansion-state* (r/atom {}))

(defn- delete
  [account]
  (when (js/confirm (str "Are you sure you want to delete the account " (:name account) "?"))
    (accounts/delete account
                     #(secretary/dispatch! "/accounts")
                     notify/danger)))

(defn- find-account
  ([account-id]
   (some #(= account-id (:id %)) @*accounts*)))

(defn- account-expanded?
  [account]
  (::expanded? account))

(defn- account-visible?
  [account]
  (or (not (:parent-id account))
      (::visible? account)))

(defn- toggle-account
  [account]
  (swap! *accounts* (fn [accounts]
                      (map (fn [a]
                             (cond
                               (= (:id account) (:id a))
                               (update-in a [::expanded?] not)

                               (= (:id account) (:parent-id a))
                               (update-in a [::visible?] not)

                               :else
                               a))
                           accounts))))

(defn- account-row
  [account depth]
  ^{:key (str "account-" (:id account))}
  [:tr {:class (if (account-visible? account) nil "hidden")}
   [:td [:span {:class (str "account-depth-" depth)}
         [:span.toggle-ctl.glyphicon {:aria-hidden true
                                      :on-click #(toggle-account account)
                                      :class [(if (account-expanded? account)
                                                "glyphicon-collapse-up"
                                                "glyphicon-expand")
                                              (if (seq (:children account))
                                                nil
                                                "invisible")]}]
         (:name account)]]
   [:td
    [:div.btn-group
     (util/link-to nil
                   (util/path :accounts (:id account))
                   {:icon :list-alt
                    :class "btn btn-default btn-xs"
                    :title "Click here to view transactions for this account."})
     (util/link-to nil
                   (util/path :accounts (:id account) :edit)
                   {:icon :pencil
                    :class "btn btn-info btn-xs"
                    :title "Click here to edit this account."})
     (util/button nil
                  #(delete account)
                  {:icon :remove
                   :class "btn btn-danger btn-xs"
                   :title "Click here to remove this account."})]]])

(defn- account-and-child-rows
  ([account]
   (account-and-child-rows account 0 []))
  ([account depth result]
   (-> []
       (conj (account-row account depth))
       (concat (mapv #(account-and-child-rows % (+ 1 depth) result)
                     (:children account))))))

(defn- account-type-rows
  [{:keys [type accounts] :as group}]
  (-> '()
      (conj ^{:key (str "account-type" type)}
            [:tr.account-type {:id (str "account-type-" type)}
             [:td {:colSpan 2} type]])
      (concat (mapcat #(account-and-child-rows %) accounts))))

(defn- account-list
  []
  [:table.table.table-striped.table-hover
   [:tbody
    [:tr
     [:th "Name"]
     [:th (util/space)]]
    (if (seq @*accounts*)
      (doall (mapcat #(account-type-rows %) (nest @*accounts*)))
      [:tr
       [:td {:colSpan 2} [:span.inline-status "Loading..."]]])]])

(defn- accounts-page []
  (accounts/get-all (:id @state/current-entity)
                    #(reset! *accounts* (-> % nest unnest))
                    notify/danger)
  (commodities/get-all (:id @state/current-entity)
                       #(reset! *commodities* %)
                       notify/danger)
  (with-layout
    [:section
     [:h1 "Accounts"]
     [account-list]
     (util/add-button "/accounts/new")]))

(defn- accounts-source
  [text]
  (->> @*accounts*
       (filter #(not= -1 (-> (:path %)
                             (.toLowerCase)
                             (.indexOf text))))
       (mapv (juxt :path :id))))

(defn- commodities-source
  [text]
  (->> @*commodities*
       (filter #(not= -1 (-> (:name %)
                             (.toLowerCase)
                             (.indexOf text))))
       (mapv (juxt :name :id))))

(defn- model-in-fn
  [collection-atom display-field]
  (fn [id]
    ((juxt display-field :id) (->> @collection-atom
                                   (filter #(= id (:id %)))
                                   first))))

(def ^:private account-form
  [:form
   (typeahead-input
     :parent-id
     {:data-source accounts-source
      :input-placeholder "Select an account"
      :in-fn (model-in-fn *accounts* :path)
      :out-fn second
      :result-fn first})
   (select-input :type account-types {:visible? #(nil? (:parent-id %))})
   (text-input :name :required)
   (typeahead-input
     :commodity-id
     {:data-source commodities-source
      :input-placeholder "Select a commodity"
      :in-fn (model-in-fn *commodities* :name)
      :out-fn second
      :result-fn first})])

(defn- create-account
  [{:keys [parent-id] :as account}]
  (let [parent (find-account parent-id)]
    (accounts/create (if parent
                       (assoc account :type (:type parent))
                       account)
                     #(secretary/dispatch! "/accounts")
                     notify/danger)))

(defn- new-account []
  (accounts/get-all (:id @state/current-entity)
                    #(reset! *accounts* (-> % nest unnest))
                    notify/danger)

  (let [account (r/atom {:entity-id (:id @state/current-entity)})]
    (with-layout
      [:div.row
       [:div.col-md-6
        [:h1 "New Account"]
        [bind-fields account-form account]
        (util/button "Save"
                     #(create-account @account)
                     {:class "btn btn-primary"
                      :title "Click here to create the account."
                      :icon :ok})
        (util/space)
        (util/link-to "Cancel"
                      "/accounts"
                      {:class "btn btn-danger"
                       :title "Click here to return to the list of accounts."
                       :icon :ban-circle})]])))

(defn- update-account
  [account]
  (accounts/update account
                   #(secretary/dispatch! "/accounts")
                   notify/danger))

(defn- edit-account [id]
  (let [account (r/atom {:entity-id (:id @state/current-entity)})]
    (accounts/get-one id #(reset! account %) notify/danger)
    (with-layout
      [:div.row
       [:div.col-md-6
        [:h1 "Edit Account"]
        [bind-fields account-form account]
        (util/button "Save"
                     #(update-account @account)
                     {:class "btn btn-primary"
                      :title "Click here to save the account."
                      :icon :ok})
        (util/space)
        (util/link-to "Cancel"
                      "/accounts"
                      {:class "btn btn-danger"
                       :title "Click here to return to the list of accounts."
                       :icon :ban-circle})]])))

(defn- account-header
  [account]
  [:section
   [:div.pull-right
    [:a.btn.btn-info
     {:href "/accounts"
      :title "Click here to return to the account list."}
     "Back"]]
   [:h1 (:name @account)]])

(defn- item-row
  [item]
  ^{:key (:id item)}
  [:tr
   [:td.text-right (util/format-date (:transaction-date item))]
   [:td (:description item)]
   [:td.text-right (currency-format (:polarized-value item))]
   [:td.text-right (currency-format (:balance item))]])

(defn- items-table
  [items]
  [:table.table.table-striped.table-hover
   [:tbody
    [:tr
     [:th.col-sm-2.text-right "Date"]
     [:th.col-sm-6 "Description"]
     [:th.col-sm-2.text-right "Amount"]
     [:th.col-sm-2.text-right "Balance"]]
    (if @items
      (map item-row @items)
      [:tr [:td {:colSpan 4} [:span.inline-status "Loading..."]]])]])

(defn- query-ranges []
  (map #(vector % (t/last-day-of-the-month %))
       (desc-periodic-seq (t/first-day-of-the-month (t/today))
                          (t/months 1))))

(defn- query-items
  [account-id items]
  (->> (query-ranges)
       (map #(.log js/console %))
       (take 4)))

(defn- show-account [id]
  (let [account (r/atom {})
        transaction-items (r/atom nil)]
    (accounts/get-one id
                      (fn [a]
                        (reset! account a)
                        (transaction-items/search
                          {:account-id (:id a)}
                          (fn [items]
                            (reset! transaction-items
                                    (map #(polarize-item % a)
                                         items)))
                          notify/danger))
                      notify/danger)

    (with-layout
      [:div.row
       [:div.col-md-6
        [account-header account]
        [items-table transaction-items]]])))

(secretary/defroute new-account-path "/accounts/new" []
  (r/render [new-account] (app-element)))

(secretary/defroute edit-account-path "/accounts/:id/edit" [id]
  (r/render [edit-account id] (app-element)))

(secretary/defroute show-account-path "/accounts/:id" [id]
  (r/render [show-account id] (app-element)))

(secretary/defroute accounts-path "/accounts" []
  (r/render [accounts-page] (app-element)))
