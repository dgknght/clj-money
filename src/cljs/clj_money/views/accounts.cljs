(ns clj-money.views.accounts
  (:require [clojure.set :refer [rename-keys]]
            [reagent.core :as r]
            [reagent-forms.core :refer [bind-fields]]
            [reagent.format :refer [currency-format]]
            [secretary.core :as secretary :include-macros true]
            [cljs-time.core :as t]
            [cljs-time.format :as f]
            [clj-money.macros :refer-macros [with-retry]]
            [clj-money.api.commodities :as commodities]
            [clj-money.api.accounts :as accounts]
            [clj-money.api.transaction-items :as transaction-items]
            [clj-money.api.transactions :as transactions]
            [clj-money.x-platform.accounts :refer [account-types
                                                   left-side?
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
                                     number-input
                                     select-input
                                     typeahead-input
                                     required]]))

(def ^:private accounts (r/atom []))

(defn- load-accounts []
  (accounts/get-all (:id @state/current-entity)
                    #(reset! accounts (-> % nest unnest))
                    notify/danger))

(def ^:private commodities (r/atom []))

(defn- load-commodities []
  (commodities/get-all (:id @state/current-entity)
                       #(reset! commodities %)
                       notify/danger))

(defn- delete
  [account]
  (when (js/confirm (str "Are you sure you want to delete the account " (:name account) "?"))
    (accounts/delete account
                     #(secretary/dispatch! "/accounts")
                     notify/danger)))

(defn- find-account
  ([account-id]
   (some #(= account-id (:id %)) @accounts)))

(defn- account-expanded?
  [account]
  (::expanded? account))

(defn- account-visible?
  [account]
  (or (not (:parent-id account))
      (::visible? account)))

(defn- toggle-account
  [account]
  (swap! accounts (fn [accounts]
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
   [:thead
    [:tr
     [:th "Name"]
     [:th (util/space)]]]
   [:tbody
    (if (seq @accounts)
      (doall (mapcat #(account-type-rows %) (nest @accounts)))
      [:tr
       [:td {:colSpan 2} [:span.inline-status "Loading..."]]])]])

(defn- accounts-page []
  (load-accounts)
  (load-commodities)
  (with-layout
    [:section
     [:h1 "Accounts"]
     [account-list]
     (util/add-button "/accounts/new")]))

(defn- accounts-source
  [text]
  (->> @accounts
       (filter #(not= -1 (-> (:path %)
                             (.toLowerCase)
                             (.indexOf text))))
       (mapv (juxt :path :id))))

(defn- commodities-source
  [text]
  (->> @commodities
       (filter #(not= -1 (-> (:name %)
                             (.toLowerCase)
                             (.indexOf text))))
       (mapv (juxt :name :id))))

(defn- model-in-fn
  [collection-atom display-field]
  (fn [display-value]
    (if-let [model (->> @collection-atom
                        (filter #(= display-value (get % display-field)))
                        first)]
      ((juxt display-field :id) model)
      [display-value nil])))

(def ^:private account-form
  [:form
   (typeahead-input
     :parent-id
     {:data-source accounts-source
      :input-placeholder "Select an account"
      :in-fn (model-in-fn accounts :path)
      :out-fn (fn [v] (if (iterable? v) (first v) v))
      :result-fn first})
   (select-input :type account-types {:visible? #(nil? (:parent-id %))})
   (text-input :name :required)
   (typeahead-input
     :commodity-id
     {:data-source commodities-source
      :input-placeholder "Select a commodity"
      :in-fn (model-in-fn commodities :name)
      :out-fn (fn [v] (if (iterable? v) (first v) v))
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
  (load-accounts)
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
  [{:keys [transaction account]}]
  [:section
   [:div.pull-right
    (util/button "New"
                 (fn []
                   (reset! transaction

                           ; The simplified form
                           #_{:account-id (:id @account)
                                        :entity-id (:id @state/current-entity)
                                        :transaction-date (f/unparse (f/formatter "M/d/yyyy")
                                                                     (t/today))}

                           ; The full form
                           {:entity-id (:id @state/current-entity)
                            :transaction-date (f/unparse (f/formatter "M/d/yyyy")
                                                                     (t/today))
                            :items [{:account-id (:id @account)
                                     :debit-quantity nil
                                     :credit-quantity nil}]})
                   (with-retry
                     (.focus (.getElementById js/document "transaction-date"))))
                 {:icon :plus
                  :class "btn btn-primary"
                  :title "Click here to create a new transaction for this account."})
    (util/space)
    (util/link-to "Back"
                  "/accounts"
                  {:icon :hand-left
                   :class "btn btn-info"
                   :title "Click here to return to the account list."})]
   [:h1 (:name @account)]])

(defn- item-row
  [item]
  ^{:key (str "item-row-" (:id item))}
  [:tr
   [:td.text-right (util/format-date (:transaction-date item))]
   [:td (:description item)]
   [:td.text-right (currency-format (:polarized-value item))]
   [:td.text-right (currency-format (:balance item))]])

(defn- items-table
  [{:keys [items]}]
  [:table.table.table-striped.table-hover
   [:thead
    [:tr
     [:th.col-sm-2.text-right "Date"]
     [:th.col-sm-6 "Description"]
     [:th.col-sm-2.text-right "Amount"]
     [:th.col-sm-2.text-right "Balance"]]]
   [:tbody
    (if-let [items @items]
      (map item-row items)
      [:tr [:td {:colSpan 4} [:span.inline-status "Loading..."]]])]])

(defn- query-again?
  [items]
  (and (< (count items) 50)
       (not= 0 (-> items last :index))))

(defn- next-query-range
  [{:keys [account]} [prev-start]]
  (let [latest (:latest-transaction-date account)
        start (if prev-start
                (t/minus prev-start (t/months 3))
                (t/first-day-of-the-month (or latest
                                              (t/today))))]
    [start (-> start (t/plus (t/months 2)) t/last-day-of-the-month)]))

(defn- get-items
  ([context] (get-items context nil))
  ([{:keys [account items] :as context} prev-date-range]
   (let [[start end :as date-range] (next-query-range context prev-date-range)]
     (transaction-items/search
       {:account-id (:id @account)
        :transaction-date [:between start end]}
       (fn [result]
         (swap! items
                (fnil concat [])
                (map #(polarize-item % @account) result))
         (when (query-again? @items)
           (get-items context date-range)))
       notify/danger))))

(defn- reformat-date
  [date-str]
  (->> date-str
       (f/parse (f/formatter "M/d/yyyy"))
       (f/unparse (:date f/formatters))))

(defn- find-account-by-path
  [path]
  (->> @accounts
       (filter #(= (:path %) path))
       first
       :id))

(defn- transform-transaction
  [{:keys [quantity
           other-account-id]
    :as quick-entry-trx}
   context]
  (let [account @(:account context)
        rename-map (if (or (and (> quantity 0)
                                (left-side? account))
                           (and (< quantity 0)
                                (not (left-side? account))))
                     {:account-id :debit-account-id
                      :other-account-id :credit-account-id}
                     {:account-id :credit-account-id
                      :other-account-id :debit-account-id})]
    (-> quick-entry-trx
        (update-in [:other-account-id] find-account-by-path)
        (update-in [:quantity] Math/abs)
        (update-in [:transaction-date] reformat-date)
        (rename-keys rename-map))))

(defn- handle-saved-transaction
  [_ {:keys [transaction items] :as context}]
  (reset! transaction nil)
  (reset! items nil)
  (get-items context))

(defn- save-transaction
  [{:keys [transaction] :as context}]
  (-> @transaction
      (transform-transaction context)
      (transactions/create #(handle-saved-transaction % context)
                           notify/danger)))

(defn- item-input-row
  [index form-state]
  ^{:key (str "item-form-" index)}
  [:tr {:class (when (>= index (:visible-row-count @form-state))  "hidden")}
   [:td "account control goes here"]
   [:td [:input.form-control {:field :numeric
                              :id [:items index :credit-quantity]}]]
   [:td [:input.form-control {:field :numeric
                              :id [:items index :debit-quantity]}]]])

(defn- trx-form
  [form-state]
  [:form
   (text-input :transaction-date :required)
   (text-input :description :required)

   (.log js/console "trx-form " (prn-str @form-state))

   ; full
   [:table.table
    [:thead
     [:tr
      [:td "Account"]
      [:td "Credit Amount"]
      [:td "Debit Amount"]]]
    [:tbody
     (->> (range 10)
          (map #(item-input-row % form-state))
          doall)]]

   ; simplified
   #_(number-input :quantity :required)
   #_(typeahead-input
       :other-account-id
       {:data-source accounts-source
        :input-placeholder "Select the other account"
        :in-fn (model-in-fn accounts :path)
        :out-fn (fn [v] (if (iterable? v) (first v) v))
        :result-fn (fn [[path id]] path)})])

(defn- transaction-form
  [{:keys [transaction] :as context}]
  (when @transaction
    (let [form-state (r/atom {:visible-row-count 1})]
      [:div.panel.panel-primary
       [:div.panel-heading
        [:h2.panel-title (if (:id @transaction)
                           "Edit Transaction"
                           "New Transaction")]]
       [:div.panel-body
        [bind-fields
         (trx-form form-state)
         transaction
         (fn [path value {:keys [items] :as doc}]

           (swap! form-state assoc :last-update (t/now))
           (.log js/console "updated form state " (prn-str @form-state))

           (let [filled (->> items
                             (filter #(some % [:debit-quantity :credit-quantity]))
                             count)
                 visible (:visible-row-count @form-state)]
             (when (>= filled visible)
               (swap! form-state update-in [:visible-row-count] inc))
             nil))]
        (util/button "Save"
                     #(save-transaction context)
                     {:class "btn btn-primary"
                      :icon :ok
                      :title "Click here to save the transaction"})]])))

(defn- show-account
  [id]
  (let [context {:account (r/atom nil)
                 :transaction (r/atom nil)
                 :items (r/atom nil)}]
    (load-accounts)
    (accounts/get-one id
                      (fn [a]
                        (update-in context [:account] #(reset! % a))
                        (get-items context))
                      notify/danger)
    (with-layout
      [:section
       [:div.row
        [:div.col-md-12
         [account-header context]]]
       [:div.row
        [:div.col-md-6
         [transaction-form context]]
        [:div.col-md-6
         [:div.panel.panel-default
          [:div.panel-heading
           [:h2.panel-title "Transaction Items"]]
          [:div.panel-body {:style {:height "40em" :overflow "auto"}}
           [items-table context]]]]]])))

(secretary/defroute new-account-path "/accounts/new" []
  (r/render [new-account] (app-element)))

(secretary/defroute edit-account-path "/accounts/:id/edit" [id]
  (r/render [edit-account id] (app-element)))

(secretary/defroute show-account-path "/accounts/:id" [id]
  (r/render [show-account id] (app-element)))

(secretary/defroute accounts-path "/accounts" []
  (r/render [accounts-page] (app-element)))
