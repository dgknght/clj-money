(ns clj-money.views.accounts
  (:require [clojure.set :refer [rename-keys]]
            [reagent.core :as r]
            [reagent-forms.core :refer [bind-fields]]
            [reagent.format :refer [currency-format]]
            [secretary.core :as secretary :include-macros true]
            [cljs.core.async :refer [chan <! >! go-loop go]]
            [cljs-time.core :as t]
            [cljs-time.format :as f]
            [clj-money.macros :refer-macros [with-retry]]
            [clj-money.components :refer [load-on-scroll]]
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

(defn- new-transaction
  [{:keys [transaction account]} trx-type]
  (reset! transaction
          (cond-> {:entity-id (:id @state/current-entity)
                   :transaction-date (f/unparse (f/formatter "M/d/yyyy")
                                                (t/today))}
            (= :full trx-type)
            (assoc :items [{:account-id (:id @account)
                            :debit-quantity nil
                            :credit-quantity nil}])

            (= :simple trx-type)
            (assoc :account-id (:id @account)
                   :entity-id (:id @state/current-entity)
                   :transaction-date (f/unparse (f/formatter "M/d/yyyy")
                                                (t/today)))))
  (with-retry
    (.focus (.getElementById js/document "transaction-date"))))

(defn- account-buttons
  [{:keys [transaction] :as context}]
  [:div
   [:div.btn-group.dropup
    [:button {:type :button
              :class ["btn"
                      "btn-primary"
                      "dropdown-toggle"]
              :data-toggle :dropdown
              :aria-haspopup true
              :aria-expanded false
              :disabled (not (nil? @transaction))}
     [:span.glyphicon.glyphicon-plus]
     (util/space)
     "New"
     (util/space)
     [:span.caret]]
    [:ul.dropdown-menu
     [:li
      [:a {:href "#"
           :on-click #(new-transaction context :simple)}
       "Simple Entry"]
      [:a {:href "#"
           :on-click #(new-transaction context :full)}
       "Full Entry"]]]]
   (util/space)
   (util/link-to "Back"
                 "/accounts"
                 {:icon :hand-left
                  :class "btn btn-info"
                  :title "Click here to return to the account list."})])

(defn- find-account-by-id
  [id]
  (->> @accounts
       (filter #(= (:id %) id))
       first
       :path))

(defn- find-account-by-path
  [path]
  (->> @accounts
       (filter #(= (:path %) path))
       first
       :id))

(defn- prepare-transaction-item-for-edit
  [item]
  (-> item
      (assoc :debit-quantity (when (= :debit (:action item))
                               (:quantity item))
             :credit-quantity (when (= :credit (:action item))
                                (:quantity item)))
      (update-in [:account-id] find-account-by-id)
      (select-keys [:id :account-id :debit-quantity :credit-quantity])))

(defn- prepare-transaction-items-for-edit
  [items]
  (->> items
       (map prepare-transaction-item-for-edit)
       (into [])))

(defn- prepare-transaction-for-edit
  [transaction]
  (-> transaction
      (update-in [:transaction-date]
                 #(f/unparse-local-date
                    (f/formatter "M/d/yyyy")
                    (f/parse-local-date (:date f/formatters) %)))
      (update-in [:items] prepare-transaction-items-for-edit)))

(defn- item->tkey
  [item]
  (-> item
      (select-keys [:transaction-id :transaction-date])
      (rename-keys {:transaction-id :id})))

(defn- edit-transaction
  [item {:keys [transaction]}]
  (transactions/get-one (item->tkey item)
                        (fn [result]
                          (let [prepared (prepare-transaction-for-edit result)]
                            (reset! transaction prepared))
                          (with-retry
                            (.focus (.getElementById js/document "transaction-date"))))
                        notify/danger))

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

(defn- init-item-loading
  [{:keys [account ctl-chan items] :as context}]
  (let [end (-> @account :latest-transaction-date t/first-day-of-the-month)
        start (-> @account :earliest-transaction-date t/first-day-of-the-month)
        items-chan (chan)]

    ; handle items received on the items channel
    (go-loop [call-count 0]
             (when-let [received (<! items-chan)]
               (swap! items #((fnil concat []) % received))
               (recur (inc call-count))))

    ; response to requests for more items by querying
    ; the service and putting the retrieved items
    ; on the items channel
    (go-loop [date-ranges (->> (desc-periodic-seq start end (t/months 1))
                               (map #(vector :between % (t/last-day-of-the-month %))))]
             (let [action (<! ctl-chan) ; action is either :fetch or the minimum number of items we want to fetch before we pause
                   count-needed (if (number? action)
                                  action
                                  50)]
               (when (not= :quit action)
                 (transaction-items/search
                   {:account-id (:id @account)
                    :transaction-date (first date-ranges)}
                   #(go
                      (>! items-chan %)
                      (when (< (count %)
                               count-needed)
                        (>! ctl-chan (- count-needed (count %)))))
                   notify/danger))
               (if (and (not= :quit action)
                        (seq (rest date-ranges)))
                 (recur (rest date-ranges))
                 (reset! (:more-items? context) false))))

    ; Get the first batch
    (go (>! ctl-chan :fetch))))

(defn- reset-item-loading
  [context]
  (reset! (:items context) nil)
  (reset! (:transaction context) nil)
  (go (>! (:ctl-chan context) :quit))
  (init-item-loading context))

(defn- delete-transaction
  [item context]
   (transactions/delete (item->tkey item)
                        (fn [& _]
                          (reset-item-loading context))
                        notify/danger))

(defn- item-row
  [item context]
  ^{:key (str "item-row-" (:id item))}
  [:tr
   [:td.text-right (util/format-date (:transaction-date item))]
   [:td (:description item)]
   [:td.text-right (currency-format (:polarized-quantity item))]
   [:td.text-right (currency-format (:balance item))]
   [:td
    [:div.btn-group
     (util/button nil
                  #(edit-transaction item context)
                  {:icon :pencil
                   :class "btn btn-info btn-xs"
                   :title "Click here to edit this transaction."})
     (util/button nil
                  #(when (js/confirm "Are you sure you want to delete this transaction?")
                     (delete-transaction item context))
                  {:icon :remove
                   :class "btn btn-danger btn-xs"
                   :title "Click here to remove this transaction."})]]])

(defn- items-table
  [{:keys [items] :as context}]
  [:table.table.table-striped.table-hover
   [:thead
    [:tr
     [:th.col-sm-2.text-right "Date"]
     [:th.col-sm-3 "Description"]
     [:th.col-sm-2.text-right "Amount"]
     [:th.col-sm-2.text-right "Balance"]
     [:th.col-sm-2 (util/space)]]]
   [:tbody
    (if @items
      (map #(item-row % context) @items)
      [:tr [:td {:colSpan 4} [:span.inline-status "Loading..."]]])]])

(defn- reformat-date
  [date-str]
  (->> date-str
       (f/parse (f/formatter "M/d/yyyy"))
       (f/unparse (:date f/formatters))))

(defn- refine-item
  [item]
  (-> item
      (update-in [:account-id] find-account-by-path)
      (assoc :quantity (some item [:debit-quantity :credit-quantity]))
      (assoc :action (if (:debit-quantity item)
                       :debit
                       :credit))
      (dissoc :debit-quantity :credit-quantity)))

(defn- handle-saved-transaction
  [_  context]
  (reset-item-loading context))

(defmulti ^:private prepare-transaction-for-save
  (fn [transaction _]
    (if (:items transaction)
      :full
      :simple)))

(defmethod ^:private prepare-transaction-for-save :simple
  [{:keys [quantity] :as transaction} account]
  (let [rename-map (if (or (and (> quantity 0)
                                (left-side? account))
                           (and (< quantity 0)
                                (not (left-side? account))))
                     {:account-id :debit-account-id
                      :other-account-id :credit-account-id}
                     {:account-id :credit-account-id
                      :other-account-id :debit-account-id})]
    (-> transaction
        (update-in [:other-account-id] find-account-by-path)
        (update-in [:quantity] Math/abs)
        (update-in [:transaction-date] reformat-date)
        (rename-keys rename-map))))

(defmethod ^:private prepare-transaction-for-save :full
  [transaction _]
  (-> transaction
      (update-in [:transaction-date] reformat-date)
      (update-in [:items] (fn [items]
                            (->> items
                                 (filter #(some % [:debit-quantity
                                                   :credit-quantity]))
                                 (map refine-item)
                                 (into []))))))

(defn- save-transaction
  [{:keys [transaction account] :as context}]
  (let [save-fn (if (:id @transaction)
                  transactions/update
                  transactions/create)]
    (-> @transaction
        (prepare-transaction-for-save @account)
        (save-fn #(handle-saved-transaction % context)
                 notify/danger))))

(defn- item-input-row
  [index]
  ^{:key (str "item-form-" index)}
  [:tr
   [:td [:div {:field :typeahead
               :id [:items index :account-id]
               :input-class "form-control"
               :list-class "typeahead-list"
               :item-class "typeahead-item"
               :highlight-class "typeahead-highlight"
               :clear-on-focus? false
               :data-source accounts-source
               :input-placeholder "Select the account"
               :in-fn (model-in-fn accounts :path)
               :out-fn (fn [v] (if (iterable? v) (first v) v))
               :result-fn (fn [[path id]] path)}]]
   [:td [:input.form-control {:field :text
                              :id [:items index :memo]}]]
   [:td [:input.form-control {:field :numeric
                              :id [:items index :credit-quantity]}]]
   [:td [:input.form-control {:field :numeric
                              :id [:items index :debit-quantity]}]]])

(def ^:private simple-transaction-form
  [:form
   (text-input :transaction-date :required)
   (text-input :description :required)
   (number-input :quantity :required)
   (typeahead-input
     :other-account-id
     {:data-source accounts-source
      :input-placeholder "Select the other account"
      :in-fn (model-in-fn accounts :path)
      :out-fn (fn [v] (if (iterable? v) (first v) v))
      :result-fn (fn [[path id]] path)})])

(def ^:private full-transaction-form
  [:form
   (text-input :transaction-date :required)
   (text-input :description :required)
   [:table.table
    [:thead
     [:tr
      [:td "Account"]
      [:td "Memo"]
      [:td "Credit Amount"]
      [:td "Debit Amount"]]]
    [:tbody
     (->> (range 4)
          (map item-input-row)
          doall)]]])

(defn- transaction-form
  [{:keys [transaction] :as context}]
  (when @transaction
    [:div.panel.panel-primary
     [:div.panel-heading
      [:h2.panel-title (if (:id @transaction)
                         "Edit Transaction"
                         "New Transaction")]]
     [:div.panel-body
      [bind-fields
       (if (:items @transaction)
         full-transaction-form
         simple-transaction-form)
       transaction]
      (util/button "Save"
                   #(save-transaction context)
                   {:class "btn btn-primary"
                    :icon :ok
                    :title "Click here to save the transaction"})
      (util/space)
      (util/button "Cancel"
                   #(reset! transaction nil)
                   {:class "btn btn-danger"
                    :icon :remove
                    :title "Click here to cancel this transaction"})]]))

(defn- account-header
  [{:keys [account]}]
  [:h1 (:name @account)])

(defn- show-account
  [id]
  (let [{:keys [items
                more-items?
                ctl-chan]
         :as context} {:account (r/atom nil)
                       :transaction (r/atom nil)
                       :ctl-chan (chan)
                       :more-items? (atom true)
                       :items (r/atom nil)}]
    (load-accounts)
    (accounts/get-one id
                      (fn [a]
                        (update-in context [:account] reset! a)
                        (init-item-loading context))
                      notify/danger)
    (with-layout
      [:section
       [:div.row
        [:div.col-md-12
         [account-header context]]]
       [:div.row
        [:div.col-md-6
         [:div.panel.panel-default
          [:div.panel-heading
           [:h2.panel-title "Transaction Items"]]
          [:div#items-container.panel-body {:style {:height "40em" :overflow "auto"}}
           [items-table context]]
          [:div.panel-footer
           [load-on-scroll {:target "items-container"
                            :can-load-more? (fn [] @more-items?)
                            :load-fn #(go (>! ctl-chan :fetch))}]]]
         [account-buttons context]]
        [:div.col-md-6
         [transaction-form context]]]])))

(secretary/defroute new-account-path "/accounts/new" []
  (r/render [new-account] (app-element)))

(secretary/defroute edit-account-path "/accounts/:id/edit" [id]
  (r/render [edit-account id] (app-element)))

(secretary/defroute show-account-path "/accounts/:id" [id]
  (r/render [show-account id] (app-element)))

(secretary/defroute accounts-path "/accounts" []
  (r/render [accounts-page] (app-element)))
