(ns clj-money.views.accounts
  (:require [clojure.set :refer [rename-keys]]
            [clojure.string :as string]
            [goog.string :as gstr]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [reagent.format :refer [currency-format]]
            [secretary.core :as secretary :include-macros true]
            [cljs.core.async :refer [chan >! go]]
            [cljs-time.core :as t]
            [clj-money.inflection :refer [humanize]]
            [clj-money.plain-forms :as forms]
            [clj-money.components :refer [load-on-scroll
                                          load-in-chunks]]
            [clj-money.bootstrap :refer [nav-tabs]]
            [clj-money.api.commodities :as commodities]
            [clj-money.api.accounts :as accounts]
            [clj-money.api.transaction-items :as transaction-items]
            [clj-money.api.transactions :as transactions]
            [clj-money.api.lots :as lots]
            [clj-money.api.prices :as prices]
            [clj-money.api.trading :as trading]
            [clj-money.x-platform.transactions :refer [simplify
                                                       fullify
                                                       can-simplify?
                                                       entryfy
                                                       unentryfy]]
            [clj-money.x-platform.accounts :refer [account-types
                                                   polarize-quantity
                                                   nest
                                                   unnest]]
            [clj-money.state :refer [app-state]]
            [clj-money.notifications :as notify]
            [clj-money.x-platform.util :refer [serialize-date]]
            [clj-money.util :as util]))

(defn- load-accounts
  ([page-state] (load-accounts page-state identity))
  ([page-state callback]
   (accounts/get-all (get-in @app-state [:current-entity :id])
                     (fn [result]
                       (swap! page-state assoc
                              :accounts (-> result nest unnest)
                              :hide-zero-balances? (->> result
                                                        (map :value)
                                                        (not-every? #(= 0 %))))
                       (callback))
                     notify/danger)))

(defn- delete
  [account page-state]
  (when (js/confirm (str "Are you sure you want to delete the account " (:name account) "?"))
    (accounts/delete account
                     #(load-accounts page-state)
                     notify/danger)))

(defn- toggle-account
  [id page-state]
  (swap! page-state update-in [:expanded] (fn [expanded]
                                            (if (expanded id)
                                              (disj expanded id)
                                              (conj expanded id)))))

(defn- stop-item-loading
  [page-state]
  (go (>! (:ctl-chan @page-state) :quit)))

(defn- init-item-loading
  [page-state]
  (let [account (:view-account @page-state)
        end (t/last-day-of-the-month (or (:latest-transaction-date account)
                                          (t/today)))] ; This is probably only nil for newly imported entities
    (load-in-chunks
      {:start (t/first-day-of-the-month (or (:earliest-transaction-date account)
                                            (t/minus- end (t/months 6))))
       :end end
       :ctl-chan (:ctl-chan @page-state)
       :fetch-fn (fn [date-range callback-fn]
                            (transaction-items/search
                              {:account-id (:id account)
                               :transaction-date date-range}
                              callback-fn
                              (notify/danger-fn "Unable to fetch transaction items: %s")))
       :receive-fn #(swap! page-state update-in [:items] (fnil concat []) %)
       :finish-fn #(swap! page-state assoc :more-items? false)})))

(defn- account-hidden?
  [{:keys [parents] :as account} expanded hide-zero-balances?]
  (or (and @hide-zero-balances?
           (= 0 (+ (:value account)
                   (:children-value account))))
      (not (@expanded (:type account)))
      (and parents
           (not-every? @expanded parents))))

(defn- account-row
  [{:keys [id parents] :as account} expanded hide-zero-balances? page-state]
  ^{:key (str "account-" id)}
  [:tr {:class (when (account-hidden? account expanded hide-zero-balances?)
                 "hidden")}
   [:td [:span {:class (str "account-depth-" (count parents))}
         [:span.toggle-ctl.glyphicon {:aria-hidden true
                                      :on-click #(toggle-account (:id account) page-state)
                                      :class [(if (@expanded id)
                                                "glyphicon-collapse-up"
                                                "glyphicon-expand")
                                              (when-not (:has-children? account)
                                                "invisible")]}]
         (:name account)]]
   [:td.text-right (currency-format (+ (:children-value account)
                                       (:value account)))]
   [:td.text-center
    [:div.btn-group
     (util/button nil
                  #(swap! page-state assoc :view-account account)
                   {:icon :list-alt
                    :class "btn btn-default btn-xs"
                    :title "Click here to view transactions for this account."})
     (util/button  nil
                  (fn []
                    (swap! page-state assoc :selected account)
                    (util/set-focus "parent-id"))
                   {:icon :pencil
                    :class "btn btn-info btn-xs"
                    :title "Click here to edit this account."})
     (util/button nil
                  #(delete account page-state)
                  {:icon :remove
                   :class "btn btn-danger btn-xs"
                   :title "Click here to remove this account."})]]])

(defn- account-and-type-rows
  [page-state]
  (let [accounts (r/cursor page-state [:accounts])
        expanded (r/cursor page-state [:expanded])
        hide-zero-balances? (r/cursor page-state [:hide-zero-balances?])]
    (fn []
      (let [grouped (group-by :type @accounts)]
        [:tbody
         (doall (mapcat (fn [[account-type group]]
                          (concat [^{:key (str "account-type" account-type)}
                                   [:tr.account-type {:id (str "account-type-" account-type)}
                                    [:td
                                     [:span.toggle-ctl.glyphicon {:aria-hidden true
                                                                  :on-click #(toggle-account account-type page-state)
                                                                  :class [(if (@expanded account-type)
                                                                            "glyphicon-collapse-up"
                                                                            "glyphicon-expand")]}]
                                     (name account-type)]
                                    [:td.text-right (currency-format (->> group
                                                                          (map :value)
                                                                          (reduce +)))]
                                    [:td (util/space)]]]
                                  (doall (map #(account-row % expanded hide-zero-balances? page-state) group))))
                     grouped))]))))

(defn- account-list
  [page-state]
  (let [accounts (r/cursor page-state [:accounts])
        current-entity (r/cursor app-state [:current-entity])]
    (fn []
      [:div
       [:div.accounts-options
        [forms/checkbox-field
         page-state
         :hide-zero-balances?
         {:caption "Hide Zero-Balance Accounts"}]]
       [:table.table.table-striped.table-hover
        [:thead
         [:tr
          [:th.col-md-7 "Name"]
          [:th.col-md-3.text-right "Value"]
          [:th.col-md-2 (util/space)]]]
        (if (seq @accounts)
          [account-and-type-rows page-state]
          [:tbody
           [:tr
            [:td {:col-span 3} [:span.inline-status "Loading..."]]]])]
       (util/button "Add"
                    (fn []
                      (swap! page-state assoc :selected {:entity-id (:id @current-entity)
                                                         :type :asset})
                      (util/set-focus "parent-id"))
                    {:class "btn btn-primary"
                     :icon :plus})])))

(defn- account-fields
  [page-state]
  (let [account (r/cursor page-state [:selected])
        accounts (r/cursor page-state [:accounts])
        commodities (r/cursor page-state [:commodities])]
    (fn []
      [:form
       (forms/typeahead-field
         account
         :parent-id
         {:search-fn (fn [input callback]
                       (let [term (string/lower-case input)]
                         (->> @accounts
                              (filter #(string/includes? (string/lower-case (:path %))
                                                         term))
                              callback)))
          :caption-fn :path
          :value-fn :id
          :find-fn (fn [id callback]
                     (->> @accounts
                          (filter #(= id (:id %)))
                          first
                          callback))})
       (forms/select-field account
                           :type
                           (map (juxt name humanize) account-types)
                           {:class (when (:parent-id @account) "hidden")})
       (forms/text-field account :name {:validate [:required]})
       (forms/typeahead-field
         account
         :commodity-id
         {:search-fn (fn [input callback]
                       (let [term (string/lower-case input)]
                         (->> @commodities
                              (filter #(or (string/includes? (string/lower-case (:name %))
                                                             term)
                                           (string/includes? (string/lower-case (:symbol %))
                                                             term)))
                              callback)))
          :caption-fn :name
          :value-fn :id
          :find-fn (fn [id callback]
                     (->> @commodities
                          (filter #(= id (:id %)))
                          first
                          callback))})
       (forms/checkbox-field
         account
         :trading
         {:caption "Check here if this account is used to trade commodities"})])))

(defn- save-account
  [page-state]
  (accounts/save (:selected @page-state)
                 (fn [_]
                   (swap! page-state dissoc :selected)
                   (load-accounts page-state))
                 notify/danger))

(defn- account-form
  [page-state]
  (let [account (r/cursor page-state [:selected])]
    (fn []
      [:div.row
       [:div.col-md-6
        [:h2 (if (:id @account) "Edit" "New")]
        [account-fields page-state]
        (util/button "Save"
                     #(save-account page-state)
                     {:class "btn btn-primary"
                      :title "Click here to save the account."
                      :icon :ok})
        (util/space)
        (util/button "Cancel"
                     #(swap! page-state dissoc :selected)
                      {:class "btn btn-danger"
                       :title "Click here to return to the list of accounts."
                       :icon :ban-circle})]])))

(defn- prepare-transaction-for-edit
  [transaction account]
  (if (can-simplify? transaction)
      [(simplify transaction account) :simple]
      [(entryfy transaction) :full]))

(defn- item->tkey
  [item]
  (-> item
      (select-keys [:transaction-id :transaction-date])
      (rename-keys {:transaction-id :id})))

(defn- edit-transaction
  [item page-state]
  (transactions/get-one (item->tkey item)
                        (fn [result]
                          (let [[prepared mode] (prepare-transaction-for-edit
                                                  result
                                                  (:view-account @page-state))]
                            (swap! page-state
                                   assoc
                                   :transaction prepared
                                   :transaction-entry-mode mode))
                          (util/set-focus "transaction-date"))
                        notify/danger))

(defn- reset-item-loading
  [page-state]
  (swap! page-state dissoc :items :transaction)
  (stop-item-loading page-state)
  (init-item-loading page-state))

(defn- delete-transaction
  [item page-state]
  (when (js/confirm "Are you sure you want to delete this transaction?")
    (transactions/delete (item->tkey item)
                         #(reset-item-loading page-state)
                         notify/danger)))

(defn- item-row
  [item page-state]
  (let [account (r/cursor page-state [:view-account])]
    ^{:key (str "item-row-" (:id item))}
    [:tr
     [:td.text-right (util/format-date (:transaction-date item))]
     [:td (:description item)]
     [:td.text-right (currency-format (polarize-quantity item @account))]
     [:td.text-right (currency-format (:balance item))]
     [:td
      [:div.btn-group
       (util/button nil
                    #(edit-transaction item page-state)
                    {:icon :pencil
                     :class "btn btn-info btn-xs"
                     :title "Click here to edit this transaction."})
       (util/button nil
                    #(delete-transaction item page-state)
                    {:icon :remove
                     :class "btn btn-danger btn-xs"
                     :title "Click here to remove this transaction."})]]]))

(defn- items-table
  [page-state]
  (let [items (r/cursor page-state [:items])]
    (fn []
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
          (doall (map #(item-row % page-state) @items))
          [:tr [:td {:col-span 4} [:span.inline-status "Loading..."]]])]])))

(defn- find-account-fn
  [page-state]
  (fn [id]
    (->> (:accounts @page-state)
         (filter #(=  (:id %) id))
         first)))

(defn- new-transaction
  [page-state]
  (let [account-id (get-in @page-state [:view-account :id])]
    (swap! page-state assoc
           :transaction {:entity-id (get-in @app-state [:current-entity :id])
                         :transaction-date (t/today)
                         :account-id account-id}
           :transaction-entry-mode :simple
           :unprep-fn #(fullify % (find-account-fn page-state))))
  (util/set-focus "transaction-date"))

(defn- account-buttons
  [page-state]
  (let  [transaction (r/cursor page-state [:transaction])]
    (fn []
      [:div
       (util/button "New"
                    #(new-transaction page-state)
                    {:icon :plus
                     :class "btn btn-primary"
                     :disabled (not (nil? @transaction))})
       (util/space)
       (util/button "Back"
                    (fn []
                      (stop-item-loading page-state)
                      (swap! page-state #(-> %
                                             (dissoc :view-account)
                                             (assoc :items []
                                                    :more-items? true))))
                    {:icon :hand-left
                     :class "btn btn-info"
                     :title "Click here to return to the account list."})])))

(defn- refine-item
  [item]
  (-> item
      (assoc :quantity (some item [:debit-quantity :credit-quantity]))
      (assoc :action (if (:debit-quantity item)
                       :debit
                       :credit))
      (dissoc :debit-quantity :credit-quantity)))

(defn- post-transaction-save
  [page-state]
  (load-accounts page-state #(reset-item-loading page-state)))

(defmulti ^:private save-transaction
  (fn [page-state]
    (let [transaction (:transaction @page-state)]
      (cond
        (:trade-date transaction) :buy))))

(defmethod ^:private save-transaction :default
  [page-state]
  (let  [unprep-fn (get-in @page-state [:unprep-fn] identity)]
    (-> (:transaction @page-state)
        (unprep-fn (:account @page-state))
        (transactions/save #(post-transaction-save page-state)
                           notify/danger))))

(defmethod ^:private save-transaction :buy
  [page-state]
  (trading/create (:transaction @page-state)
                  #(post-transaction-save page-state)
                  (notify/danger-fn "Unable to create the trade: %s")))

(defn- simple-transaction-form
  [page-state]
  (let [transaction (r/cursor page-state [:transaction])
        accounts (r/cursor page-state [:accounts])]
    (fn []
      [:form {:class (when (or (:items @transaction)
                               (:trade-date @transaction))
                       "hidden")}
       (forms/date-field transaction :transaction-date {:validate [:required]})
       (forms/text-field transaction :description {:validate [:required]})
       (forms/float-field transaction :quantity {:validate [:required]})
       [forms/typeahead-field
         transaction
         :other-account-id
         {:search-fn (fn [input callback]
                       (let [term (string/lower-case input)]
                         (->> @accounts
                              (filter #(string/includes? (string/lower-case (:path %))
                                                         term))
                              callback)))
          :caption-fn :path
          :value-fn :id
          :find-fn (fn [id callback]
                     (->> @accounts
                          (filter #(= id (:id %)))
                          first
                          callback))}]])))

(defn- trade-transaction-form
  [page-state]
  (let [transaction (r/cursor page-state [:transaction])
        commodities (r/cursor page-state [:commodities])]
    (fn []
      [:form {:class (when-not (:trade-date @transaction) "hidden")}
       (forms/date-field transaction :trade-date {:validate [:required]})
       (forms/select-field transaction :action (map (juxt name humanize) [:buy :sell]) {})
       (forms/float-field transaction :shares {:validate [:required]})
       (forms/float-field transaction :value {:validate [:required]})
       [forms/typeahead-field
         transaction
         :commodity-id
         {:search-fn (fn [input callback]
                       (let [term (string/lower-case input)]
                         (->> @commodities
                              (filter #(or (string/includes? (string/lower-case (:name %))
                                                             term)
                                           (string/includes? (string/lower-case (:symbol %))
                                                             term)))
                              callback)))
          :caption-fn :symbol
          :value-fn :id
          :find-fn (fn [id callback]
                     (->> @commodities
                          (filter #(= id (:id %)))
                          first
                          callback))}]])))

(defn- item-input-row
  [item index page-state]
  ^{:key (str "item-form-" index)}
  [:tr
   [:td [forms/typeahead-input
         item
         :account-id
         {:search-fn (fn [input callback]
                       (let [term (string/lower-case input)]
                         (->> (:accounts @page-state)
                              (filter #(string/includes? (string/lower-case (:path %))
                                                         term))
                              callback)))
          :caption-fn :path
          :value-fn :id
          :find-fn (fn [id callback]
                     (->> (:accounts @page-state)
                          (filter #(= id (:id %)))
                          first
                          callback))}]]
   [:td [forms/text-input item :memo {}]]
   [:td [forms/float-input item :credit-quantity {}]]
   [:td [forms/float-input item :debit-quantity {}]]])

(defn- full-transaction-form
  [page-state]
  (let [transaction (r/cursor page-state [:transaction])]
    (fn []
      [:form {:class (when-not (and (:transaction-date @transaction)
                                    (:items @transaction))
                       "hidden")}
       (forms/date-field transaction :transaction-date {:validate [:required]})
       (forms/text-field transaction :description {:validate [:required]})
       [:table.table
        [:thead
         [:tr
          [:td "Account"]
          [:td "Memo"]
          [:td "Credit Amount"]
          [:td "Debit Amount"]]]
        [:tbody
         (doall (for [index (range (count (:items @transaction)))]
                  (item-input-row (r/cursor page-state [:transaction :items index])
                                  index
                                  page-state)))]]])))

(defmulti ^:private nav-to-mode
  (fn [_ mode]
    mode))

(defmethod ^:private nav-to-mode :full
  [page-state m]
  (-> page-state
      (update-in [:transaction] fullify (find-account-fn page-state))
      (assoc :transaction-entry-mode m)))

(defmethod ^:private nav-to-mode :simple
  [page-state m]
  (-> page-state
      (update-in [:transaction] simplify (:view-account page-state))
      (assoc :transaction-entry-mode m)))

(defn- tradify
  "Accepts a regular transaction and converts it into a trade transaction"
  [transaction account]
  (-> transaction
      (rename-keys {:transaction-date :trade-date})
      (assoc :account-id (:id account)
             :shares (->> (:items transaction)
                          (map :quantity)
                          first))
      (dissoc :items)))

(defn- untradify
  "Accepts a trade transaction and converts it into a regular transaction, more or less."
  [transaction]
  (-> transaction
      (rename-keys {:trade-date :transaction-date})
      (dissoc :shares :value :commodity-id)
      (assoc :items [{:account-id (:id (:account-id transaction))
                      :action :credit
                      :quantity (or (:shares transaction) 0)}
                     {:quantity (or (:shares transaction) 0)
                      :action :debit}])))

(defn- transaction-form-nav-items
  [page-state]
  (let [account (r/cursor page-state [:view-account])
        transaction (r/cursor page-state [:transaction])]
    {:simple {:disabled? (not (can-simplify? @transaction))
              :prep-fn #(simplify % @account)
              :unprep-fn #(fullify % (find-account-fn page-state))}
     :full   {:prep-fn entryfy
              :unprep-fn unentryfy}
     :trade  {:prep-fn #(tradify % @account)
              :unprep-fn tradify
              :hidden? (not (contains? (:tags @account) :trading))}}))

(defn- do-tab-nav
  [mode page-state]
  (let [current-unprep-fn (get-in @page-state [:unprep-fn] identity)
        nav-items (transaction-form-nav-items page-state)
        {:keys [prep-fn unprep-fn]} (get-in nav-items [mode])]
    (swap! page-state #(-> %
                          (assoc :transaction-entry-mode mode
                                 :unprep-fn unprep-fn)
                          (update-in [:transaction] #_(comp prep-fn unprep-fn)
                                     (fn [trx]
                                       (let  [unprepped (current-unprep-fn trx)
                                              prepped (prep-fn unprepped)]
                                         prepped)))))))

(defn- transaction-form-nav-tab
  [mode {:keys [disabled? hidden?]} page-state]
  (let [current-mode (r/cursor page-state [:transaction-entry-mode])]
    {:caption (humanize mode)
     :disabled? disabled?
     :hidden? hidden?
     :elem-key (str "entry-mode-" (name mode))
     :active? (= @current-mode mode)
     :on-click #(do-tab-nav mode page-state)}))

(defn- transaction-form
  [page-state]
  (let [transaction (r/cursor page-state [:transaction])]
    (fn []
      (when @transaction
        [:div.panel.panel-primary
         [:div.panel-heading
          [:h2.panel-title (if (:id @transaction)
                             "Edit Transaction"
                             "New Transaction")]]
         [:div.panel-body
          (nav-tabs (map #(transaction-form-nav-tab (first %) (second %) page-state)
                         (transaction-form-nav-items page-state)))
          [:div.transaction-form-container
           [full-transaction-form page-state]
           [simple-transaction-form page-state]
           [trade-transaction-form page-state]]
          (util/button "Save"
                       #(save-transaction page-state)
                       {:class "btn btn-primary"
                        :icon :ok
                        :title "Click here to save the transaction"})
          (util/space)
          (util/button "Cancel"
                       #(swap! page-state dissoc :transaction)
                       {:class "btn btn-danger"
                        :icon :remove
                        :title "Click here to cancel this transaction"})]]))))

(defn- currency-account-details
  [page-state]
  (let [account (r/cursor page-state [:view-account])
        ctl-chan (r/cursor page-state [:ctl-chan])
        more-items? (r/cursor page-state [:more-items?])]
    (init-item-loading page-state)
    (fn []
      [:section
       [:div.row
        [:div.col-md-12
         [:h2 (str (:name @account) " Details")]]]
       [:div.row
        [:div.col-md-6
         [:div.panel.panel-default
          [:div.panel-heading
           [:h2.panel-title "Transaction Items"]]
          [:div#items-container.panel-body {:style {:height "40em" :overflow "auto"}}
           [items-table page-state]]
          [:div.panel-footer
           [load-on-scroll {:target "items-container"
                            :can-load-more? (fn [] @more-items?)
                            :load-fn #(go (>! @ctl-chan :fetch))}]]]
         [account-buttons page-state]]
        [:div.col-md-6
         [transaction-form page-state]]]])))

(defn- lots-table
  [page-state]
  (let [lots (r/cursor page-state [:lots])
        prices (r/cursor page-state [:prices])
        latest-price (make-reaction #(->> @prices
                                           (sort-by (comp serialize-date :trade-date))
                                           last))
        total-shares (make-reaction #(->> @lots
                                           (map :shares-owned)
                                           (reduce +)))
        total-value (make-reaction #(* (or @total-shares 0)
                                       (or (:price @latest-price) 0)))
        total-cost (make-reaction #(->> @lots
                                        (map (fn [{:keys [purchase-price shares-owned]}]
                                               (* purchase-price shares-owned)))
                                        (reduce +)))
        gain-loss (make-reaction #(- @total-value @total-cost))]
    (fn []
      [:table.table.table-striped
       [:thead
        [:tr
         [:th.text-right "Purchase Date"]
         [:th.text-right "Shares Purchased"]
         [:th.text-right "Shares Owned"]
         [:th.text-right "Purchase Price"]
         [:th.text-right "Gn/Ls"]
         [:th.text-right "Gn/Ls %"]]]
       [:tbody
        (doall (for [lot (sort-by (comp serialize-date :purchase-date) @lots)]
                 (let [g-l (- (* (:price @latest-price)
                                 (:shares-owned lot))
                              (* (:purchase-price lot)
                                 (:shares-owned lot)))]
                   ^{:key (str "lot-" (:id lot))}
                   [:tr
                    [:td.text-right (util/format-date (:purchase-date lot))]
                    [:td.text-right (util/format-decimal (:shares-purchased lot) 4)]
                    [:td.text-right (util/format-decimal (:shares-owned lot) 4)]
                    [:td.text-right (util/format-decimal (:purchase-price lot) 2)]
                    [:td.text-right {:class (if (> g-l) "success" "danger")}
                     (util/format-decimal g-l)]
                    [:td.text-right {:class (if (> @gain-loss) "success" "danger")}
                     (util/format-percent (/ g-l
                                             (* (:shares-purchased lot)
                                                (:purchase-price lot)))
                                          3)]])))]
       [:tfoot
        [:tr
         [:td.text-right {:col-span 2}
          (when @latest-price
            (gstr/format "(%s as of %s)"
                         (currency-format (:price @latest-price))
                         (util/format-date (:trade-date @latest-price))))]
         [:td.text-right (util/format-decimal @total-shares 4)]
         [:td.text-right (currency-format @total-value)]
         [:td.text-right {:class (if (> @gain-loss) "text-success" "text-danger")}
          (currency-format @gain-loss)]
         [:td.text-right {:class (if (> @gain-loss) "text-success" "text-danger")}
          (util/format-percent (/ @gain-loss
                                  @total-cost)
                               3)]]]])))

(defn- fund-transactions-table
  [page-state]
  (let [items (r/cursor page-state [:items])
        account  (r/cursor page-state [:view-account])]
    (fn []
      [:table.table.table-striped
       [:thead
        [:tr
         [:th.text-right "Transaction Date"]
         [:th "Description"]
         [:th.text-right "Qty."]
         [:th.text-right "Bal."]
         [:th.text-right "Value"]]]
       [:tbody
        (doall (for [item (sort-by (comp serialize-date :transaction-date) @items)]
                 ^{:key (str "item-" (:id item))}
                 [:tr
                  [:td.text-right (util/format-date (:transaction-date item))]
                  [:td (:description item)]
                  [:td.text-right (util/format-decimal (polarize-quantity item @account) 4)]
                  [:td.text-right (util/format-decimal (:balance item), 4)]
                  [:td.text-right (currency-format (:value item))]]))]])))

(defn- tradable-account-details
  [page-state]
  (let [account (r/cursor page-state [:view-account])
        {:keys [parent-id
                commodity-id
                id
                earliest-transaction-date
                latest-transaction-date]} @account]
    (lots/search {:account-id parent-id
                  :commodity-id commodity-id
                  :shares-owned [:!= 0]}
                 #(swap! page-state assoc :lots %)
                 (notify/danger-fn "Unable to load the lots: %s"))
    ; I don't think we need to chunk this, but maybe we do
    (transaction-items/search {:account-id id
                               :transaction-date [:between
                                                  earliest-transaction-date
                                                  latest-transaction-date]}
                              #(swap! page-state assoc :items %)
                              (notify/danger-fn "Unable to load the transaction items: %s"))
    (prices/search {:commodity-id commodity-id
                    :trade-date [:between
                                 earliest-transaction-date
                                 latest-transaction-date]}
                   #(swap! page-state assoc :prices %)
                   (notify/danger-fn "Unable to load the prices: %s"))
    (fn []
      [:section
       [:div.row
        [:div.col-md-12
         [:h2 (:name @account)]]]
       [:div.row
        [:div.col-md-7
         [:h3 "Lots"]
         [lots-table page-state]]]
       [:div.row
        [:div.col-md-8
         [:h3 "Transactions"]
         [fund-transactions-table page-state]]]
       [:div.row
        [:div.col-md-6
         (util/button "Buy/Sell"
                    #(js/alert "Not implemented")
                    {:icon :plus
                     :class "btn btn-primary"})
       (util/space)
       (util/button "Back"
                    #(swap! page-state dissoc :view-account)
                    {:icon :hand-left
                     :class "btn btn-info"
                     :title "Click here to return to the account list."})]]])))

(defn- account-details
  [page-state]
  (let [tags (r/cursor page-state [:view-account :tags])]
    (fn []
      (cond
        (:tradable @tags) [tradable-account-details page-state]
        :else [currency-account-details page-state]))))

(defn- load-commodities
  [page-state]
  (commodities/get-all (get-in @app-state [:current-entity :id])
                       #(swap! page-state assoc :commodities %)
                       notify/danger))

(defn- accounts-page []
  (let [page-state (r/atom {:expanded #{}
                            :ctl-chan (chan)
                            :transaction-entry-mode :simple
                            :more-items? true})
        selected (r/cursor page-state [:selected])
        view-account (r/cursor page-state [:view-account])]
    (load-accounts page-state)
    (load-commodities page-state)
    (add-watch app-state :current-entity (fn [field _sender _before after]
                                           (swap! page-state assoc :accounts [] :commodities [])
                                           (when (get-in after [field])
                                             (load-accounts page-state)
                                             (load-commodities page-state))))
    (fn []
      [:section
       [:div.accounts-header
        [:h1.accounts-title "Accounts"]]
       (when-not (or @selected @view-account)
         [account-list page-state])
       (when @selected
         [account-form page-state])
       (when @view-account
         [account-details page-state])])))

(secretary/defroute "/accounts" []
  (swap! app-state assoc :page #'accounts-page))
