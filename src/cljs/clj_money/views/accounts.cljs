(ns clj-money.views.accounts
  (:require [clojure.string :as string]
            [goog.string :as gstr]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [reagent.format :refer [currency-format]]
            [secretary.core :as secretary :include-macros true]
            [cljs.core.async :refer [chan >! go]]
            [cljs-time.core :as t]
            [clj-money.decimal :as decimal]
            [clj-money.inflection :refer [humanize]]
            [clj-money.forms :as forms]
            [clj-money.components :refer [load-on-scroll]]
            [clj-money.bootstrap :as bs :refer [nav-tabs]]
            [clj-money.api.commodities :as commodities]
            [clj-money.api.accounts :as accounts]
            [clj-money.api.lots :as lots]
            [clj-money.api.prices :as prices]
            [clj-money.transactions :refer [can-simplify?]]
            [clj-money.accounts :refer [account-types
                                        nest
                                        unnest]]
            [clj-money.state :refer [app-state]]
            [clj-money.notifications :as notify]
            [clj-money.util :refer [serialize-date
                                    format-percent
                                    format-date
                                    format-decimal]]
            [clj-money.views.transactions :as trns]
            [clj-money.views.reconciliations :as recs]
            [clj-money.views.attachments :as atts]
            [clj-money.html :as html]))

(defn- load-accounts
  ([page-state] (load-accounts page-state identity))
  ([page-state callback]
   (accounts/select (fn [result]
                      (swap! page-state
                             (fn [s]
                               (let [accounts (->> result
                                                   (nest {:plus decimal/+})
                                                   unnest)]
                                 (cond-> (assoc s
                                                :accounts accounts
                                                :mapped-accounts (->> accounts
                                                                      (map (juxt :id identity))
                                                                      (into {})))
                                   (empty? (:accounts s))
                                   (assoc :hide-zero-balances? (->> result
                                                                    (map :value)
                                                                    (not-every? #(= 0 %))))))))
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



(defn- recently-created?
  [{:keys [created-at]}]
  (t/before?
    (t/minus (t/now) (-> 1 t/hours))
    created-at))

(defn- account-hidden?
  [{:keys [parents] :as account} expanded hide-zero-balances?]
  (or (and @hide-zero-balances?
           (decimal/zero? (decimal/+ (:value account)
                                     (:children-value account)))
           (not (recently-created? account)))
      (not (@expanded (:type account)))
      (and parents
           (not-every? @expanded parents))))

(defn- account-row
  [{:keys [id parents] :as account} expanded hide-zero-balances? page-state]
  ^{:key (str "account-" id)}
  [:tr {:class (when (account-hidden? account expanded hide-zero-balances?)
                 "d-none")}
   [:td [:span {:class (str "account-depth-" (count parents))}
         [:span.toggle-ctl {:on-click #(toggle-account (:id account) page-state)
                            :class (when-not (:has-children? account)
                                     "invisible")}
          (bs/icon (if (@expanded id)
                     :arrows-collapse
                     :arrows-expand))]
         (:name account)]]
   [:td.text-right (currency-format (decimal/+ (:children-value account)
                                               (:value account)))]
   [:td.text-center
    [:div.btn-group
     [:button.btn.btn-light.btn-sm {:on-click #(swap! page-state assoc :view-account account)
                                    :title "Click here to view transactions for this account."}
      (bs/icon :collection)]
     [:button.btn.btn-info.btn-sm {:on-click (fn []
                                               (swap! page-state assoc :selected account)
                                               (html/set-focus "parent-id"))
                                   :title "Click here to edit this account."}
      (bs/icon :pencil)]
     [:button.btn.btn-danger.btn-sm {:on-click #(delete account page-state)
                                     :title "Click here to remove this account."}
      (bs/icon :x-circle)]]]])

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
                                     [:span.toggle-ctl {:aria-hidden true
                                                        :on-click #(toggle-account account-type page-state)
                                                        }
                                      (bs/icon (if (@expanded account-type)
                                                 :arrows-collapse 
                                                 :arrows-expand))]
                                     (name account-type)]
                                    [:td.text-right (currency-format (->> group
                                                                          (map :value)
                                                                          (reduce decimal/+)))]
                                    [:td (html/space)]]]
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
         [:hide-zero-balances?]
         {:caption "Hide Zero-Balance Accounts"}]]
       [:table.table.table-hover
        [:thead
         [:tr
          [:th.col-md-7 "Name"]
          [:th.col-md-3.text-right "Value"]
          [:th.col-md-2 (html/space)]]]
        (if (seq @accounts)
          [account-and-type-rows page-state]
          [:tbody
           [:tr
            [:td {:col-span 3} [:span.inline-status "Loading..."]]]])]
       [:button.btn.btn-primary {:on-click (fn []
                                             (swap! page-state assoc :selected {:entity-id (:id @current-entity)
                                                                                :type :asset})
                                             (html/set-focus "parent-id"))}
        (bs/icon-with-text :plus "Add")]])))

(defn- account-fields
  [page-state]
  (let [account (r/cursor page-state [:selected])
        accounts (r/cursor page-state [:accounts])
        commodities (r/cursor page-state [:commodities])]
    (fn []
      [:form
       (forms/typeahead-field
         account
         [:parent-id]
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
                           [:type]
                           (map (juxt name humanize) account-types)
                           {:class (when (:parent-id @account) "hidden")})
       (forms/text-field account [:name] {:validate [:required]})
       (forms/typeahead-field
         account
         [:commodity-id]
         {:search-fn (fn [input callback]
                       (let [term (string/lower-case input)]
                         (->> @commodities
                              vals
                              (filter #(or (string/includes? (string/lower-case (:name %))
                                                             term)
                                           (string/includes? (string/lower-case (:symbol %))
                                                             term)))
                              callback)))
          :caption-fn :name
          :value-fn :id
          :find-fn (fn [id callback]
                     (callback (get-in @commodities [id])))})
       (forms/checkbox-field
         account
         [:trading]
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
        [:button.btn.btn-primary {:on-click #(save-account page-state)
                                  :title "Click here to save the account."}
         (bs/icon-with-text :check "Save")]
        
        (html/space)
        [:button.btn.btn-danger {:on-click #(swap! page-state dissoc :selected)
                                 :title "Click here to return to the list of accounts."}
         (bs/icon-with-text :x "Cancel")]]])))

(defn- new-transaction
  [page-state]
  (let [account-id (get-in @page-state [:view-account :id])]
    (swap! page-state assoc
           :transaction (trns/mode
                          {:entity-id (get-in @app-state [:current-entity :id])
                           :transaction-date (t/today)
                           :account-id account-id}
                          ::trns/simple)))
  (html/set-focus "transaction-date"))

(defn- account-buttons
  [page-state]
  (let  [transaction (r/cursor page-state [:transaction])]
    (fn []
      [:div
       [:button.btn.btn-primary {:on-click #(new-transaction page-state)
                                 :disabled (not (nil? @transaction))}
        (bs/icon-with-text :plus "Add")]
       (html/space)
       [:button.btn.btn-info {:on-click (fn []
                                          (trns/stop-item-loading page-state)
                                          (swap! page-state assoc
                                                 :items nil)
                                          (recs/load-working-reconciliation page-state)
                                          (trns/load-unreconciled-items page-state)
                                          (html/set-focus "end-of-period"))
                              :title "Click here to reconcile this account"}
        (bs/icon-with-text :check-box "Reconcile")]
       (html/space)
       [:button.btn.btn-info {:on-click (fn []
                                          (trns/stop-item-loading page-state)
                                          (swap! page-state dissoc
                                                 :view-account
                                                 :items
                                                 :all-items-fetched?))
                              :title "Click here to return to the account list."}
        (bs/icon-with-text :arrow-left-short "Back")]])))

(defn- post-transaction-save
  [page-state]
  (fn []
    (load-accounts page-state #(trns/reset-item-loading page-state))))

(defn- do-tab-nav
  [mode page-state]
  (let [{:keys [mapped-accounts view-account commodities transaction]} @page-state
        unprep-fn (get-in (trns/untransformations mapped-accounts)
                          [(trns/mode transaction)])
        prep-fn (get-in (trns/transformations view-account
                                              mapped-accounts
                                              commodities)
                        [mode])]
    (swap! page-state
           (fn [state]
             (update-in state
                        [:transaction]
                        (comp #(trns/mode % mode)
                              prep-fn
                              unprep-fn))))))

(defn- transaction-form-nav-tab
  [{:keys [mode disabled?]} page-state]
  (let [current-mode (trns/mode (:transaction @page-state))]
    {:caption (humanize (name mode))
     :disabled? disabled?
     :elem-key (str "entry-mode-" (name mode))
     :active? (= current-mode mode)
     :on-click #(do-tab-nav mode page-state)}))

(defn- neutralize
  [transaction mode accounts]
  (let [f (get-in (trns/untransformations accounts)
                     [mode])]
    (f transaction)))

(defn- transaction-form
  [page-state]
  (let [transaction (r/cursor page-state [:transaction])
        mode (make-reaction #(trns/mode @transaction))
        accounts (r/cursor page-state [:mapped-accounts])
        neutralized (make-reaction #(neutralize @transaction @mode @accounts))
        tags (r/cursor page-state [:view-account :tags])
        disable-trade? (make-reaction #(or (:id @transaction)
                                           (not (:trading @tags))))]
    (fn []
      [:div.card
       [:div.card-header
        [:strong (if (:id @transaction)
                   "Edit Transaction"
                   "New Transaction")]]
       [:div.card-body
        (nav-tabs (map #(transaction-form-nav-tab % page-state)
                       [{:mode ::trns/simple
                         :disabled? (not (can-simplify? @neutralized))}
                        {:mode ::trns/full
                         :disabled? false}
                        {:mode ::trns/trade
                         :disabled? @disable-trade?}
                        {:mode ::trns/dividend
                         :disabled? @disable-trade?}]))
        [:div.mt-3
         [trns/full-transaction-form page-state]
         [:div.row
          [:div.col-md-4
           [trns/simple-transaction-form page-state]
           [trns/trade-transaction-form page-state]
           [trns/dividend-transaction-form page-state]]]]]
       [:div.card-footer
        [:button.btn.btn-primary {:on-click #(trns/save-transaction page-state (post-transaction-save page-state))
                                  :title "Click here to save the transaction"}
         (bs/icon-with-text :check "Save")]

        (html/space)
        [:button.btn.btn-danger {:on-click #(swap! page-state dissoc :transaction)
                                 :title "Click here to cancel this transaction"}
         (bs/icon-with-text :x "Cancel")]]])))

(defn- transaction-item-list
  [page-state]
  (let [ctl-chan (r/cursor page-state [:ctl-chan])
        all-items-fetched? (r/cursor page-state [:all-items-fetched?])]
    (trns/init-item-loading page-state)
    (fn []
      [:div.card
       [:div.card-header [:strong "Transaction Items"]]
       [:div#items-container {:style {:max-height "40em" :overflow "auto"}}
        [trns/items-table page-state]]
       [:div.card-footer.d-flex.align-items-center
        [account-buttons page-state]
        [:span.ml-auto
         [load-on-scroll {:target "items-container"
                          :all-items-fetched? all-items-fetched?
                          :load-fn #(go (>! @ctl-chan :fetch))}]]]])))

(defn- currency-account-details
  [page-state]
  (let [transaction (r/cursor page-state [:transaction])
        reconciliation (r/cursor page-state [:reconciliation])
        attachments-item (r/cursor page-state [:attachments-item])
        selected-attachment (r/cursor page-state [:selected-attachment])]
    (fn []
      (cond
        @transaction         [transaction-form page-state]
        @reconciliation      [:div.row
                              [:div.col
                               [transaction-item-list page-state]]
                              [:div.col
                               [recs/reconciliation-form page-state]]]
        @selected-attachment [atts/attachment-form page-state]
        @attachments-item    [atts/attachments-card page-state]
        :else                [transaction-item-list page-state]))))

(defn- lots-table
  [page-state]
  (let [lots (r/cursor page-state [:lots])
        prices (r/cursor page-state [:prices])
        latest-price (make-reaction #(->> @prices
                                           (sort-by (comp serialize-date :trade-date))
                                           last))
        total-shares (make-reaction #(->> @lots
                                           (map :shares-owned)
                                           (reduce decimal/+)))
        total-value (make-reaction #(decimal/* (or @total-shares
                                                   (decimal/zero))
                                               (or (:price @latest-price)
                                                   (decimal/zero))))
        total-cost (make-reaction #(->> @lots
                                        (map (fn [{:keys [purchase-price shares-owned]}]
                                               (* purchase-price shares-owned)))
                                        (reduce +)))
        gain-loss (make-reaction #(- @total-value @total-cost))]
    (fn []
      [:table.table.table-hover.table-borderless
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
                    [:td.text-right (format-date (:purchase-date lot))]
                    [:td.text-right (format-decimal (:shares-purchased lot) 4)]
                    [:td.text-right (format-decimal (:shares-owned lot) 4)]
                    [:td.text-right (format-decimal (:purchase-price lot) 2)]
                    [:td.text-right
                     {:class (if (>= g-l 0M)
                               "text-success"
                               "text-danger")}
                     (format-decimal g-l)]
                    [:td.text-right
                     {:class (if (>= @gain-loss 0M)
                               "text-success"
                               "text-danger")}
                     (format-percent (/ g-l
                                             (* (:shares-purchased lot)
                                                (:purchase-price lot)))
                                          3)]])))]
       [:tfoot
        [:tr
         [:td.text-right {:col-span 2}
          (when @latest-price
            (gstr/format "(%s as of %s)"
                         (currency-format (:price @latest-price))
                         (format-date (:trade-date @latest-price))))]
         [:td.text-right (format-decimal @total-shares 4)]
         [:td.text-right (currency-format @total-value)]
         [:td.text-right {:class (if (>= @gain-loss 0M) "text-success" "text-danger")}
          (currency-format @gain-loss)]
         [:td.text-right {:class (if (>= @gain-loss 0M) "text-success" "text-danger")}
          (format-percent (/ @gain-loss
                                  @total-cost)
                               3)]]]])))

(defn- tradable-account-items
  [page-state]
  (let [current-nav (r/atom :lots)
        account (r/cursor page-state [:view-account])
        {:keys [parent-id
                entity-id
                commodity-id
                earliest-transaction-date
                latest-transaction-date]} @account]
    (lots/search {:account-id parent-id
                  :commodity-id commodity-id
                  :shares-owned [:!= 0]}
                 #(swap! page-state assoc :lots %)
                 (notify/danger-fn "Unable to load the lots: %s"))
    (prices/search {:commodity-id commodity-id
                    :trade-date [:between
                                 earliest-transaction-date
                                 latest-transaction-date]}
                   #(swap! page-state assoc :prices %)
                   (notify/danger-fn "Unable to load the prices: %s"))
    (fn []
      [:section
       (bs/nav-tabs [{:caption "Lots"
                      :elem-key :lots
                      :active? (= :lots @current-nav)
                      :on-click #(reset! current-nav :lots)}
                     {:caption "Transactions"
                      :elem-key :transactions
                      :active? (= :transactions @current-nav)
                      :on-click #(reset! current-nav :transactions)}])
       (case @current-nav
         :lots         [lots-table page-state]
         :transactions [trns/fund-transactions-table page-state])
       [:div.row
        [:div.col-md-6
         [:button.btn.btn-primary {:title "Click here to buy or sell this commodity."
                                   :on-click (fn []
                                               (swap! page-state
                                                      assoc
                                                      :transaction
                                                      {:trade-date (t/today)
                                                       :entity-id entity-id
                                                       :account-id parent-id
                                                       :commodity-id commodity-id
                                                       :commodity-account-id (:id @account)})
                                               (html/set-focus "trade-date"))}
          (bs/icon-with-text :plus "Buy/Sell")]
         (html/space)
         [:button.btn.btn-info {:title "Click here to return the the account list."
                                :on-click #(swap! page-state dissoc :view-account)}
          (bs/icon-with-text :arrow-left-short "Back")]]]])))

(defn- tradable-account-details
  [page-state]
  (let [transaction (r/cursor page-state [:transaction])]
    (fn []
      (if @transaction
        [:div.mt-3
         [:div.row
          [:div.col-md-4
           [:div.card
            [:div.card-header
             [:strong (if (:id @transaction)
                        "Edit Transaction"
                        "New Transaction")]]
            [:div.card-body
             [trns/trade-transaction-form page-state]]
            [:div.card-footer
             [:button.btn.btn-primary {:on-click #(trns/save-transaction page-state (post-transaction-save page-state))
                                       :title "Click here to save the transaction"}
              (bs/icon-with-text :check "Save")]
             (html/space)
             [:button.btn.btn-danger {:on-click #(swap! page-state dissoc :transaction)
                                      :title "Click here to cancel this transaction"}
              (bs/icon-with-text :x "Cancel")]]]]]]
        [tradable-account-items page-state]))))

(defn- account-details
  [page-state]
  (let [tags (r/cursor page-state [:view-account :tags])]
    (fn []
      (cond
        (:tradable @tags) [tradable-account-details page-state]
        :else [currency-account-details page-state]))))

(defn- load-commodities
  [page-state]
  (commodities/select #(swap! page-state assoc :commodities (->> %
                                                                 (map (juxt :id identity))
                                                                 (into {})))
                      notify/danger))

(defn- index []
  (let [page-state (r/atom {:expanded #{}
                            :ctl-chan (chan)})
        selected (r/cursor page-state [:selected])
        view-account (r/cursor page-state [:view-account])]
    (load-accounts page-state)
    (load-commodities page-state)
    
    (fn []
      [:div.mt-5
       [:div.accounts-header
        [:h1.accounts-title (str "Accounts" (when @view-account
                                              (str " - " (:name @view-account) )))]]
       (when-not (or @selected @view-account)
         [account-list page-state])
       (when @selected
         [account-form page-state])
       (when @view-account
         [account-details page-state])])))

(secretary/defroute "/accounts" []
  (swap! app-state assoc :page #'index))
