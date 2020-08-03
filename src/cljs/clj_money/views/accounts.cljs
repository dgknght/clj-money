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
            [clj-money.decimal :as decimal]
            [clj-money.inflection :refer [humanize]]
            [clj-money.forms :as forms]
            [clj-money.components :refer [load-on-scroll]]
            [clj-money.bootstrap :as bs :refer [nav-tabs]]
            [clj-money.api.commodities :as commodities]
            [clj-money.api.accounts :as accounts]
            [clj-money.api.transactions :as transactions]
            [clj-money.api.lots :as lots]
            [clj-money.api.prices :as prices]
            [clj-money.api.trading :as trading]
            [clj-money.transactions :refer [simplify
                                            fullify
                                            can-simplify?
                                            entryfy
                                            unentryfy]]
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
   (accounts/get-all (get-in @app-state [:current-entity :id])
                     (fn [result]
                       (swap! page-state
                              (fn [s]
                                (cond-> (assoc s :accounts (->> result
                                                                (nest {:plus decimal/+})
                                                                unnest))
                                  (empty? (:accounts s))
                                  (assoc :hide-zero-balances? (->> result
                                                                   (map :value)
                                                                   (not-every? #(= 0 %)))))))
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



(defn- account-hidden?
  [{:keys [parents] :as account} expanded hide-zero-balances?]
  (or (and @hide-zero-balances?
           (decimal/zero? (decimal/+ (:value account)
                                     (:children-value account))))
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
  (load-accounts page-state #(trns/reset-item-loading page-state)))

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
                       "d-none")}
       (forms/date-field transaction [:transaction-date] {:validate [:required]})
       (forms/text-field transaction [:description] {:validate [:required]})
       (forms/decimal-field transaction [:quantity] {:validate [:required]})
       [forms/typeahead-field
         transaction
         [:other-account-id]
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
      [:form {:class (when-not (:trade-date @transaction) "d-none")}
       (forms/date-field transaction [:trade-date] {:validate [:required]})
       (forms/select-field transaction [:action] (map (juxt name humanize) [:buy :sell]) {})
       (forms/decimal-field transaction [:shares] {:validate [:required]})
       (forms/decimal-field transaction [:value] {:validate [:required]})
       [forms/typeahead-field
         transaction
         [:commodity-id]
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
         [:account-id]
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
   [:td [forms/text-input item [:memo] {}]]
   [:td [forms/decimal-input item [:credit-quantity] {}]]
   [:td [forms/decimal-input item [:debit-quantity] {}]]])

(defn- full-transaction-form
  [page-state]
  (let [transaction (r/cursor page-state [:transaction])]
    (fn []
      [:form {:class (when-not (and (:transaction-date @transaction)
                                    (:items @transaction))
                       "d-none")}
       (forms/date-field transaction [:transaction-date] {:validate [:required]})
       (forms/text-field transaction [:description] {:validate [:required]})
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
                          (update-in [:transaction] (comp prep-fn current-unprep-fn))))))

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
      [:div.card
       [:div.card-header
        [:strong (if (:id @transaction)
                   "Edit Transaction"
                   "New Transaction")]]
       [:div.card-body
        (nav-tabs (map #(transaction-form-nav-tab (first %) (second %) page-state)
                       (transaction-form-nav-items page-state)))
        [:div.mt-3
         [full-transaction-form page-state]
         [simple-transaction-form page-state]
         [trade-transaction-form page-state]]]
       [:div.card-footer
        [:button.btn.btn-primary {:on-click #(save-transaction page-state)
                                  :title "Click here to save the transaction"}
         (bs/icon-with-text :check "Save")]

        (html/space)
        [:button.btn.btn-danger {:on-click #(swap! page-state dissoc :transaction)
                                 :title "Click here to cancel this transaction"}
         (bs/icon-with-text :x "Cancel")]]])))

(defn- currency-account-details
  [page-state]
  (let [ctl-chan (r/cursor page-state [:ctl-chan])
        all-items-fetched? (r/cursor page-state [:all-items-fetched?])
        transaction (r/cursor page-state [:transaction])
        reconciliation (r/cursor page-state [:reconciliation])
        attachments-item (r/cursor page-state [:attachments-item])
        selected-attachment (r/cursor page-state [:selected-attachment])]
    (trns/init-item-loading page-state)
    (fn []
      [:div.row
       [:div.col {:style {:max-width "50em"
                          :flex-grow 3}}
        [:div.card
         [:div.card-header [:strong "Transaction Items"]]
         [:div#items-container {:style {:max-height "40em" :overflow "auto"}}
          [trns/items-table page-state]]
         [:div.card-footer.d-flex.align-items-center
          [account-buttons page-state]
          [:span.ml-auto
           [load-on-scroll {:target "items-container"
                            :all-items-fetched? all-items-fetched?
                            :load-fn #(go (>! @ctl-chan :fetch))}]]]]]
       (when (or @transaction @reconciliation @attachments-item)
         [:div.col {:style {:flex-grow 2}}
          (when @selected-attachment
            [atts/attachment-form page-state])
          (when @attachments-item
            [atts/attachments-card page-state])
          (when @transaction
            [transaction-form page-state])
          (when @reconciliation
            [recs/reconciliation-form page-state])])])))

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

(defn- tradable-account-details
  [page-state]
  (let [current-nav (r/atom :lots)
        account (r/cursor page-state [:view-account])
        {:keys [parent-id
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
                                   :on-click #(js/alert "Not implemented")}
          (bs/icon-with-text :plus "Buy/Sell")]
         (html/space)
         [:button.btn.btn-info {:title "Click here to return the the account list."
                                :on-click #(swap! page-state dissoc :view-account)}
          (bs/icon-with-text :arrow-left-short "Back")]]]])))

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

(defn- index []
  (let [page-state (r/atom {:expanded #{}
                            :ctl-chan (chan)
                            :transaction-entry-mode :simple})
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
