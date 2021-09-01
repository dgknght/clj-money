(ns clj-money.views.accounts
  (:require [clojure.string :as string]
            [clojure.set :refer [union
                                 intersection]]
            [goog.string :as gstr]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [reagent.format :refer [currency-format]]
            [secretary.core :as secretary :include-macros true]
            [cljs.core.async :refer [chan >! go]]
            [cljs-time.core :as t]
            [dgknght.app-lib.web :refer [serialize-date
                                         format-percent
                                         format-date
                                         format-decimal]]
            [dgknght.app-lib.inflection :refer [humanize]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.decimal :as decimal]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.notifications :as notify]
            [dgknght.app-lib.busy :refer [busy +busy -busy]]
            [dgknght.app-lib.bootstrap-5 :as bs :refer [nav-tabs]]
            [clj-money.views.util :refer [handle-error]]
            [clj-money.components :refer [load-on-scroll]]
            [clj-money.api.commodities :as commodities]
            [clj-money.api.accounts :as accounts]
            [clj-money.api.lots :as lots]
            [clj-money.api.prices :as prices]
            [clj-money.transactions :refer [can-simplify?]]
            [clj-money.cached-accounts :refer [fetch-accounts]]
            [clj-money.accounts :refer [account-types
                                        find-by-path]]
            [clj-money.state :refer [app-state
                                     current-entity
                                     accounts
                                     accounts-by-id]]
            [clj-money.views.transactions :as trns]
            [clj-money.views.reconciliations :as recs]
            [clj-money.views.attachments :as atts]))

(defn- delete
  [account page-state]
  (when (js/confirm (str "Are you sure you want to delete the account " (:name account) "?"))
    (+busy page-state)
    (accounts/delete account
                     (fn []
                       (fetch-accounts)
                       (-busy page-state))
                     (handle-error page-state "Unable to delete the account: %s"))))

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
  [{:keys [parent-ids] :as account} expanded hide-zero-balances?]
  (or (and hide-zero-balances?
           (decimal/zero? (decimal/+ (:value account)
                                     (:children-value account)))
           (not (recently-created? account)))
      (not (expanded (:type account)))
      (and (seq parent-ids)
           (not-every? expanded parent-ids))))

(defn- account-row
  [{:keys [id parent-ids] :as account} expanded page-state]
  ^{:key (str "account-" id)}
  [:tr
   [:td [:span {:class (str "account-depth-" (count parent-ids))}
         [:span.toggle-ctl {:on-click #(toggle-account (:id account) page-state)
                            :class (when-not (:has-children? account)
                                     "invisible")}
          (bs/icon (if (expanded id)
                     :arrows-collapse
                     :arrows-expand)
                   {:size :small})]
         (:name account)]]
   [:td.text-end (currency-format (:total-value account))]
   [:td.text-center
    [forms/checkbox-input page-state [:bulk-edit :account-ids] {:no-bootstrap? true
                                                                :html {:name "bulk-edit-id"}
                                                                :value id}]]
   [:td.text-center
    [:div.btn-group
     [:button.btn.btn-light.btn-sm {:on-click #(swap! page-state assoc :view-account account)
                                    :title "Click here to view transactions for this account."}
      (bs/icon :collection {:size :small})]
     [:button.btn.btn-light.btn-sm {:on-click (fn []
                                               (swap! page-state assoc :selected account)
                                               (html/set-focus "parent-id"))
                                   :title "Click here to edit this account."}
      (bs/icon :pencil {:size :small})]
     [:button.btn.btn-danger.btn-sm {:on-click #(delete account page-state)
                                     :title "Click here to remove this account."}
      (bs/icon :x-circle {:size :small})]]]])

(defn- account-type-row
  [account-type group expanded page-state]
  ^{:key (str "account-type" account-type)}
  [:tr.account-type {:id (str "account-type-" account-type)}
   [:td
    [:span.toggle-ctl {:aria-hidden true
                       :on-click #(toggle-account account-type page-state)}
     (bs/icon (if (expanded account-type)
                :arrows-collapse
                :arrows-expand)
              {:size :small})]
    (name account-type)]
   [:td.text-end (currency-format (->> group
                                         (map :value)
                                         (reduce decimal/+)))]
   [:td (html/space)]])

(defn- compare-vec
  [v1 v2]
  (let [r (compare (first v1) (first v2))]
    (if (zero? r)
      (compare-vec (rest v1) (rest v2))
      r)))

(defn- account-and-type-rows
  [page-state]
  (let [filter-tags (r/cursor page-state [:filter-tags])
        filter-fn (make-reaction #(cond
                                    (@filter-tags :_untagged) (fn [{:keys [user-tags]}]
                                                                (empty? user-tags))
                                    (seq @filter-tags) (fn [{:keys [user-tags]}]
                                                         (seq (intersection @filter-tags user-tags)))
                                    :else identity))
        expanded (r/cursor page-state [:expanded])
        hide-zero-balances? (r/cursor page-state [:hide-zero-balances?])]
    (fn []

      (let [grouped (group-by :type @accounts)]
        [:tbody
         (doall (mapcat (fn [[account-type group]]
                          (concat [(account-type-row account-type group @expanded page-state)]
                                  (->> group
                                       (sort-by :path compare-vec)
                                       (filter (every-pred @filter-fn
                                                           #(not (account-hidden? % @expanded @hide-zero-balances?))))
                                       (map #(account-row %
                                                          @expanded
                                                          page-state)))))
                        grouped))]))))

(defn- bulk-save
  [page-state]
  (+busy page-state)
  (let [{{:keys [account-ids
                 merge-user-tags?
                 user-tags]} :bulk-edit} @page-state
        account-ids (if (set? account-ids)
                      account-ids
                      #{account-ids})
        results (atom {:succeeded 0
                       :errors []
                       :completed 0})
        receive-fn (fn [update-fn]
                     (swap! results (fn [state]
                                      (-> state
                                          update-fn
                                          (update-in [:completed] inc))))
                     (when (>= (:completed @results)
                               (count account-ids))
                       (swap! page-state #(-> %
                                              -busy
                                              (dissoc :bulk-edit)))
                       (fetch-accounts)
                       (notify/toast "Updated Finished"
                                     (str "Updated "
                                          (:succeeded @results)
                                          " account(s)."))))
        apply-fn (if merge-user-tags?
                   #(update-in % [:user-tags] union user-tags)
                   #(assoc % :user-tags user-tags))
        to-update (->> account-ids
                       (map @accounts-by-id)
                       (map apply-fn))]
    (doseq [account to-update]
      (accounts/update account
                       #(receive-fn (fn [state] (update-in state [:succeeded] inc)))
                       #(receive-fn (fn [state] (update-in state [:errors] conj %)))))))

(defn- tag-elem
  [tag {:keys [remove-fn]}]
  (let [tag-name (name tag)]
    ^{:key (str "tag-" tag-name)}
    [:div.tag.account-tag.d-flex
     [:span.me-2 tag-name]
     [:a.ms-auto {:href "#"
                  :title "Click here to remove this tag"
                  :on-click (fn [] (remove-fn tag))}
      (bs/icon :x {:size :small})]]))

(defn- tag-elems
  [tags opts]
  [:div.d-flex.mb-3.mt-3
   (if (seq tags)
     (->> tags
          (map #(tag-elem % opts))
          doall)
     [:span.text-muted "None"])])

(defn- tag-search-fn
  [bulk-edit all-user-tags]
  (fn [term callback]
    (let [existing (or (:user-tags @bulk-edit)
                       #{})]
      (->> @all-user-tags
           (remove existing)
           (map name)
           (filter #(string/includes? % term))
           callback))))

(defn- apply-tag
  [bulk-edit tag]
  (swap! bulk-edit #(-> %
                        (update-in [:user-tags] (fnil conj #{}) (keyword tag))
                        (dissoc :working-tag))))

(defn- bulk-edit-form
  [page-state]
  (let [bulk-edit (r/cursor page-state [:bulk-edit])
        busy? (r/cursor page-state [:busy?])
        user-tags (r/cursor bulk-edit [:user-tags])
        all-user-tags (make-reaction #(->> @accounts
                                           (mapcat :user-tags)
                                           set))]
    (fn []
      [:form {:no-validate true
              :on-submit (fn [e]
                           (.preventDefault e)
                           (bulk-save page-state))}
       [:fieldset
        [:legend "Tags"]
        [forms/typeahead-input
         bulk-edit
         [:working-tag]
         {:search-fn (tag-search-fn bulk-edit all-user-tags)
          :mode :direct
          :html {:on-blur #(when-let [tag (get-in @bulk-edit [:working-tag])]
                             (apply-tag bulk-edit tag))}
          :caption-fn name
          :value-fn name
          :find-fn keyword
          :on-change (fn [tag]
                       (apply-tag bulk-edit tag))}]
        (tag-elems @user-tags {:remove-fn (fn [tag]
                                            (.log js/console (prn-str {:remove tag
                                                                       :user-tags @user-tags
                                                                       }))
                                            (swap! user-tags
                                                  disj
                                                  tag))})]
       [forms/checkbox-field bulk-edit [:merge-user-tags?] {:caption "Keep existing tags"}]
       [:button.btn.btn-primary.me-2 {:title "Click here to apply these changes to the selected accounts."
                                      :type :submit}
        (if @busy?
          [:div
           (bs/spinner {:size :small})
           (html/space)
           "Save"]
          (bs/icon-with-text :check "Save"))]
       [:button.btn.btn-secondary {:title "Click here to cancel this edit operation."
                                   :type :button
                                   :on-click #(swap! page-state dissoc :bulk-edit)}
        (bs/icon-with-text :x-circle "Cancel")]])))

(defn- accounts-table
  [page-state]
  (let [all-tags (make-reaction #(->> @accounts
                                      (mapcat :user-tags)
                                      set))
        tag-items (make-reaction #(concat [[:_untagged "untagged"]]
                                          (map (fn [v]
                                                 [v (name v)])
                                               @all-tags)))
        current-entity (r/cursor app-state [:current-entity])
        bulk-select (r/cursor page-state [:bulk-edit :account-ids])
        busy? (busy page-state)]
    (fn []
      [:div.row
       [:div.col-md-6
        [:div.accounts-options
         [:div.form-check.mb-1
          [forms/checkbox-input
           page-state
           [:hide-zero-balances?]
           {:caption "Hide Zero-Balance Accounts"}]
          [:label.form-check-label {:for "hide-zero-balances?"}
           "Hide zero balances"]]
         [forms/checkbox-inputs
          page-state
          [:filter-tags]
          @tag-items
          {:container-html {:class ["d-flex flex-column"]}
           :input-container-html {:class "mb-1"}}]]
        [:table.table.table-hover
         [:thead
          [:tr
           [:th.col-md-6 "Name"]
           [:th.col-md-3.text-end "Value"]
           [:th.col-md-1 (html/space)]
           [:th.col-md-2 (html/space)]]]
         (if @accounts
           (if (seq @accounts)
             [account-and-type-rows page-state]
             [:tbody
              [:tr
               [:td {:col-span 4} "No accounts"]]])
           [:tbody
            [:tr
             [:td {:col-span 4}
              [:div.d-flex.justify-content-center.m2
               [:div.spinner-border {:role :status}
                [:span.visually-hidden "Loading"]]]]]])]
        [:button.btn.btn-primary {:on-click (fn []
                                              (swap! page-state assoc :selected {:entity-id (:id @current-entity)
                                                                                 :type :asset})
                                              (html/set-focus "parent-id"))
                                  :disabled @busy?}
         (bs/icon-with-text :plus "Add")]]
       [:div.col-md-6 {:class (when-not (seq @bulk-select) "d-none")}
        [bulk-edit-form page-state]]])))

(defn- save-account
  [page-state]
  (+busy page-state)
  (accounts/save (:selected @page-state)
                 (fn [_]
                   (fetch-accounts)
                   (swap! page-state #(-> %
                                          -busy
                                          (dissoc :selected))))
                 (handle-error page-state "Unable to save the account: %s")))

(defn- account-form
  [page-state]
  (let [account (r/cursor page-state [:selected])
        user-tags (r/cursor account [:user-tags])
        accounts  (r/cursor page-state [:accounts])
        all-user-tags (make-reaction #(->> @accounts
                                           (mapcat :user-tags)
                                           set))
        commodities (r/cursor page-state [:commodities])]
    (fn []
      [:div.row
       [:div.col-md-6
        [:h2 (if (:id @account) "Edit" "New")]
        [:form {:no-validate true
                :on-submit (fn [e]
                             (.preventDefault e)
                             (save-account page-state))}
         [forms/typeahead-field
          account
          [:parent-id]
          {:search-fn (fn [input callback]
                        (callback (find-by-path input @accounts)))
           :caption-fn :path
           :value-fn :id
           :find-fn (fn [id callback]
                      (->> @accounts
                           (filter #(= id (:id %)))
                           first
                           callback))}]
         [forms/select-field account
          [:type]
          (map (juxt name humanize) account-types)
          {:class (when (:parent-id @account) "hidden")}]
         [forms/text-field account [:name] {:validate [:required]}]
         [forms/typeahead-field
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
                      (callback (get-in @commodities [id])))}]
         [forms/checkbox-field
          account
          [:trading]
          {:caption "Check here if this account is used to trade commodities"
           :form-group-attr {:class (when-not (= :asset (:type @account)) "d-none")}}]
         [:fieldset
          [:legend "Tags"]
          [forms/typeahead-input
           account
           [:working-tag]
           {:search-fn (fn [term callback]
                         (let [existing (:user-tags @account)]
                           (->> @all-user-tags
                                (remove existing)
                                (map name)
                                (filter #(string/includes? % term))
                                callback)))
            :caption-fn name
            :value-fn name
            :find-fn keyword
            :create-fn keyword
            :on-change (fn [tag]
                         (swap! account #(-> %
                                             (update-in [:user-tags] (fnil conj #{}) tag)
                                             (dissoc :working-tag))))}]
          (tag-elems @user-tags {:remove-fn #(swap! account
                                                    update-in
                                                    [:user-tags]
                                                    disj
                                                    %)})]
         [:div
          [:button.btn.btn-primary {:type :submit
                                    :title "Click here to save the account."}
           (bs/icon-with-text :check "Save")]

          [:button.btn.btn-danger.ms-2 {:on-click #(swap! page-state dissoc :selected)
                                        :type :button
                                        :title "Click here to return to the list of accounts."}
           (bs/icon-with-text :x "Cancel")]]]]])))

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
       [:button.btn.btn-secondary.ms-2 {:on-click (fn []
                                                    (trns/stop-item-loading page-state)
                                                    (swap! page-state assoc
                                                           :items nil)
                                                    (recs/load-working-reconciliation page-state)
                                                    (trns/load-unreconciled-items page-state)
                                                    (html/set-focus "end-of-period"))
                                        :title "Click here to reconcile this account"}
        (bs/icon-with-text :check-box "Reconcile")]
       [:button.btn.btn-secondary.ms-2 {:on-click (fn []
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
    (fetch-accounts #(trns/reset-item-loading page-state))))

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
        system-tags (r/cursor page-state [:view-account :system-tags])
        disable-trade? (make-reaction #(or (:id @transaction)
                                           (not (:trading @system-tags))))]
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
        [:span.ms-auto
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
         [:th.text-end "Purchase Date"]
         [:th.text-end "Shares Purchased"]
         [:th.text-end "Shares Owned"]
         [:th.text-end "Purchase Price"]
         [:th.text-end "Gn/Ls"]
         [:th.text-end "Gn/Ls %"]]]
       [:tbody
        (doall (for [lot (sort-by (comp serialize-date :purchase-date) @lots)]
                 (let [g-l (- (* (:price @latest-price)
                                 (:shares-owned lot))
                              (* (:purchase-price lot)
                                 (:shares-owned lot)))]
                   ^{:key (str "lot-" (:id lot))}
                   [:tr
                    [:td.text-end (format-date (:purchase-date lot))]
                    [:td.text-end (format-decimal (:shares-purchased lot) 4)]
                    [:td.text-end (format-decimal (:shares-owned lot) 4)]
                    [:td.text-end (format-decimal (:purchase-price lot) 2)]
                    [:td.text-end
                     {:class (if (>= g-l 0M)
                               "text-success"
                               "text-danger")}
                     (format-decimal g-l)]
                    [:td.text-end
                     {:class (if (>= @gain-loss 0M)
                               "text-success"
                               "text-danger")}
                     (format-percent (/ g-l
                                        (* (:shares-purchased lot)
                                           (:purchase-price lot)))
                                     3)]])))]
       [:tfoot
        [:tr
         [:td.text-end {:col-span 2}
          (when @latest-price
            (gstr/format "(%s as of %s)"
                         (currency-format (:price @latest-price))
                         (format-date (:trade-date @latest-price))))]
         [:td.text-end (format-decimal @total-shares 4)]
         [:td.text-end (currency-format @total-value)]
         [:td.text-end {:class (if (>= @gain-loss 0M) "text-success" "text-danger")}
          (currency-format @gain-loss)]
         [:td.text-end {:class (if (>= @gain-loss 0M) "text-success" "text-danger")}
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
         [:button.btn.btn-light {:title "Click here to return the the account list."
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
  (let [system-tags (r/cursor page-state [:view-account :system-tags])]
    (fn []
      (cond
        (:tradable @system-tags) [tradable-account-details page-state]
        :else [currency-account-details page-state]))))

(defn- load-commodities
  [page-state]
  (+busy page-state)
  (commodities/select (fn [result]
                        (swap! page-state #(-> %
                                               -busy
                                               (assoc :commodities (->> result
                                                                        (map (juxt :id identity))
                                                                        (into {}))))))
                      (handle-error page-state "Unable to load the commodities: %s")))

(defn- index []
  (let [page-state (r/atom {:expanded #{}
                            :filter-tags #{}
                            :ctl-chan (chan)})
        selected (r/cursor page-state [:selected])
        view-account (r/cursor page-state [:view-account])]
    (load-commodities page-state)

    (add-watch current-entity
               ::refine-state
               (fn [_ _ _ entity]
                 (when entity
                   (swap! page-state assoc
                          :hide-zero-balances? (->> (or @accounts [])
                                                    (map :value)
                                                    (not-every? #(= 0 %)))

                          :expanded #{}
                          :filter-tags #{}))))

    (fn []
      [:div.mt-5
       [:div.accounts-header
        [:h1.accounts-title (str "Accounts" (when @view-account
                                              (str " - " (:name @view-account))))]]
       (when-not (or @selected @view-account)
         [accounts-table page-state])
       (when @selected
         [account-form page-state])
       (when @view-account
         [account-details page-state])])))

(secretary/defroute "/accounts" []
  (swap! app-state assoc :page #'index))
