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
            [dgknght.app-lib.dom :refer [set-focus]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.decimal :as decimal]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.forms-validation :as v]
            [dgknght.app-lib.notifications :as notify]
            [dgknght.app-lib.bootstrap-5 :as bs :refer [nav-tabs]]
            [clj-money.icons :refer [icon
                                     icon-with-text]]
            [clj-money.components :refer [load-on-scroll
                                          button]]
            [clj-money.api.commodities :as commodities]
            [clj-money.api.accounts :as accounts]
            [clj-money.api.lots :as lots]
            [clj-money.api.prices :as prices]
            [clj-money.transactions :refer [can-simplify?]]
            [clj-money.cached-accounts :refer [fetch-accounts]]
            [clj-money.accounts :refer [account-types
                                        allocate
                                        find-by-path]]
            [clj-money.state :refer [app-state
                                     current-entity
                                     accounts
                                     accounts-by-id
                                     +busy
                                     -busy
                                     busy?]]
            [clj-money.views.transactions :as trns]
            [clj-money.views.reconciliations :as recs]
            [clj-money.views.attachments :as atts]))

(defn- delete
  [account]
  (when (js/confirm (str "Are you sure you want to delete the account " (:name account) "?"))
    (+busy)
    (accounts/delete account
                     :callback -busy
                     :on-success fetch-accounts)))

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

(defn- abbr-acct-name
  [account]
  (let [words (string/split (:name account) #"\s+")]
    (if (< 3 (count words))
      [:span {:title (:name account)
              :data-bs-toggle :tooltip
              :data-bs-placement :top}
       (str
         (->> words
              (take 2)
              (string/join " "))
         "...")]
      (:name account))))

(defn- prepare-for-allocation
  [{:keys [children-value] :as account}]
  (if (:allocations account)
    account
    (assoc account :allocations (->> @accounts
                                     (filter #(= (:id account)
                                                 (:parent-id %)))
                                     (reduce #(assoc %1
                                                     (:id %2)
                                                     (decimal/* 100M
                                                                (decimal// (:total-value %2)
                                                                           children-value)))
                                             {})))))

(defn- account-row
  [{:keys [id parent-ids system-tags] :as account} expanded page-state]
  ^{:key (str "account-" id)}
  [:tr
   [:td [:span.account-depth {:class (str "account-depth-" (count parent-ids))}
         [:span.toggle-ctl {:on-click #(toggle-account (:id account) page-state)
                            :class (when-not (:has-children? account)
                                     "invisible")}
          (icon (if (expanded id)
                     :arrows-collapse
                     :arrows-expand)
                   :size :small)]
         (abbr-acct-name account)]]
   [:td.text-end.d-none.d-sm-table-cell.value-depth
    [:span {:class (str "value-depth-" (count parent-ids))}
     (currency-format (:total-value account))]]
   [:td.text-center.d-none.d-md-table-cell
    [forms/checkbox-input page-state [:bulk-edit :account-ids] {:no-bootstrap? true
                                                                :html {:name "bulk-edit-id"}
                                                                :value id}]]
   [:td.text-center
    [:div.btn-group
     [:button.btn.btn-secondary.btn-sm {:on-click #(swap! page-state assoc :view-account account)
                                    :title "Click here to view transactions for this account."}
      (icon :collection :size :small)]
     [:button.btn.btn-secondary.btn-sm {:on-click (fn []
                                               (swap! page-state assoc :selected account)
                                               (set-focus "parent-id"))
                                   :title "Click here to edit this account."}
      (icon :pencil :size :small)]
     [:button.btn.btn-secondary {:on-click #(swap! page-state assoc :allocation {:account (prepare-for-allocation account)
                                                                             :cash (:value account)
                                                                             :withdrawal 0M})
                             :disabled (not (system-tags :trading))
                             :title "Click here to manage asset allocation for this account."}
      (icon (if (system-tags :trading)
                 :pie-chart-fill
                 :pie-chart)
               :size :small)]
     [:button.btn.btn-danger.btn-sm {:on-click #(delete account)
                                     :title "Click here to remove this account."}
      (icon :x-circle :size :small)]]]])

(defn- account-type-row
  [account-type group expanded page-state]
  ^{:key (str "account-type" account-type)}
  [:tr.account-type {:id (str "account-type-" account-type)}
   [:td
    [:span.toggle-ctl {:aria-hidden true
                       :on-click #(toggle-account account-type page-state)}
     (icon (if (expanded account-type)
                :arrows-collapse
                :arrows-expand)
              :size :small)]
    (name account-type)]
   [:td.text-end.d-none.d-sm-table-cell
    (currency-format (->> group
                          (map :value)
                          (reduce decimal/+)))]
   [:td {:col-span 2} (html/space)]])

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
  (+busy)
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
                       (-busy)
                       (swap! page-state #(dissoc % :bulk-edit))
                       (fetch-accounts)
                       (notify/toast "Updated Finished"
                                     (str "Updated "
                                          (:succeeded @results)
                                          " account(s)."))))
        success-fn #(receive-fn (fn [state] (update-in state [:succeeded] inc)))
        error-fn #(receive-fn (fn [state] (update-in state [:errors] conj %)))
        apply-fn (if merge-user-tags?
                   #(update-in % [:user-tags] union user-tags)
                   #(assoc % :user-tags user-tags))
        to-update (->> account-ids
                       (map @accounts-by-id)
                       (map apply-fn))]
    (doseq [account to-update]
      (accounts/save account
                     :callback -busy
                     :on-success success-fn
                     :on-error error-fn))))

(defn- tag-elem
  [tag {:keys [remove-fn]}]
  (let [tag-name (name tag)]
    ^{:key (str "tag-" tag-name)}
    [:div.tag.account-tag.d-flex
     [:span.me-2 tag-name]
     [:a.ms-auto {:href "#"
                  :title "Click here to remove this tag"
                  :on-click (fn [] (remove-fn tag))}
      (icon :x :size :small)]]))

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
        (tag-elems @user-tags {:remove-fn #(swap! user-tags disj %)})]
       [forms/checkbox-field bulk-edit [:merge-user-tags?] {:caption "Keep existing tags"}]
       [button {:html {:title "Click here to apply these changes to the selected accounts."
                       :class "btn-primary"
                       :type :submit}
                :caption "Save"
                :icon :check}]
       [button {:html {:title "Click here to cancel this edit operation."
                       :class "btn-secondary ms-2"
                       :type :button
                       :on-click #(swap! page-state dissoc :bulk-edit)}
                :caption "Cancel"
                :icon :x}]])))

(defn- accounts-table
  [page-state]
  (let [selected (r/cursor page-state [:selected])
        view-account (r/cursor page-state [:view-account])
        allocation-account (r/cursor page-state [:allocation :account])
        hide? (make-reaction #(or @selected @view-account @allocation-account))
        bulk-select (r/cursor page-state [:bulk-edit :account-ids])]
    (fn []
      [:div.row {:class (when @hide? "d-none")}
       [:div.col-lg-8
        [:table.table.table-hover
         [:thead
          [:tr
           [:th.col-md-6 "Name"]
           [:th.col-md-3.text-end.d-none.d-sm-table-cell "Value"]
           [:th.col-md-1.d-none.d-md-table-cell (html/space)]
           [:th.col-md-2 (html/space)]]]
         (cond
           (seq @accounts)
           [account-and-type-rows page-state]

           @accounts
           [:tbody
            [:tr
             [:td {:col-span 4} "No accounts"]]]

           :else
           [:tbody
            [:tr
             [:td {:col-span 4}
              [:div.d-flex.justify-content-center.m2
               [:div.spinner-border {:role :status}
                [:span.visually-hidden "Loading"]]]]]])]
        [button {:html {:class "btn-primary"
                        :on-click (fn []
                                    (swap! page-state assoc :selected {:entity-id (:id @current-entity)
                                                                       :type :asset})
                                    (set-focus "parent-id"))
                        :disabled @busy?}
                 :caption "Add"
                 :icon :plus}]]
       [:div.col-lg-4 {:class (when-not (seq @bulk-select) "d-none")}
        [bulk-edit-form page-state]]])))

(defn- prepare-for-save
  [{:keys [parent-id] :as account}]
  (let [parent (->> @accounts
                    (filter #(= (:id %) parent-id))
                    first)]
    (cond-> (update-in account
                       [:allocations]
                       (fn [allocations]
                         (when allocations
                           (->> allocations
                                (remove (fn [[_ v]] (decimal/zero? v)))
                                (into {})))))
      parent (merge (select-keys parent [:type])))))

(defn- save-account
  [page-state]
  (+busy)
  (-> (some #(get-in @page-state %) [[:selected]
                                     [:allocation :account]])
      prepare-for-save
      (accounts/save :callback -busy
                     :on-success (fn [_saved]
                                   (fetch-accounts)
                                   (swap! page-state dissoc :selected :allocation)))))

(defn- account-form
  [page-state]
  (let [account (r/cursor page-state [:selected])
        user-tags (r/cursor account [:user-tags])
        all-user-tags (make-reaction #(->> @accounts
                                           (mapcat :user-tags)
                                           set))
        commodities (r/cursor page-state [:commodities])
        hide-type? (make-reaction #(:parent-id @account))]
    (fn []
      (when @account
        [:div.row {:class (when-not @account "d-none")}
         [:div.col-md-6
          [:h2 (if (:id @account) "Edit" "New")]
          [:form {:no-validate true
                  :on-submit (fn [e]
                               (.preventDefault e)
                               (v/validate account)
                               (when (v/valid? account)
                                 (save-account page-state)))}
           [forms/typeahead-field
            account
            [:parent-id]
            {:search-fn (fn [input callback]
                          (->> @accounts
                               (find-by-path input)
                               callback))
             :caption-fn (comp (partial string/join "/") :path)
             :value-fn :id
             :find-fn (fn [id callback]
                        (->> @accounts
                             (filter #(= id (:id %)))
                             first
                             callback))}]
           [forms/select-field account
            [:type]
            (map (juxt name humanize) account-types)
            {:hide? hide-type?}]
           [forms/text-field account [:name] {:validations #{::v/required}}]
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
                        (callback (get-in @commodities [id])))
             :validations #{::v/required}}]
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
            [button {:html {:type :submit
                            :class "btn-primary"
                            :title "Click here to save the account."}
                     :caption "Save"
                     :icon :check}]
            [button {:html {:on-click #(swap! page-state dissoc :selected)
                            :class "btn-secondary ms-2"
                            :type :button
                            :title "Click here to return to the list of accounts."}
                     :caption "Cancel"
                     :icon :x}]]]]]))))

(defn- new-transaction
  [page-state]
  (let [account-id (get-in @page-state [:view-account :id])]
    (swap! page-state assoc
           :transaction (trns/mode
                         {:entity-id (:id @current-entity)
                          :transaction-date (t/today)
                          :account-id account-id}
                         ::trns/simple)))
  (set-focus "transaction-date"))

(defn- account-buttons
  [page-state]
  (let  [transaction (r/cursor page-state [:transaction])]
    (fn []
      [:div.d-flex.justify-content-between
       [:button.btn.btn-primary {:on-click #(new-transaction page-state)
                                 :disabled (not (nil? @transaction))}
        (icon-with-text :plus "Add")]
       [:button.btn.btn-secondary.ms-2.d-none.d-md-block
        {:on-click (fn []
                     (trns/stop-item-loading page-state)
                     (swap! page-state dissoc :items)
                     (recs/load-working-reconciliation page-state)
                     (trns/load-unreconciled-items page-state)
                     (set-focus "end-of-period"))
         :title "Click here to reconcile this account"}
        (icon-with-text :check-square "Reconcile")]
       [:button.btn.btn-secondary.ms-2 {:on-click (fn []
                                                    (trns/stop-item-loading page-state)
                                                    (swap! page-state dissoc
                                                           :view-account
                                                           :items
                                                           :all-items-fetched?))
                                        :title "Click here to return to the account list."}
        (icon-with-text :arrow-left-short "Back")]])))

(defn- refresh-accounts
  [page-state]
  (fn [xf]
    (completing
      (fn [ch res]
        (fetch-accounts (map (fn [accounts]
                               ; The :view-account might now be stale, e.g., if the new transaction
                               ; changes :latest-transaction-date
                               (let [account (get-in @page-state [:view-account])
                                     updated (->> accounts
                                                  (filter #(= (:id account)
                                                              (:id %)))
                                                  first)]
                                 (swap! page-state assoc :view-account updated))
                               (xf ch res))))))))

(defn- refresh-items
  [page-state]
  (fn [xf]
    (completing
      (fn [ch res]
        (trns/reset-item-loading page-state)
        (xf ch res)))))

(defn- post-transaction-save
  [page-state]
  (comp (refresh-accounts page-state)
        (refresh-items page-state)))

(defn- do-tab-nav
  [mode page-state]
  (let [{:keys [view-account commodities transaction]} @page-state
        unprep-fn (get-in (trns/untransformations)
                          [(trns/mode transaction)])
        prep-fn (get-in (trns/transformations view-account
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
    {:label (humanize (name mode))
     :disabled? disabled?
     :id (str "entry-mode-" (name mode))
     :active? (= current-mode mode)
     :nav-fn #(do-tab-nav mode page-state)}))

(defn- neutralize
  [transaction mode]
  (let [f (get-in (trns/untransformations)
                  [mode])]
    (f transaction)))

(defn- transaction-form
  [page-state]
  (let [transaction (r/cursor page-state [:transaction])
        mode (make-reaction #(trns/mode @transaction))
        neutralized (make-reaction #(neutralize @transaction @mode))
        system-tags (r/cursor page-state [:view-account :system-tags])
        disable-trade? (make-reaction #(or (:id @transaction)
                                           (not (:trading @system-tags))))]
    (fn []
      [:<>
       [:h3 (if (:id @transaction)
              "Edit Transaction"
              "New Transaction")]
       [:div.mt-3
        (nav-tabs {:class "mb-3"}
                  (map #(transaction-form-nav-tab % page-state)
                       [{:mode ::trns/simple
                         :disabled? (not (can-simplify? @neutralized))}
                        {:mode ::trns/full
                         :disabled? false}
                        {:mode ::trns/trade
                         :disabled? @disable-trade?}
                        {:mode ::trns/dividend
                         :disabled? @disable-trade?}]))
        [trns/full-transaction-form page-state]
        [trns/simple-transaction-form page-state]
        [trns/trade-transaction-form page-state]
        [trns/dividend-transaction-form page-state]]
       [:div
        [:button.btn.btn-primary {:on-click #(trns/save-transaction page-state (post-transaction-save page-state))
                                  :title "Click here to save the transaction"}
         (icon-with-text :check "Save")]
        [:button.btn.btn-secondary.ms-2 {:on-click #(swap! page-state dissoc :transaction)
                                         :title "Click here to cancel this transaction"}
         (icon-with-text :x "Cancel")]]])))

(defn- check-all-items
  ([page-state]  (check-all-items page-state true))
  ([page-state checked?]
  (swap! page-state
         update-in
         [:reconciliation :item-refs]
         merge
         (->> (:items @page-state)
              (map (comp #(vector % checked?)
                         :id))
              (into {})))))

(defn- uncheck-all-items
  [page-state]
  (check-all-items page-state false))

(defn- transaction-item-list
  [page-state]
  (let [ctl-chan (r/cursor page-state [:ctl-chan])
        all-items-fetched? (r/cursor page-state [:all-items-fetched?])
        reconciliation (r/cursor page-state [:reconciliation])]
    (fn []
      [:<>
       [:div.d-flex.flex-row-reverse {:class (when-not @reconciliation "d-none")}
        [:button.btn.btn-secondary {:on-click #(check-all-items page-state)
                                :title "Click here to mark all items as reconciled"}
         (icon :check-box :size :small)]
        [:button.btn.btn-secondary.ms-2 {:on-click #(uncheck-all-items page-state)
                                     :title "Click here to mark all items as unreconciled"}
         (icon :unchecked-box :size :small)]]

       [:div.d-flex.flex-column.h-75
        [:div#items-container.flex-grow-1.overflow-auto {:style {:height "0"}}
         [trns/items-table page-state]]
        [:div.d-flex.mt-2 {:style {:flex :none}}
         [account-buttons page-state]
         [:span.ms-auto
          [load-on-scroll {:target "items-container"
                           :all-items-fetched? @all-items-fetched?
                           :load-fn #(go (>! @ctl-chan :fetch))}]]]]])))

(defn- currency-account-details
  [page-state]
  (let [transaction (r/cursor page-state [:transaction])
        reconciliation (r/cursor page-state [:reconciliation])
        attachments-item (r/cursor page-state [:attachments-item])
        selected-attachment (r/cursor page-state [:selected-attachment])]
    (trns/init-item-loading page-state)
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
    (lots/select {:account-id parent-id
                  :commodity-id commodity-id
                  :shares-owned [:!= 0]}
                 :on-success #(swap! page-state assoc :lots %))
    (prices/select {:commodity-id commodity-id
                    :trade-date [earliest-transaction-date
                                 latest-transaction-date]}
                   :on-success #(swap! page-state assoc :prices %))
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
                                                      ^{::trns/mode ::trns/trade}
                                                      {:trade-date (t/today)
                                                       :entity-id entity-id
                                                       :account-id parent-id
                                                       :commodity-id commodity-id
                                                       :commodity-account-id (:id @account)})
                                               (set-focus "trade-date"))}
          (icon-with-text :plus "Buy/Sell")]
         (html/space)
         [:button.btn.btn-secondary {:title "Click here to return the the account list."
                                 :on-click #(swap! page-state dissoc :view-account)}
          (icon-with-text :arrow-left-short "Back")]]]])))

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
              (icon-with-text :check "Save")]
             (html/space)
             [:button.btn.btn-secondary {:on-click #(swap! page-state dissoc :transaction)
                                         :title "Click here to cancel this transaction"}
              (icon-with-text :x "Cancel")]]]]]]
        [tradable-account-items page-state]))))

(defn- account-details
  [page-state]
  (let [system-tags (r/cursor page-state [:view-account :system-tags])]
    (fn []
      (when @system-tags
        (cond
          (:tradable @system-tags) [tradable-account-details page-state]
          :else [currency-account-details page-state])))))

(defn- asset-allocation-row
  [{:keys [account adj-value target-value current-value current-percentage]} parent hide-zero-balances?]
  ^{:key (str "asset-allocation-row-" (:id account))}
  [:tr {:class (when (and (decimal/zero? (:quantity account))
                          hide-zero-balances?)
                 "d-none")}
   [:td
    [:div.d-none.d-lg-block
     (:name account)]
    [:div.text-truncate.d-lg-none {:style {:max-width "5em"}}
     (get-in account [:commodity :symbol])]]
   [:td.text-end [forms/decimal-input parent [:allocations (:id account)]]]
   [:td.text-end.d-none.d-lg-table-cell (currency-format target-value)]
   [:td.text-end.d-none.d-lg-table-cell (format-percent current-percentage)]
   [:td.text-end.d-none.d-lg-table-cell (currency-format current-value)]
   [:td.text-end (currency-format adj-value)]])

(defn- asset-allocation
  [page-state]
  (let [allocation (r/cursor page-state [:allocation])
        account (r/cursor allocation [:account])
        cash (r/cursor allocation [:cash])
        withdrawal (r/cursor allocation [:withdrawal])
        allocations (make-reaction #(sort-by (comp :name :account)
                                             (allocate @account @accounts-by-id {:cash @cash
                                                                                 :withdrawal @withdrawal})))
        total-percent (make-reaction #(decimal// (reduce decimal/+ 0M (vals (:allocations @account)))
                                                 100M))
        total-percent-class (make-reaction #(if (decimal/zero? (decimal/- 1M @total-percent))
                                              "text-success"
                                              "text-danger"))
        hide-zero-balances? (r/cursor page-state [:hide-zero-balances?])]
    (fn []
      [:div {:class (when-not @account "d-none")}
       [:h2 "Asset Allocation for " (:name @account) " " (currency-format (:total-value @account))]
       [:div.row
        [:div.col-md-6
         [:table.table.table-borderless
          [:tbody
           [:tr
            [:th {:scope :row} "Cash"]
            [:td (currency-format (:quantity @account))]]
           [:tr
            [:th {:scope :row} "Cash to reserve"]
            [:td
             [forms/decimal-input allocation [:cash]]]]
           [:tr
            [:th {:scope :row} "Cash to withdraw"]
            [:td
             [forms/decimal-input allocation [:withdrawal]]]]]]]]
       [:table.table
        [:thead
         [:tr
          [:th "Account"]
          [:th.text-end "Target %"]
          [:th.text-end.d-none.d-lg-table-cell "Target $"]
          [:th.text-end.d-none.d-lg-table-cell  "Current %"]
          [:th.text-end.d-none.d-lg-table-cell  "Current $"]
          [:th.text-end.text-nowrap "Adj. $"]]]
        [:tbody
         (if (seq @allocations)
           (->> @allocations
                (map #(asset-allocation-row % account @hide-zero-balances?))
                doall)
           [:tr [:td {:col-span 6} "This account has no child accounts."]])]
        [:tfoot
         [:tr
          [:td (html/space)]
          [:td {:col-span 5 :class @total-percent-class} (format-percent @total-percent)]]]]
       [:div
        [:button.btn.btn-primary {:title "Click here to save these allocations."
                                  :on-click #(save-account page-state)}
         (icon-with-text :check "Save")]
        [:button.btn.btn-secondary.ms-2 {:title "Click here to to cancel and return to the account list."
                                         :on-click #(swap! page-state dissoc :allocation)}
         (icon-with-text :x-circle "Cancel")]]])))

(defn- load-commodities
  [page-state]
  (+busy)
  (commodities/select {}
                      :callback -busy
                      :on-success #(swap! page-state
                                          assoc
                                          :commodities
                                          (->> %
                                               (map (juxt :id identity))
                                               (into {})))))

(defn- account-filter
  [page-state]
  (let [all-tags (make-reaction #(->> @accounts
                                      (mapcat :user-tags)
                                      set))
        tag-items (make-reaction #(concat [[:_untagged "untagged"]]
                                          (map (fn [v]
                                                 [v (name v)])
                                               @all-tags)))
        selected (r/cursor page-state [:selected])
        view-account (r/cursor page-state [:view-account])
        hide? (make-reaction #(or @selected @view-account))]
    (fn []
      [:div.accounts-options {:class (when @hide? "d-none")}
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
        tag-items
        {:container-html {:class ["d-flex flex-column"]}
         :input-container-html {:class "mb-1"}}]])))

(defn- account-filter-container
  [page-state]
  [:div#account-filter.offcanvas.offcanvas-end {:tab-index -1}
   [:div.offcanvas-header
    [:h3.off-canvas-title "Filter"]
    [:button.btn-close.text-reset {:data-bs-dismiss "offcanvas"
                                   :aria-label "Close"}]]
   [:div.offcanvas-body [account-filter page-state]]])

(defn- any-non-zero-balances?
  ([] (any-non-zero-balances? @accounts))
  ([accounts]
  (->> accounts
       (map :value)
       (not-every? #(= 0 %)))))

(defn- index []
  (let [page-state (r/atom {:expanded #{}
                            :hide-zero-balances? (any-non-zero-balances?)
                            :filter-tags #{}
                            :ctl-chan (chan)})
        view-account (r/cursor page-state [:view-account])
        selected (r/cursor page-state [:selected])
        hide-funnel? (make-reaction #(or @selected @view-account))]
    (load-commodities page-state)
    (add-watch current-entity
               ::index
               (fn [_ _ _ accts]
                 (swap! page-state
                        #(-> %
                             (dissoc :selected
                                     :view-account
                                     :commodities
                                     :all-items-fetched?
                                     :items)
                             (assoc
                               :expanded #{}
                               :filter-tags #{}
                               :hide-zero-balances? (any-non-zero-balances? accts))))
                 (load-commodities page-state)))
    (fn []
      [:div.mt-3.h-100
       [:div.row
        [:div.col-lg-8
         [:div.d-flex
          [:h1.accounts-title.me-auto (str "Accounts" (when @view-account
                                                        (str " - " (:name @view-account))))]
          [:button.btn.btn-dark {:type :button
                                 :class (when @hide-funnel? "d-none")
                                 :data-bs-toggle "offcanvas"
                                 :data-bs-target "#account-filter"
                                 :aria-controls "account-filter"}
           (icon :funnel :size :small)]]]]
       [account-filter-container page-state]
       [accounts-table page-state]
       [account-form page-state]
       [account-details page-state]
       [asset-allocation page-state]])))

(secretary/defroute "/accounts" []
  (swap! app-state assoc :page #'index))
