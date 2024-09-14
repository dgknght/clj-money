(ns clj-money.views.transactions
  (:require [clojure.set :refer [rename-keys]]
            [clojure.string :as string]
            [clojure.zip :as zip]
            [goog.string :as gstr]
            [cljs.core.async :as a :refer [<! >! go go-loop]]
            [cljs-time.core :as t]
            [reagent.core :as r]
            [reagent.format :refer [currency-format]]
            [reagent.ratom :refer [make-reaction]]
            [dgknght.app-lib.core :refer [parse-int]]
            [dgknght.app-lib.web :refer [format-date
                                         format-decimal ]]
            [dgknght.app-lib.inflection :refer [humanize]]
            [dgknght.app-lib.dom :refer [set-focus
                                         key-code
                                         shift-key?]]
            [dgknght.app-lib.html :refer [space
                                          special-char]]
            [dgknght.app-lib.decimal :as decimal]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.notifications :as notify]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [clj-money.state :refer [accounts
                                     accounts-by-id
                                     +busy
                                     -busy]]
            [clj-money.dnd :as dnd]
            [clj-money.util :as util :refer [debounce]]
            [clj-money.dates :as dates]
            [clj-money.accounts :refer [polarize-quantity
                                        find-by-path
                                        format-quantity]]
            [clj-money.transactions :refer [simplify
                                            fullify
                                            can-simplify?
                                            entryfy
                                            unentryfy
                                            ensure-empty-item
                                            tradify
                                            untradify]]
            [clj-money.components :refer [load-in-chunks]]
            [clj-money.api.transaction-items :as transaction-items]
            [clj-money.api.transactions :as transactions]
            [clj-money.api.attachments :as att]
            [clj-money.api.trading :as trading]))

(defn- fullify-trx
  [trx]
  (fullify trx @accounts-by-id))

(defn mode
  ([transaction]
   (-> transaction meta ::mode))
  ([transaction mode]
   (vary-meta transaction assoc ::mode mode)))

(defn- mode?
  [transaction mode]
  (= mode
     (-> transaction meta ::mode)))

(defn- prepare-transaction-for-edit
  [transaction account]
  (if (can-simplify? transaction)
    (-> transaction
        (simplify account)
        (mode ::simple))
    (-> transaction
        entryfy
        (mode ::full))))

(defn- item->tkey
  [item]
  (-> item
      (select-keys [:transaction-id :transaction-date])
      (rename-keys {:transaction-id :id})))

(defn- edit-transaction
  [item page-state]
  (+busy)
  (transactions/get (item->tkey item)
                    (map (fn [result]
                           (-busy)
                           (let [prepared (prepare-transaction-for-edit
                                            result
                                            (:view-account @page-state))]
                             (swap! page-state assoc :transaction prepared))
                           (set-focus "transaction-date")))))

(defn load-unreconciled-items
  [page-state]
  (+busy)
  (let [account (:view-account @page-state)
        criteria {:account-id (:id account)
                  :transaction-date [(:earliest-transaction-date account)
                                     (:latest-transaction-date account)]
                  :unreconciled true
                  :include-children (:include-children? @page-state)}]
    (transaction-items/search
      criteria
      (map (fn [items]
             (-busy)
             (swap! page-state assoc :items items))))))

(defn- load-attachments
  [page-state]
  (let [{:keys [attachments-item]} @page-state
        criteria {:transaction-id (:transaction-id attachments-item)
                  :transaction-date (:transaction-date attachments-item)}]
    (att/search criteria
                (map #(swap! page-state assoc :attachments %)))))

(defn- fetch-items
  [xf]
  (completing
    (fn [ch criteria]
      (transaction-items/search criteria
                                (map #(xf ch %))))))

(defn init-item-loading
  [page-state]
  (let [account (get-in @page-state [:view-account])
        start (:earliest-transaction-date account)
        end (:latest-transaction-date account)]
    (when (and start end)
      (swap! page-state dissoc :items :all-items-fetched?)
      (let [{:keys [ctl-ch items-ch]} (->> (dates/desc-ranges start end (t/months 6))
                                           (map vec)
                                           (load-in-chunks {:fetch-xf (comp (map #(hash-map :account-id (:id account)
                                                                                            :transaction-date %))
                                                                            fetch-items)
                                                            :chunk-size 100}))]
        (go-loop [items (<! items-ch)]
                 (if items
                   (do
                     (swap! page-state update-in [:items] (fnil concat []) items)
                     (recur (<! items-ch)))
                   (swap! page-state assoc :all-items-fetched? true)))

        (swap! page-state assoc :ctl-chan ctl-ch)
        (go (>! ctl-ch :fetch))))))

(defn stop-item-loading
  [page-state]
  (let [{:keys [all-items-fetched? ctl-chan]} @page-state]
    (when-not all-items-fetched?
      (go (>! ctl-chan :quit)))))

(defn reset-item-loading
  [page-state]
  (swap! page-state dissoc :items :transaction)
  (stop-item-loading page-state)
  (go (<! (a/timeout 250)) ; give the pipes some time to flush out
      (init-item-loading page-state)))

(defn- delete-transaction
  [item page-state]
  (when (js/confirm "Are you sure you want to delete this transaction?")
    (transactions/delete (item->tkey item)
                         (map #(reset-item-loading page-state)))))

(defn- post-item-row-drop
  [page-state item {:keys [body]}]
  (swap! page-state
         (fn [state]
           (-> state
               (update-in [:item-row-styles]
                          dissoc
                          (:id item))
               (update-in [:items] (fn [items]
                                     (map (fn [item]
                                            (if (= (:transaction-id body)
                                                   (:transaction-id item))
                                              (update-in item [:attachment-count] inc)
                                              item))
                                          items))))))
  (notify/toast "Success" "The attachment was saved successfully."))

(defn- handle-item-row-drop
  [item e page-state]
  (.preventDefault e)
  (att/create {:transaction-id (:transaction-id item) ; TODO: use transaction-ref to combine these?
               :transaction-date (:transaction-date item)
               :file (first (dnd/files e))}
              (map (partial post-item-row-drop page-state item))))

(defn- item-row
  [{:keys [attachment-count] :as item} page-state]
  (let [account (r/cursor page-state [:view-account])
        reconciliation (r/cursor page-state [:reconciliation])
        styles (r/cursor page-state [:item-row-styles])]
    ^{:key (str "item-row-" (:id item))}
    [:tr {:on-drag-enter #(swap! page-state
                                 assoc-in
                                 [:item-row-styles (:id item)]
                                 {:background-color "var(--primary)"
                                  :color "var(--white)"
                                  :cursor :copy})
          :on-drag-leave (debounce 100 #(swap! page-state update-in [:item-row-styles] dissoc (:id item)))
          :on-drag-over #(.preventDefault %)
          :on-drop #(handle-item-row-drop item % page-state)
          :style (get-in @styles [(:id item)])}
     [:td.text-end
      [:span.d-md-none (format-date (:transaction-date item) "M/d")]
      [:span.d-none.d-md-inline (format-date (:transaction-date item))]]
     [:td {:style (get-in @styles [(:id item)])} (:description item)]
     [:td.text-end (format-quantity (polarize-quantity item @account)
                                    @account)]
     [:td.text-center.d-none.d-md-table-cell
      (if @reconciliation
        [forms/checkbox-input reconciliation [:item-refs (:id item)] {::forms/decoration ::forms/none}]
        (bs/icon
         (case (:reconciliation-status item)
           :completed :check-box
           :new       :dot
           :unchecked-box)
         {:size :small}))]
     (when-not @reconciliation
       [:td.text-end.d-none.d-md-table-cell (format-quantity (:balance item)
                                                             @account)])
     (when-not @reconciliation
       [:td
        [:div.btn-group
         [:button.btn.btn-light.btn-sm {:on-click #(edit-transaction item page-state)
                                       :title "Click here to edit this transaction."}
          (bs/icon :pencil {:size :small})]
         [:button.btn.btn-light.btn-sm.d-none.d-md-block
          {:on-click (fn []
                       (swap! page-state
                              assoc
                              :attachments-item
                              item)
                       (load-attachments page-state))
           :title "Click here to view attachments for this transaction"}
          (if (zero? attachment-count)
            (bs/icon :paperclip {:size :small})
            [:span.badge.bg-secondary attachment-count])]
         [:button.btn.btn-danger.btn-sm {:on-click #(delete-transaction item page-state)
                                         :title "Click here to remove this transaction."}
          (bs/icon :x-circle {:size :small})]]])]))

(defn items-table
  [page-state]
  (let [items (r/cursor page-state [:items])
        include-children? (r/cursor page-state [:include-children?])
        account (r/cursor page-state [:view-account])
        reconciliation (r/cursor page-state [:reconciliation])
        filter-fn (make-reaction (fn []
                                   (if @include-children?
                                     identity
                                     #(= (:id @account)
                                         (:account-id %)))))]
    (fn []
      [:table.table.table-striped.table-hover
       [:thead
        [:tr
         [:th.text-end "Date"]
         [:th "Description"]
         [:th.text-end "Amount"]
         [:th.text-center.d-none.d-md-table-cell "Rec."]
         (when-not @reconciliation
           [:th.text-end.d-none.d-md-table-cell "Balance"])
         (when-not @reconciliation
           [:th (space)])]]
       [:tbody
        (if @items
          (->> @items
               (filter @filter-fn)
               (map #(item-row % page-state))
               doall)
          [:tr [:td {:col-span 6} [:span.inline-status "Loading..."]]])]])))

(defn fund-transactions-table
  [page-state]
  (let [items (r/cursor page-state [:items])
        account  (r/cursor page-state [:view-account])]
    ; I don't think we need to chunk this, but maybe we do
    (transaction-items/search {:account-id (:id @account)
                               :transaction-date [(:earliest-transaction-date @account)
                                                  (:latest-transaction-date @account)]}
                              (map #(swap! page-state assoc :items %)))
    (fn []
      [:table.table.table-hover.table-borderless
       [:thead
        [:tr
         [:th.text-end "Transaction Date"]
         [:th "Description"]
         [:th.text-end "Qty."]
         [:th.text-end "Bal."]
         [:th.text-end "Value"]]]
       [:tbody
        (doall (for [item (sort-by :index > @items)]
                 ^{:key (str "item-" (:id item))}
                 [:tr
                  [:td.text-end (format-date (:transaction-date item))]
                  [:td (:description item)]
                  [:td.text-end (format-decimal (polarize-quantity item @account) 4)]
                  [:td.text-end (format-decimal (:balance item), 4)]
                  [:td.text-end (currency-format (:value item))]]))]])))

(defn- ensure-entry-state
  [page-state]
  (swap! page-state update-in [:transaction] ensure-empty-item))

(def ^:private item-nav-fields
  ["account-id" "memo" "credit-quantity" "debit-quantity"])

(defn- item-nav-fields-zip []
  (zip/seq-zip (cycle item-nav-fields)))

(defn- prev-item-nav-field
  [current]
  (loop [fields (zip/next (item-nav-fields-zip))]
    (if (= current (first (zip/next fields)))
      (first fields)
      (recur (zip/next fields)))))

(defn- next-item-nav-field
  [current]
  (loop [fields (zip/next (item-nav-fields-zip))]
    (if (= current (first fields))
      (first (zip/next fields))
      (recur (zip/next fields)))))

(defn- arrow-key?
  [e]
  (#{:up :down :left :right} (key-code e)))

(defn- item-navigate
  [e item-count]
  (when-not (shift-key? e)
    (let [target-id (.-id (.-target e))
          [_ field raw-index] (re-find #"(.*)-(\d)" target-id)
          index (parse-int raw-index)
          [to-field to-index] (case (key-code e)
                                :up    [field (mod (dec index) item-count)]
                                :down  [field (mod (inc index) item-count)]
                                :left  [(prev-item-nav-field field) index]
                                :right [(next-item-nav-field field) index]
                                nil)]
      (when to-field
        (.preventDefault e)
        (set-focus (str to-field "-" to-index))))))

(defn- item-input-row
  [item index item-count page-state]
  ^{:key (str "item-form-" index)}
  [:tr
   [:td [forms/typeahead-input
         item
         [:account-id]
         {:search-fn (fn [input callback]
                       (callback (find-by-path input @accounts)))
          :on-change #(ensure-entry-state page-state)
          :caption-fn #(string/join "/" (:path %))
          :value-fn :id
          :find-fn (fn [id callback]
                     (callback (@accounts-by-id id)))
          :html {:id (str "account-id-" index)
                 :on-key-up #(item-navigate % item-count)}}]]
   [:td [forms/text-input item [:memo] {:on-change #(ensure-entry-state page-state)
                                        :html {:on-key-up #(item-navigate % item-count)
                                               :id (str "memo-" index)}}]]
   [:td [forms/decimal-input item [:credit-quantity] {:on-accept #(ensure-entry-state page-state)
                                                      :html {:on-key-up #(item-navigate % item-count)
                                                             :on-key-down #(when (arrow-key? %) (.preventDefault %))
                                                             :id (str "credit-quantity-" index)}}]]
   [:td [forms/decimal-input item [:debit-quantity] {:on-accept #(ensure-entry-state page-state)
                                                     :html {:id (str "debit-quantity-" index)
                                                            :on-key-up #(item-navigate % item-count)
                                                            :on-key-down #(when (arrow-key? %) (.preventDefault %))}}]]])

(defn full-transaction-form
  [page-state]
  (let [transaction (r/cursor page-state [:transaction])
        total-credits (make-reaction #(->> (:items @transaction)
                                           (map :credit-quantity)
                                           (reduce decimal/+ 0M)))
        total-debits (make-reaction #(->> (:items @transaction)
                                          (map :debit-quantity)
                                          (reduce decimal/+ 0M)))
        correction (make-reaction #(decimal/abs (- @total-debits @total-credits)))
        item-count (make-reaction #(count (:items @transaction)))]
    (fn []
      [:div {:class (when-not (mode? @transaction ::full) "d-none")}
       [:div.alert.alert-warning.d-md-none "Please use a larger screen to edit the transaction in this mode."]
       [:form.d-none.d-md-block
        [forms/date-field transaction [:transaction-date] {:validate [:required]}]
        [forms/text-field transaction [:description] {:validate [:required]}]
        [:table.table
         [:thead
          [:tr
           [:td "Account"]
           [:td "Memo"]
           [:td "Credit Amount"]
           [:td "Debit Amount"]]]
         [:tbody
          (doall (for [index (range @item-count)]
                   (item-input-row (r/cursor page-state [:transaction :items index])
                                   index
                                   @item-count
                                   page-state)))]
         [:tfoot
          [:tr
           [:td {:col-span 2} (special-char :nbsp)]
           [:td.text-end (format-decimal @total-credits)]
           [:td.text-end.d-flex.flex-row-reverse.justify-content-between
            (format-decimal @total-debits)
            (when-not (decimal/zero? @correction)
              [:span.text-danger.me-3
               (special-char :pm)
               (format-decimal @correction)])]]]]]])))

(defn simple-transaction-form
  [page-state]
  (let [transaction (r/cursor page-state [:transaction])]
    (fn []
      [:form {:class (when-not (mode? @transaction ::simple) "d-none")}
       [forms/date-field transaction [:transaction-date] {:validate [:required]}]
       [forms/text-field transaction [:description] {:validate [:required]}]
       [forms/decimal-field transaction [:quantity] {:validate [:required]}]
       [forms/typeahead-field
        transaction
        [:other-account-id]
        {:search-fn (fn [input callback]
                      (callback (find-by-path input @accounts)))
         :caption-fn #(string/join "/" (:path %))
         :value-fn :id
         :find-fn (fn [id callback]
                    (callback (@accounts-by-id id)))}]])))

(defn dividend-transaction-form
  [page-state]
  (let [transaction (r/cursor page-state [:transaction])
        shares (r/cursor transaction [:shares])
        quantity (r/cursor transaction [:quantity])
        price (make-reaction #(when (and @shares @quantity)
                                (decimal// @quantity @shares)))
        commodities (r/cursor page-state [:commodities])]
    (fn []
      [:form {:class (when-not (mode? @transaction ::dividend) "d-none")}
       [forms/date-field transaction [:transaction-date] {:validate [:required]}]
       [:div.row
        [:div.col-md-4
       [forms/decimal-field transaction [:quantity] {:validate [:required]
                                                     :caption "Dividend"}]]
        [:div.col-md-4
       [forms/decimal-field transaction [:shares] {:validate [:required]}]]
        [:div.col-md-4.d-flex.flex-column
         [:span.mb-2 "Est. Price"]
         [:span.mb-3.ms-2 (when @price (format-decimal @price))]]]
       [forms/typeahead-field
        transaction
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
         :caption-fn #(str (:name %) " (" (:symbol %) ")")
         :value-fn :id
         :find-fn (fn [id callback]
                    (callback (get-in @commodities [id])))}]])))

(defn trade-transaction-form
  [page-state]
  (let [transaction (r/cursor page-state [:transaction])
        price (make-reaction #(when (and (:shares @transaction)
                                         (:value @transaction))
                                (decimal// (:value @transaction)
                                           (:shares @transaction))))
        commodities (r/cursor page-state [:commodities])]
    (fn []
      [:form {:class (when-not (mode? @transaction ::trade) "d-none")}
       [forms/date-field transaction [:trade-date] {:validate [:required]}]
       [forms/select-field transaction [:action] (map (juxt name humanize) [:buy :sell]) {}]
       [:div.row
        [:div.col-md-4
         [forms/decimal-field transaction [:shares] {:validate [:required]}]]
        [:div.col-md-4
         [forms/decimal-field transaction [:value] {:validate [:required]}]]
        [:div.col-md-4.d-flex.flex-column
         [:span.mb-2 "Est. Price"]
         [:span.mb-3.ms-3 (when @price (format-decimal @price))]]]
       [forms/typeahead-field
        transaction
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
         :caption-fn #(str (:name %) " (" (:symbol %) ")")
         :value-fn :id
         :find-fn (fn [id callback]
                    (callback (get-in @commodities [id])))}]])))

(defn transformations
  [account commodities]
  {::simple #(simplify % account)
   ::full entryfy
   ::trade #(tradify % {:find-account @accounts-by-id
                        :find-commodity commodities})
   ::dividend #(simplify % account)})

(defn untransformations []
  {::simple fullify-trx
   ::full unentryfy
   ::trade #(untradify % {:find-account-by-commodity-id (->> @accounts
                                                             (map (juxt :commodity-id identity)) ; TODO: This will cause errors unless we lookup by parent also
                                                             (into {}))})
   ::dividend fullify-trx})

(defmulti save-transaction
  (fn [page-state _xf]
    (-> @page-state :transaction mode)))

(defmethod save-transaction :default
  [page-state xf]
  (let [{:keys [transaction]} @page-state
        mode (mode transaction)
        prepare ((untransformations) mode)]
    (-> transaction
        prepare
        (transactions/save xf))))

(defmethod save-transaction ::trade
  [page-state xf]
  (trading/create (:transaction @page-state)
                  (map xf)))

(defn- assoc-reinvest-desc
  [transaction commodity]
  (assoc transaction
         :description
         (gstr/format "Reinvest %.2f into %.3f shares of %s (%s)"
                      (:value transaction)
                      (:shares transaction)
                      (:name commodity)
                      (:symbol commodity))))

(defn- reinvest-dividend
  [page-state]
  (fn [xf]
    (completing
      (fn [ch created]
        (let [{:keys [transaction commodities]} @page-state
              commodity (get-in commodities [(:commodity-id transaction)])]
          (-> transaction
              (rename-keys {:quantity :value
                            :transaction-date :trade-date})
              (assoc-reinvest-desc commodity)
              (assoc :action :buy)
              (trading/create #(map (xf ch {:transaction created
                                            :trade %})))))))))

(defmethod save-transaction ::dividend
  [page-state xf]
  (let [{:keys [transaction commodities]} @page-state
        commodity (get-in commodities [(:commodity-id transaction)])
        dividends-account (->> @accounts ; TODO: Need a better way to make sure we have this value
                               (filter #(= :income (:type %)))
                               (filter #(re-find #"(?i)dividend" (:name %)))
                               first)]
    (-> transaction
        (dissoc :commodity-id :shares)
        (assoc :description (gstr/format "%s (%s)"
                                         (:name commodity)
                                         (:symbol commodity))
               :other-account-id (:id dividends-account))
        fullify-trx
        (transactions/save (comp (reinvest-dividend page-state)
                                 xf)))))
