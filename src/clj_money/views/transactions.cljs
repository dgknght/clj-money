(ns clj-money.views.transactions
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [clojure.zip :as zip]
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
            [dgknght.app-lib.forms-validation :as v]
            [clj-money.icons :refer [icon]]
            [clj-money.state :refer [accounts
                                     accounts-by-id
                                     +busy
                                     -busy]]
            [clj-money.dnd :as dnd]
            [clj-money.util :as util :refer [debounce model=]]
            [clj-money.dates :as dates]
            [clj-money.commodities :as cmdts]
            [clj-money.accounts :as accounts :refer [polarize-quantity
                                                     find-by-path
                                                     format-quantity]]
            [clj-money.transactions :refer [accountify
                                            unaccountify
                                            can-accountify?
                                            entryfy
                                            unentryfy
                                            ensure-empty-item]]
            [clj-money.components :refer [load-in-chunks]]
            [clj-money.api.transaction-items :as transaction-items]
            [clj-money.api.transactions :as transactions]
            [clj-money.api.attachments :as atts]
            [clj-money.api.trading :as trading]))

(defn- fullify-trx
  [trx]
  (unaccountify trx (comp @accounts-by-id
                          :id)))

(defn- prepare-transaction-for-edit
  [transaction account]
  (if (can-accountify? transaction)
    (accountify transaction account)
    (entryfy transaction)))

(defn- item->tkey
  [{:transaction-item/keys [transaction-date transaction]}]
  {:transaction/transaction-date transaction-date
   :id (:id transaction)})

(defn- edit-transaction
  [item page-state]
  (+busy)
  (transactions/get
    (item->tkey item)
    :callback -busy
    :on-success (fn [result]
                  (let [prepared (prepare-transaction-for-edit
                                   result
                                   (:view-account @page-state))]
                    (swap! page-state assoc :transaction prepared))
                  (set-focus "transaction-date"))))

(defn load-unreconciled-items
  [page-state]
  (+busy)
  (let [account (:view-account @page-state)
        criteria {:transaction-item/account account
                  :transaction/transaction-date (apply vector
                                                            :between
                                                            (:account/transaction-date-range account))
                  :unreconciled true
                  :include-children (:include-children? @page-state)}]
    (transaction-items/select
      criteria
      :callback -busy
      :on-success #(swap! page-state assoc :items %))))

(defn- load-attachments
  [page-state]
  (let [{{:transaction-item/keys [transaction]} :attachment-item} @page-state
        criteria {:attachment/transaction transaction}]
    (+busy)
    (atts/select criteria
                 :callback -busy
                 :on-success #(swap! page-state assoc :attachments %))))

(defn- fetch-items
  [xf]
  (completing
    (fn [ch criteria]
      (if criteria
        (transaction-items/select criteria
                                  :on-success #(xf ch %))
        (xf ch [])))))

(defn init-item-loading
  [page-state]
  (let [account (get-in @page-state [:view-account])
        [first-date last-date :as range] (:account/transaction-date-range account)]
    (if range
      (do (swap! page-state dissoc :items :all-items-fetched?)
          (let [{:keys [ctl-ch items-ch]} (->> (dates/desc-ranges first-date last-date (t/months 6))
                                               (load-in-chunks
                                                 {:fetch-xf (comp
                                                              (map (fn [[start end :as range]]
                                                                     (when (seq range)
                                                                       {:transaction-item/account (util/->model-ref account)
                                                                        :transaction/transaction-date [:between> start end]})))
                                                              fetch-items)
                                                  :chunk-size 100}))]
            (go-loop [items (<! items-ch)]
                     (if items
                       (do
                         (swap! page-state update-in [:items] (fnil concat []) items)
                         (recur (<! items-ch)))
                       (swap! page-state assoc :all-items-fetched? true)))

            (swap! page-state assoc :ctl-chan ctl-ch)
            (go (>! ctl-ch :fetch))))
      (swap! page-state assoc :items []))))

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
    (+busy)
    (transactions/delete (item->tkey item)
                         :callback -busy
                         :on-success #(reset-item-loading page-state))))

(defn- post-item-row-drop
  [page-state item]
  (fn [{:keys [body]}]
    (swap! page-state
           (fn [state]
             (-> state
                 (update-in [:item-row-styles]
                            dissoc
                            (:id item))
                 (update-in [:items] (fn [items]
                                       (map (fn [item]
                                              (if (util/id= (:attachment/transaction body)
                                                            (:attachment/transaction item))
                                                (update-in item [:transaction-item/attachment-count] inc)
                                                item))
                                            items)))))))
  (notify/toast "Success" "The attachment was saved successfully."))

(defn- handle-item-row-drop
  [item e page-state]
  (.preventDefault e)
  (+busy)
  (atts/create {:transaction-id (:transaction-id item) ; TODO: use transaction-ref to combine these?
                :transaction-date (:transaction-date item)
                :file (first (dnd/data-files e))}
               :callback -busy
               :on-success (post-item-row-drop page-state item)))

(defn- item-row
  [{:keys [account
           reconciliation
           styles]}
   page-state]
  (fn [{:transaction-item/keys [attachment-count
                                quantity
                                balance
                                action
                                reconciliation-status]
        :transaction/keys [description
                           transaction-date]
        :as item}]
    ^{:key (str "item-row-" (:id item))}
    [:tr.align-middle
     {:on-drag-enter #(swap! page-state
                             assoc-in
                             [:item-row-styles (:id item)]
                             {:background-color "var(--primary)"
                              :color "var(--white)"
                              :cursor :copy})
      :on-drag-leave (debounce
                       100
                       #(swap! page-state
                               update-in
                               [:item-row-styles]
                               dissoc
                               (:id item)))
      :on-drag-over #(.preventDefault %)
      :on-drop #(handle-item-row-drop item % page-state)
      :style (get-in styles [(:id item)])}
     [:td.text-end
      [:span.d-md-none (format-date transaction-date "M/d")]
      [:span.d-none.d-md-inline (format-date transaction-date)]]
     [:td {:style (get-in styles [(:id item)])} description]
     [:td.text-end (format-quantity (polarize-quantity quantity
                                                       action
                                                       account)
                                    account)]
     [:td.text-center.d-none.d-md-table-cell
      (if @reconciliation
        [forms/checkbox-input
         reconciliation
         [:clj-money.views.reconciliations/item-selection (:id item)]
         {::forms/decoration ::forms/none}]
        (icon
          (case reconciliation-status
            :completed :check-square
            :new       :dash-sqaure
            :square)
          :size :small))]
     (when-not reconciliation
       [:td.text-end.d-none.d-md-table-cell (format-quantity balance
                                                             account)])
     (when-not @reconciliation
       [:td
        [:div.btn-group
         [:button.btn.btn-secondary.btn-sm
          {:on-click #(edit-transaction item page-state)
           :title "Click here to edit this transaction."}
          (icon :pencil :size :small)]
         [:button.btn.btn-secondary.btn-sm.d-none.d-md-block
          {:on-click (fn []
                       (swap! page-state
                              assoc
                              :attachments-item
                              item)
                       (load-attachments page-state))
           :title "Click here to view attachments for this transaction"}
          (if ((some-fn nil? zero?) attachment-count)
            (icon :paperclip :size :small)
            [:span.badge.bg-secondary attachment-count])]
         [:button.btn.btn-danger.btn-sm
          {:on-click #(delete-transaction item page-state)
           :title "Click here to remove this transaction."}
          (icon :x-circle :size :small)]]])]))

(defn items-table
  [page-state]
  (let [raw-items (r/cursor page-state [:items])
        items (make-reaction (fn []
                               (map (fn [{:transaction-item/keys [reconciliation]
                                          :keys [id]
                                          :as item}]
                                      (assoc item
                                             :transaction-item/reconciliation-status
                                             (cond
                                               (nil? id) :new
                                               reconciliation :completed
                                               :else :unreconciled)))
                                    @raw-items)))
        styles (r/cursor page-state [:item-row-styles])
        include-children? (r/cursor page-state [:include-children?])
        account (r/cursor page-state [:view-account])
        recon (r/cursor page-state [:reconciliation])
        filter-fn (make-reaction (fn []
                                   (if @include-children?
                                     identity
                                     #(model= @account
                                              (:transaction-item/account %)))))]
    (fn []
      [:table.table.table-striped.table-hover
       [:thead
        [:tr
         [:th.text-end "Date"]
         [:th "Description"]
         [:th.text-end "Amount"]
         [:th.text-center.d-none.d-md-table-cell "Rec."]
         (when-not @recon
           [:th.text-end.d-none.d-md-table-cell "Balance"])
         (when-not @recon
           [:th (space)])]]
       [:tbody
        (cond
          (seq @items)
          (->> @items
               (filter @filter-fn)
               (map (item-row {:account @account
                               :reconciliation recon
                               :styles styles}
                              page-state))
               doall)

          @items
          [:tr [:td.text-center {:col-span 6} "No transactions"]]

          :else
          [:tr [:td.text-center {:col-span 6} (bs/spinner {:size :small})]])]])))

(defn fund-transactions-table
  [page-state]
  (let [items (r/cursor page-state [:items])
        account  (r/cursor page-state [:view-account])]
    ; I don't think we need to chunk this, but maybe we do
    (transaction-items/select (accounts/->criteria @account)
                              :on-success #(swap! page-state assoc :items %))
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
        (doall (for [{:as item
                      :transaction-item/keys [transaction-date
                                              quantity
                                              balance
                                              value
                                              action]}
                     (sort-by :index > @items)]
                 ^{:key (str "item-" (:id item))}
                 [:tr
                  [:td.text-end (format-date transaction-date)]
                  [:td (:description item)]
                  [:td.text-end (format-decimal (polarize-quantity quantity
                                                                   action
                                                                   @account)
                                                4)]
                  [:td.text-end (format-decimal balance, 4)]
                  [:td.text-end (currency-format value)]]))]])))

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
         [:transaction-item/account]
         {:search-fn (fn [input callback]
                       (callback (find-by-path input @accounts)))
          :on-change #(ensure-entry-state page-state)
          :caption-fn #(string/join "/" (:account/path %))
          :find-fn (fn [{:keys [id]} callback]
                     (callback (@accounts-by-id id)))
          :html {:id (str "account-id-" index)
                 :on-key-up #(item-navigate % item-count)}}]]
   [:td [forms/text-input
         item
         [:transaction-item/memo]
         {:on-change #(ensure-entry-state page-state)
          :html {:on-key-up #(item-navigate % item-count)
                 :id (str "memo-" index)}}]]
   [:td [forms/decimal-input
         item
         [:transaction-item/credit-quantity]
         {:on-accept #(ensure-entry-state page-state)
          :html {:on-key-up #(item-navigate % item-count)
                 :on-key-down #(when (arrow-key? %) (.preventDefault %))
                 :id (str "credit-quantity-" index)}}]]
   [:td [forms/decimal-input
         item
         [:transaction-item/debit-quantity]
         {:on-accept #(ensure-entry-state page-state)
          :html {:id (str "debit-quantity-" index)
                 :on-key-up #(item-navigate % item-count)
                 :on-key-down #(when (arrow-key? %) (.preventDefault %))}}]]])

(defn full-transaction-form
  [page-state & {:keys [on-save]}]
  (let [transaction (r/cursor page-state [:transaction])
        total-credits (make-reaction #(->> (:transaction/items @transaction)
                                           (map :transaction-item/credit-quantity)
                                           (reduce decimal/+ 0M)))
        total-debits (make-reaction #(->> (:transaction/items @transaction)
                                          (map :transaction-item/debit-quantity)
                                          (reduce decimal/+ 0M)))
        correction (make-reaction #(decimal/abs (- @total-debits @total-credits)))
        item-count (make-reaction #(count (:transaction/items @transaction)))]
    (fn []
      [:div
       [:div.alert.alert-warning.d-md-none "Please use a larger screen to use full transaction mode."]
       [:form#transaction-form.d-none.d-md-block
        {:no-validate true
         :on-submit (fn [e]
                      (.preventDefault e)
                      (v/validate transaction)
                      (when (v/valid? transaction)
                       (+busy)
                       (transactions/save (unentryfy @transaction)
                                          :callback -busy
                                          :on-success on-save)))}
        [forms/date-field transaction [:transaction/transaction-date] {:validations #{::v/required}}]
        [forms/text-field transaction [:transaction/description] {:validations #{::v/required}}]
        [:table.table
         [:thead
          [:tr
           [:td "Account"]
           [:td "Memo"]
           [:td "Credit Amount"]
           [:td "Debit Amount"]]]
         [:tbody
          (doall (for [index (range @item-count)]
                   (item-input-row (r/cursor page-state [:transaction :transaction/items index])
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
  [page-state & {:keys [on-save]}]
  (let [transaction (r/cursor page-state [:transaction])]
    (fn []
      [:form#transaction-form
       {:no-validate true
        :on-submit (fn [e]
                     (.preventDefault e)
                     (v/validate transaction)
                     (when (v/valid? transaction)
                       (+busy)
                       (transactions/save (fullify-trx @transaction)
                                          :callback -busy
                                          :on-success on-save)))}
       [forms/date-field
        transaction
        [:transaction/transaction-date]
        {:validations #{::v/required}}]
       [forms/text-field transaction [:transaction/description] {:validations #{::v/required}}]
       [forms/decimal-field transaction [:transaction/quantity] {:validations #{::v/required}}]
       [forms/typeahead-field
        transaction
        [:transaction/other-account]
        {:search-fn (fn [input callback]
                      (callback (find-by-path input @accounts)))
         :caption-fn #(string/join "/" (:account/path %))
         :find-fn (fn [{:keys [id]} callback]
                    (callback (@accounts-by-id id)))}]])))

(defn trade-transaction-form
  [page-state & {:keys [on-save]}]
  (let [trade (r/cursor page-state [:trade])
        dividend? (r/cursor trade [:trade/dividend?])
        price (make-reaction #(when (and (:trade/shares @trade)
                                         (:trade/value @trade))
                                (decimal// (:trade/value @trade)
                                           (:trade/shares @trade))))
        commodities (r/cursor page-state [:commodities])]
    (fn []
      [:form#trade-form
       {:no-validate true
        :on-submit (fn [e]
                     (.preventDefault e)
                     (v/validate trade)
                     (when (v/valid? trade)
                       (+busy)
                       (trading/create @trade
                                       :callback -busy
                                       :on-success on-save)))}
       [forms/date-field trade [:trade/date] {:validations #{::v/required}}]
       (when-not @dividend?
         [forms/select-field
          trade
          [:trade/action]
          (map (juxt name humanize)
               [:buy :sell])
          {:transform-fn keyword
           :validations #{::v/required}}])
       [:div.row
        [:div.col-md-4
         [forms/decimal-field trade [:trade/shares] {:validations #{::v/required}}]]
        [:div.col-md-4
         [forms/decimal-field trade [:trade/value] {:validations #{::v/required}}]]
        [:div.col-md-4.d-flex.flex-column
         [:span.mb-2 "Est. Price"]
         [:span.mb-3.ms-3 (when @price (format-decimal @price))]]]
       [forms/typeahead-field
        trade
        [:trade/commodity]
        {:search-fn (fn [input callback]
                      (callback (cmdts/search input (vals @commodities))))
         :caption-fn cmdts/description 
         :value-fn :id
         :find-fn (fn [{:keys [id]} callback]
                    (callback (@commodities id)))
         :validations #{::v/required}}]
       (when @dividend?
         [forms/typeahead-field
          trade
          [:trade/dividend-account]
          {:search-fn (fn [input callback]
                        (->> @accounts
                             (find-by-path input)
                             callback))
           :caption-fn (comp (partial string/join "/") :account/path)
           :find-fn (fn [{:keys [id]} callback]
                      (callback (@accounts-by-id id)))
           :validations #{::v/required}}])])))
