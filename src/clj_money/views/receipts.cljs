(ns clj-money.views.receipts
  (:require [clojure.string :as string]
            [cljs.pprint :refer [pprint]]
            [goog.string :as gstr]
            [cljs-time.core :as t]
            [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [dgknght.app-lib.web :refer [format-date
                                         format-decimal]]
            [dgknght.app-lib.dom :refer [set-focus]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [dgknght.app-lib.forms-validation :as v]
            [clj-money.cached-accounts :as cached-accts]
            [clj-money.util :as util]
            [clj-money.icons :refer [icon
                                     icon-with-text]]
            [clj-money.state :refer [app-state
                                     accounts
                                     accounts-by-id
                                     +busy
                                     -busy]]
            [clj-money.accounts :refer [find-by-path]]
            [clj-money.receipts :as receipts]
            [clj-money.api.transactions :as trn]
            [clj-money.api.attachments :as atts]
            [clj-money.views.attachments :as atts-view]
            [clj-money.views.recent-transactions :as recent-trx]))

(defn- new-receipt
  [page-state]
  (let [defaults (-> (get-in @page-state [:receipt])
                     (select-keys [:receipt/transaction-date
                                   :receipt/payment-account])
                     (update-in [:receipt/transaction-date] (fnil identity (t/today)))
                     (assoc :receipt/items [{}]))]
    (swap! page-state assoc :receipt defaults)
    (set-focus "transaction-date")))

(defn- ->receipt
  ([transaction]
   (->receipt transaction {}))
  ([{:transaction/keys [items] :as transaction} {:keys [for-reuse?]}]
   (let [{:keys [debit] [credit] :credit} (group-by :transaction-item/action items)
         retain (cond-> [:transaction/description]
                  (not for-reuse?) (concat [:id :transaction/transaction-date]))]
     (merge (select-keys transaction retain)
            {:account (:transaction-item/account credit)
             :items (mapv #(select-keys % [:account-id :quantity :memo])
                          debit)}))))

(defn- touched-account-ids
  [{:receipt/keys [payment-account items]}]
  (->> items
       (map :receipt-item/account)
       (cons payment-account)
       (filter identity)
       (map :id)
       distinct))

(defn- update-account-caches
  "Advances the transaction-date-range of every account touched by the
  receipt in the client-side accounts cache, so the Accounts view doesn't
  need a full refresh to see the new transaction."
  [receipt trx-date]
  (doseq [id (touched-account-ids receipt)]
    (when-let [account (@accounts-by-id id)]
      (cached-accts/push-transaction-date! account trx-date))))

(defn- save-transaction
  [page-state]
  (let [receipt (:receipt @page-state)]
    (-> receipt
        receipts/->transaction
        (trn/save
          :callback -busy
          :on-success (fn [trx]
                        (swap! page-state
                               update-in
                               [:transactions]
                               #(util/upsert-into trx
                                                  {:sort-key :transaction/transaction-date}
                                                  %))
                        (update-account-caches receipt (:receipt/transaction-date receipt))
                        (new-receipt page-state))))))

(defn- search-accounts []
  (fn [input callback]
    (callback (find-by-path input @accounts))))

(defn- search-transactions
  [transactions]
  (fn [input callback]
    (let [term (string/lower-case input)]
      (->> transactions
           (filter #(-> %
                        (get-in [:transaction/description])
                        string/lower-case
                        (string/includes? term)))
           callback))))

(defn- ensure-blank-item
  [page-state]
  (let [{{:receipt/keys [items]} :receipt} @page-state]
    (when-not (some empty? items)
      (swap! page-state update-in [:receipt :receipt/items] conj {}))))

(defn- receipt-item-row
  [index receipt page-state]
  ^{:key (str "receipt-item-" index)}
  [:tr
   [:td [forms/typeahead-input
         receipt
         [:receipt/items index :receipt-item/account]
         {:search-fn (search-accounts)
          :find-fn (fn [account callback]
                     (callback (@accounts-by-id (:id account))))
          :on-change #(ensure-blank-item page-state)
          :caption-fn #(string/join "/" (:account/path %))}]]
   [:td [forms/decimal-input
         receipt
         [:receipt/items index :receipt-item/quantity]
         {:fraction-digits 2
          :on-accept #(ensure-blank-item page-state)}]]
   [:td [forms/text-input
         receipt
         [:receipt/items index :receipt-item/memo]
         {:on-change #(ensure-blank-item page-state)}]]])

(defn- reuse-trans
  [state transaction]
  ; The on-change will return the selected item when an item is selected
  ; and will return the simple text value if no item is selected
  (if (map? transaction)
    (-> state
        (dissoc :transaction-search)
        (update-in [:receipt] merge (->receipt transaction {:for-reuse? true})))
    state))

(defn- format-existing-trx
  [{:transaction/keys [transaction-date description value]}]
  (gstr/format "%s $%s %s"
               (format-date transaction-date)
               (format-decimal value)
               description))

(defn- receipt-form
  [page-state]
  (let [receipt (r/cursor page-state [:receipt])
        item-count (make-reaction #(count (:receipt/items @receipt)))
        transactions (r/cursor page-state [:transactions])
        total (make-reaction #(receipts/total @receipt))]
    (fn []
      [:form {:no-validate true
              :on-submit (fn [e]
                           (.preventDefault e)
                           (v/validate receipt)
                           (when (v/valid? receipt)
                             (save-transaction page-state)))}
       [forms/date-field receipt [:receipt/transaction-date] {:validations #{::v/required}}]
       [forms/typeahead-field
        receipt
        [:receipt/description]
        {:mode :direct
         :validations #{::v/required}
         :caption "Description"
         :search-fn (search-transactions @transactions)
         :find-fn (constantly nil)
         :caption-fn :description
         :list-caption-fn format-existing-trx
         :on-change #(swap! page-state reuse-trans %)
         :value-fn :description}]
       [forms/typeahead-field
        receipt
        [:receipt/payment-account]
        {:validations #{::v/required}
         :caption "Payment Method"
         :search-fn (search-accounts)
         :find-fn (fn [account callback]
                    (callback (@accounts-by-id (:id account))))
         :caption-fn #(string/join "/" (:account/path %))}]
       [forms/text-field receipt [:receipt/payment-memo] {:caption "Payment Memo"}]
       [:table.table.table-borderless
        [:thead
         [:tr
          [:th "Category"]
          [:th "Amount"]
          [:th "Memo"]]]
        [:tbody
         (->> (range @item-count)
              (map #(receipt-item-row % receipt page-state))
              doall)]
        [:tfoot
         [:tr
          [:td.text-end {:col-span 2}
           (format-decimal @total)]]]]
       [:div.mb-2
        [:button.btn.btn-primary
         {:type :submit
          :title "Click here to create this transaction."}
         (icon-with-text :check "Enter")]
        [:button.btn.btn-secondary.ms-2
         {:type :button
          :title "Click here to discard this receipt."
          :on-click (fn [_]
                      (swap! receipt select-keys [:receipt/transaction-date])
                      (set-focus "transaction-date"))}
         (icon-with-text :x "Cancel")]]])))

(defn- load-attachments
  [page-state]
  (let [{:keys [attachments-item]} @page-state]
    (+busy)
    (atts/select {:attachment/transaction attachments-item}
                 :callback -busy
                 :on-success #(swap! page-state assoc :attachments %))))

(defn- post-result-row-drop
  [page-state trx]
  (fn [_created]
    (swap! page-state
           (fn [state]
             (-> state
                 (update-in [:result-row-styles] dissoc (:id trx))
                 (update-in [:transactions]
                            (fn [transactions]
                              (map (fn [t]
                                     (if (util/id= trx t)
                                       (update-in t [:transaction/attachment-count] (fnil inc 0))
                                       t))
                                   transactions))))))))

(defn- pending-attachment-form
  [page-state]
  [atts-view/pending-attachment-form page-state
   :on-save-success (fn [{:keys [trx]} created]
                       ((post-result-row-drop page-state trx) created))])

(defn- result-row
  [{:keys [id] :transaction/keys [transaction-date description value attachment-count] :as trx} page-state]
  ^{:key (str "result-row-" id)}
  [:tr.align-middle
   (atts-view/drop-handlers page-state :result-row-styles id
                            {:trx trx
                             :attachment #:attachment{:transaction trx
                                                      :caption ""}})
   [:td (format-date transaction-date)]
   [:td description]
   [:td.text-end (format-decimal value)]
   [:td
    [:div.btn-group
     [:button.btn.btn-sm.btn-secondary
      {:title "Click here to edit this transaction."
       :on-click #(swap! page-state assoc :receipt (receipts/<-transaction trx))}
      (icon :pencil :size :small)]
     [:button.btn.btn-sm.btn-secondary
      {:title "Click here to view attachments for this transaction"
       :on-click (fn []
                   (swap! page-state assoc :attachments-item trx)
                   (load-attachments page-state))}
      (if ((some-fn nil? zero?) attachment-count)
        (icon :paperclip :size :small)
        [:span.badge.bg-info.text-dark attachment-count])]]]])

(defn- results-table
  [page-state]
  (let [transactions (r/cursor page-state [:transactions])
        settings (r/cursor page-state [:recent-settings])
        visible (make-reaction #(when @transactions
                                  (recent-trx/sort-and-limit @transactions @settings)))]
    (fn []
      [:<>
       [pending-attachment-form page-state]
       [:div.mb-2
        [forms/date-field
         page-state
         [:filter-date]
         {:caption "Entered Since"}]]
       [recent-trx/controls page-state [:recent-settings]]
       [:table.table.table-hover
        [:thead
         [:tr
          [:th "Date"]
          [:th "Description"]
          [:th.text-end "Amount"]
          [:th (html/space)]]]
        [:tbody
         (cond
           (seq @visible)
           (->> @visible
                (map #(result-row % page-state))
                doall)

           @transactions
           [:tr
            [:td {:col-span 4} "No transactions entered on this date"]]

           :else
           [:tr
            [:td {:col-span 4} (bs/spinner)]])]]])))

(defn- load-transactions
  [page-state]
  (+busy)
  (trn/select {:include-items true
               :transaction/created-at [:>= (:filter-date @page-state)]}
              :callback -busy
              :on-success #(swap! page-state
                                  assoc
                                  :transactions %)))

(defn- index []
  (let [page-state (r/atom {:filter-date (t/today)
                            :recent-settings recent-trx/default-settings})
        attachments-item (r/cursor page-state [:attachments-item])]
    (new-receipt page-state)
    (load-transactions page-state)
    (add-watch page-state ::filter-date
               (fn [_ _ old new]
                 (when (not= (:filter-date old) (:filter-date new))
                   (load-transactions page-state))))
    (fn []
      [:<>
       [:h1.mt-3 "Receipt Entry"]
       [:div.row
        [:div.col-md-6
         [receipt-form page-state]]
        [:div.col-md-6
         (if @attachments-item
           [:<>
            [atts-view/attachments-card page-state]
            [atts-view/attachment-form page-state]]
           [results-table page-state])]]])))

(secretary/defroute "/receipts" []
  (swap! app-state assoc :page #'index :active-nav :receipts))
