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
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.forms-validation :as v]
            [clj-money.cached-accounts :as cached-accts]
            [clj-money.util :as util]
            [clj-money.icons :refer [icon
                                     icon-with-text]]
            [clj-money.state :refer [app-state
                                     current-entity
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

(defn ->reused-fields
  "Extracts the payment account and line items from an existing transaction
  so a receipt can be pre-populated with them. The description and
  transaction date are left alone."
  [{:transaction/keys [items]}]
  (let [{:keys [debit] [credit] :credit} (group-by :transaction-item/action items)]
    {:receipt/payment-account (:transaction-item/account credit)
     :receipt/payment-memo (:transaction-item/memo credit)
     :receipt/items (mapv (fn [{:transaction-item/keys [account quantity memo]}]
                            #:receipt-item{:account account
                                           :quantity quantity
                                           :memo memo})
                          debit)}))

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
                               #(util/upsert-into (assoc trx :transaction/created-at (t/now))
                                                  {:sort-key :transaction/transaction-date}
                                                  %))
                        (update-account-caches receipt (:receipt/transaction-date receipt))
                        (new-receipt page-state))))))

(defn- search-accounts []
  (fn [input callback]
    (callback (find-by-path input @accounts))))

(def ^:private description-search-months
  "How far back to look for transactions to offer in the description typeahead."
  3)

(def ^:private max-transactions-to-consider
  "The transactions API defaults to returning the *search-result-limit*
  most recent matches; the Recent Transactions table and the description
  typeahead both re-sort/limit whatever comes back on the client, so they
  need a much larger ceiling than that default -- generous enough to never
  affect normal usage, but still a real backstop rather than an unbounded
  fetch."
  1000)

(defn- search-transactions
  [input callback transactions]
  (let [term (string/lower-case input)]
    (->> transactions
         (filter #(-> %
                      (get-in [:transaction/description])
                      string/lower-case
                      (string/includes? term)))
         callback)))

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
        (update-in [:receipt] merge (->reused-fields transaction))
        (update-in [:receipt :receipt/items]
                   (fn [items]
                     (if (some empty? items)
                       items
                       (conj (vec items) {})))))
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
        history (r/cursor page-state [:historical-transactions])
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
         :search-fn (fn [input callback]
                      (search-transactions input callback @history))
         :find-fn (constantly nil)
         :caption-fn :transaction/description
         :list-caption-fn format-existing-trx
         :on-change #(swap! page-state reuse-trans %)
         :value-fn :transaction/description}]
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

(def ^:private recent-options-id "receipts-recent-transactions-options")

(defn- results-table
  [page-state]
  [:<>
   [pending-attachment-form page-state]
   [recent-trx/table
    {:id recent-options-id
     :page-state page-state
     :items-path [:transactions]
     :row-fn #(result-row % page-state)
     :empty-message "No transactions entered on this date"}]])

(defn- load-transactions
  [page-state]
  (+busy)
  (trn/select {:include-items true
               :select-also ["created-at"]
               :limit max-transactions-to-consider
               :transaction/created-at [:>= (:filter-date @page-state)]}
              :callback -busy
              :on-success #(swap! page-state
                                  assoc
                                  :transactions %)))

(defn- load-historical-transactions
  "Loads the pool of transactions offered by the description typeahead,
  independent of the Recent Transactions table's 'Entered Since' filter."
  [page-state]
  (+busy)
  (trn/select {:include-items true
               :limit max-transactions-to-consider
               :transaction/transaction-date [:>= (t/minus (t/today)
                                                           (t/months description-search-months))]}
              :callback -busy
              :on-success #(swap! page-state
                                  assoc
                                  :historical-transactions
                                  %)))

(defn- index []
  (let [page-state (r/atom {:filter-date (t/today)
                            :recent-settings recent-trx/default-settings})
        attachments-item (r/cursor page-state [:attachments-item])]
    (new-receipt page-state)
    (load-transactions page-state)
    (load-historical-transactions page-state)
    (add-watch page-state ::filter-date
               (fn [_ _ old new]
                 (when (not= (:filter-date old) (:filter-date new))
                   (load-transactions page-state))))
    (add-watch current-entity
               ::index
               (fn [_ _ _ _]
                 (swap! page-state
                        #(-> %
                             (dissoc :attachments-item :attachments)
                             (assoc :transactions [])))
                 (new-receipt page-state)
                 (load-transactions page-state)
                 (load-historical-transactions page-state)))
    (fn []
      [:<>
       [:h1.mt-3 "Receipts"]
       [:div.row
        [:div.col-md-6
         [:h3 "New Transaction"]
         [receipt-form page-state]]
        [:div.col-md-6
         (if @attachments-item
           [:<>
            [atts-view/attachments-card page-state]
            [atts-view/attachment-form page-state]]
           [results-table page-state])]]])))

(secretary/defroute "/receipts" []
  (swap! app-state assoc :page #'index :active-nav :receipts))
