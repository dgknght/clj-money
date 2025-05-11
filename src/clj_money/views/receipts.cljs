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
            [clj-money.api.transactions :as trn]))

(defn- new-receipt
  [page-state]
  (let [defaults (-> (get-in @page-state [:receipt])
                     (select-keys [:receipt/transaction-date
                                   :receipt/account])
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

(defn- save-transaction
  [page-state]
  (let [{:keys [receipt]} @page-state
        trx (receipts/->transaction receipt)]
    (trn/save trx
              :callback -busy
              :on-success (fn [trx]
                            (swap! page-state
                                 update-in
                                 [:receipts] ; TODO: rename this to :transactions?
                                 #(util/upsert-into trx
                                                    {:sort-key :transaction/transaction-date}
                                                    %))
                            (new-receipt page-state)))))

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
         [:receipt/items index :receipt-item/account :id]
         {:search-fn (search-accounts)
          :find-fn (fn [id callback]
                     (callback (@accounts-by-id id)))
          :on-change #(ensure-blank-item page-state)
          :caption-fn #(string/join "/" (:account/path %))
          :value-fn :id}]]
   [:td [forms/decimal-input
         receipt
         [:receipt/items index :receipt-item/quantity]
         {:on-accept #(ensure-blank-item page-state)}]]
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
        [:receipt/payment-account :id]
        {:validations #{::v/required}
         :caption "Payment Method"
         :search-fn (search-accounts)
         :find-fn (fn [id callback]
                    (callback (@accounts-by-id id)))
         :caption-fn #(string/join "/" (:account/path %))
         :value-fn :id}]
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

(defn- result-row
  [{:transaction/keys [transaction-date description value] :as trx} page-state]
  ^{:key (str "result-row-" (:id trx))}
  [:tr
   [:td (format-date transaction-date)]
   [:td description]
   [:td.text-end (format-decimal value)]
   [:td
    [:button.btn.btn-sm.btn-secondary
     {:title "Click here to edit this transaction."
      :on-click #(swap! page-state assoc :receipt (receipts/<-transaction trx))}
     (icon :pencil :size :small)]]])

(defn- results-table
  [page-state]
  (let [transactions (r/cursor page-state [:receipts])]
    (fn []
      [:table.table.table-hover
       [:thead
        [:tr
         [:th "Date"]
         [:th "Description"]
         [:th.text-end "Amount"]
         [:th (html/space)]]]
       [:tbody
        (cond
          (seq @transactions)
          (->> @transactions
               (map #(result-row % page-state))
               doall)

          @transactions
          [:tr
           [:td {:col-span 4} "No recent transactions"]]

          :else
          [:tr
           [:td {:col-span 4} (bs/spinner)]])]])))

(defn- recent? []
  (let [cutoff (-> 24 t/hours t/ago)]
    (fn [{:transaction/keys [created-at]}]
      (t/before? cutoff created-at))))

(defn- load-transactions
  [page-state]
  (+busy)
  (trn/search {:include-items true}
              :callback -busy
              :on-success #(swap! page-state
                                  assoc
                                  :transactions %
                                  :receipts (filter (recent?) %))))

(defn- index []
  (let [page-state (r/atom {})]
    (new-receipt page-state)
    (load-transactions page-state)
    (fn []
      [:<>
       [:h1.mt-3 "Receipt Entry"]
       [:div.row
        [:div.col-md-6
         [receipt-form page-state]]
        [:div.col-md-6
         [results-table page-state]]]])))

(secretary/defroute "/receipts" []
  (swap! app-state assoc :page #'index))
