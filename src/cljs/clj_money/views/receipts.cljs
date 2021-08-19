(ns clj-money.views.receipts
  (:require [clojure.string :as string]
            [goog.string :as gstr]
            [cljs-time.core :as t]
            [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [dgknght.app-lib.busy :refer [busy +busy -busy]]
            [dgknght.app-lib.web :refer [format-date
                                         format-decimal]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.decimal :as decimal]
            [dgknght.app-lib.forms :as forms]
            [clj-money.views.util :refer [handle-error]]
            [clj-money.state :refer [app-state
                                     current-entity
                                     accounts
                                     accounts-by-id]]
            [clj-money.bootstrap :as bs]
            [clj-money.accounts :refer [find-by-path]]
            [clj-money.api.transactions :as trn]))

(defn- new-receipt
  [page-state]
  (let [transaction-date (get-in @page-state [:receipt :transaction-date] (t/today))]
    (swap! page-state assoc :receipt {:transaction-date transaction-date
                                      :items [{}]})
    (html/set-focus "transaction-date")))

(defn- ->transaction-item
  [item]
  (assoc item :action :debit))

(defn- ->transaction
  [receipt]
  (-> receipt
      (dissoc :account-id)
      (assoc :entity-id (:id @current-entity))
      (update-in [:items] (comp #(conj % {:action :credit
                                          :account-id (:account-id receipt)
                                          :quantity (->> (:items receipt)
                                                         (map :quantity)
                                                         decimal/sum)})
                                #(->> %
                                      (remove empty?)
                                      (map ->transaction-item))))))

(defn- ->receipt
  ([transaction]
   (->receipt transaction {}))
  ([{:keys [items] :as transaction} {:keys [for-reuse?]}]
   (let [{:keys [debit] [credit] :credit} (group-by :action items)
         retain (cond-> [:description]
                  (not for-reuse?) (concat [:id :transaction-date]))]
     (merge (select-keys transaction retain)
            {:account-id (:account-id credit)
             :items (mapv #(select-keys % [:account-id :quantity])
                          debit)}))))

(defn- create-transaction
  [receipt page-state]
  (+busy page-state)
  (trn/create (->transaction receipt)
              (fn [result]
                (swap! page-state #(-> %
                                       -busy
                                       (update-in
                                         [:receipts]
                                         (fnil conj '())
                                         result)))
                (new-receipt page-state))
              (handle-error page-state "Unable to create the transaction: %s")))

(defn- update-transaction
  [receipt page-state]
  (+busy page-state)
  (trn/update (->transaction receipt)
              (fn [result]
                (swap! page-state (fn [state] (-> state
                                                  -busy
                                                  (update-in
                                                    [:receipts]
                                                    (fn [receipts]
                                                      (map #(if (= (:id receipt)
                                                                   (:id %))
                                                              result
                                                              %)
                                                           receipts))))))
                (new-receipt page-state))
              (handle-error page-state "Unable to update the transaction: %s")))

(defn- save-transaction
  [page-state]
  (let [{:keys [receipt]} @page-state]
    (if (:id receipt)
      (update-transaction receipt page-state)
      (create-transaction receipt page-state))))

(defn- search-accounts []
  (fn [input callback]
      (callback (find-by-path input @accounts))))

(defn- search-transactions
  [transactions input callback]
  (let [term (string/lower-case input)]
    (->> transactions
         (filter #(-> %
                      (get-in [:description])
                      string/lower-case
                      (string/includes? term)))
         callback)))

(defn- ensure-blank-item
  [page-state]
  (let [{{:keys [items]} :receipt} @page-state]
    (when-not (some empty? items)
      (swap! page-state update-in [:receipt :items] conj {}))))

(defn- receipt-item-row
  [index receipt page-state]
  ^{:key (str "receipt-item-" index)}
  [:tr
   [:td [forms/typeahead-input
         receipt
         [:items index :account-id]
         {:validate [:required]
          :search-fn (search-accounts)
          :find-fn (fn [id callback]
                    (callback (@accounts-by-id id)))
          :on-change #(ensure-blank-item page-state)
          :caption-fn #(string/join "/" (:path %))
          :value-fn :id}]]
   [:td [forms/decimal-input
         receipt
         [:items index :quantity]
         {:validate [:required]
          :on-accept #(ensure-blank-item page-state)}]]])

(defn- reuse-trans
  [state transaction]
  (-> state
      (dissoc :transaction-search)
      (update-in [:receipt] merge (->receipt transaction {:for-reuse? true}))))

(defn- receipt-form
  [page-state]
  (let [receipt (r/cursor page-state [:receipt])
        item-count (make-reaction #(count (:items @receipt)))
        transactions (r/cursor page-state [:transactions])
        search (r/cursor page-state [:transaction-search])
        hide-description? (make-reaction #(boolean @search))
        hide-search-term? (make-reaction #(not @search))
        total (make-reaction #(->> (:items @receipt)
                                   (map :quantity)
                                   decimal/sum))
        busy? (busy page-state)]
    (fn []
      [:form {:no-validate true
              :on-submit (fn [e]
                           (.preventDefault e)
                           (save-transaction page-state))}
       [forms/date-field receipt [:transaction-date] {:validate [:required]}]
       [forms/text-field
        receipt
        [:description]
        {:validation [:required]
         :prepend [:button.btn.btn-secondary
                   {:title "Click here to find a previous transaction to re-use."
                    :on-click (fn [e]
                                (.preventDefault e)
                                (swap! page-state assoc :transaction-search {})
                                (js/setTimeout
                                  #(html/set-focus "search-term")
                                  250))}
                   (bs/icon :search)]
         :html {:auto-complete :off}
         :hide? hide-description?}]
       [forms/typeahead-field
        search
        [:search-term]
        {:validate [:required]
         :hide? hide-search-term?
         :caption "Description"
         :search-fn #(search-transactions @transactions %1 %2)
         :find-fn (constantly nil)
         :caption-fn :description
         :list-caption-fn #(gstr/format "%s $%s %s"
                                        (format-date (:transaction-date %))
                                        (format-decimal (:value %))
                                        (:description %))
         :on-change #(swap! page-state reuse-trans %)
         :value-fn :description
         :prepend [:button.btn.btn-secondary
                   {:title "Click here to cancel the search."
                    :on-click (fn [e]
                                (.preventDefault e)
                                (swap! page-state dissoc :transaction-search))}
                   (bs/icon :x)]}]
       [forms/typeahead-field
        receipt
        [:account-id]
        {:validate [:required]
         :caption "Payment Method"
         :search-fn (search-accounts)
         :find-fn (fn [id callback]
                    (callback (@accounts-by-id id)))
         :caption-fn #(string/join "/" (:path %))
         :value-fn :id}]
       [:table.table.table-borderless
        [:thead
         [:tr
          [:th "Category"]
          [:th "Amount"]]]
        [:tbody
         (->> (range @item-count)
              (map #(receipt-item-row % receipt page-state))
              doall)]
        [:tfoot
         [:tr
          [:td.text-right {:col-span 2}
           (format-decimal @total)]]]]
       [:div
        [:button.btn.btn-primary
         {:type :submit
          :disabled @busy?
          :title "Click here to create this transaction."}
         (bs/icon-with-text :check "Enter")]
        (html/space)
        [:button.btn.btn-secondary
         {:type :button
          :disabled @busy?
          :title "Click here to discard this receipt."
          :on-click (fn []
                      (swap! receipt select-keys [:transaction-date])
                      (html/set-focus "transaction-date"))}
         (bs/icon-with-text :x "Cancel")]]])))

(defn- result-row
  [transaction page-state]
  ^{:key (str "result-row-" (:id transaction))}
  [:tr
   [:td (format-date (:transaction-date transaction))]
   [:td (:description transaction)]
   [:td (format-decimal (:value transaction))]
   [:td
    [:button.btn.btn-sm.btn-info
     {:title "Click here to edit this transaction."
      :on-click #(swap! page-state assoc :receipt (->receipt transaction))}
     (bs/icon :pencil)]]])

(defn- results-table
  [page-state]
  (let [transactions (r/cursor page-state [:receipts])]
    (fn []
      [:table.table.table-hover
       [:thead
        [:tr
         [:th "Date"]
         [:th "Description"]
         [:th "Amount"]
         [:th (html/space)]]]
       [:tbody
        (->> @transactions
             (map #(result-row % page-state))
             doall)]])))

(defn- load-transactions
  [page-state]
  (+busy page-state)
  (trn/search {:include-items true}
              (fn [transactions]
                (swap! page-state (fn [state]
                                    (-> state
                                        -busy
                                        (assoc
                                          :transactions transactions
                                          :receipts (filter #(t/after? (:created-at %)
                                                                       (-> 12 t/hours t/ago))
                                                            transactions))))))
              (handle-error page-state "Unable to load the transactions: %s")))

(defn- index []
  (let [page-state (r/atom {})]
    (new-receipt page-state)
    (load-transactions page-state)
    (fn []
      [:div.mt-5
       [:h1 "Receipt Entry"]
       [:div.row
        [:div.col
         [receipt-form page-state]]
        [:div.col
         [results-table page-state]]]])))

(secretary/defroute "/receipts" []
  (swap! app-state assoc :page #'index))
