(ns clj-money.views.receipts
  (:require [clojure.string :as string]
            [goog.string :as gstr]
            [cljs-time.core :as t]
            [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [clj-money.decimal :as decimal]
            [clj-money.state :refer [app-state
                                     current-entity]]
            [clj-money.bootstrap :as bs]
            [clj-money.html :as html]
            [clj-money.util :as util :refer [format-date
                                             format-decimal]]
            [clj-money.forms :as forms]
            [clj-money.notifications :as notify]
            [clj-money.accounts :refer [nest unnest]]
            [clj-money.api.accounts :as act]
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
  (trn/create (->transaction receipt)
              (fn [result]
                (swap! page-state
                       update-in
                       [:receipts]
                       (fnil conj '())
                       result)
                (new-receipt page-state))
              (notify/danger-fn "Unable to create the transaction: %s")))

(defn- update-transaction
  [receipt page-state]
  (trn/update (->transaction receipt)
              (fn [result]
                (swap! page-state
                       update-in
                       [:receipts]
                       (fn [receipts]
                         (map #(if (= (:id receipt)
                                      (:id %))
                                 result
                                 %)
                              receipts)))
                (new-receipt page-state))
              (notify/danger-fn "Unable to update the transaction: s%")))

(defn- save-transaction
  [page-state]
  (let [{:keys [receipt]} @page-state]
    (if (:id receipt)
      (update-transaction receipt page-state)
      (create-transaction receipt page-state))))

(defn- search-coll
  [coll field input callback]
  (let [term (string/lower-case input)]
    (->> coll
         (filter #(-> %
                      (get-in field)
                      string/lower-case
                      (string/includes? term)))
         callback)))

(defn- search-accounts
  [accounts input callback]
  (search-coll (vals accounts) [:path] input callback))

(defn- search-transactions
  [transactions input callback]
  (search-coll transactions [:description] input callback))

(defn- ensure-blank-item
  [page-state]
  (let [{{:keys [items]} :receipt} @page-state]
    (when-not (some empty? items)
      (swap! page-state update-in [:receipt :items] conj {}))))

(defn- receipt-item-row
  [index receipt accounts page-state]
  ^{:key (str "receipt-item-" index)}
  [:tr
   [:td [forms/typeahead-input
         receipt
         [:items index :account-id]
         {:validate [:required]
          :search-fn #(search-accounts @accounts %1 %2)
          :find-fn (fn [id callback]
                    (callback (@accounts id)))
          :on-change #(ensure-blank-item page-state)
          :caption-fn :path
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
        accounts (r/cursor page-state [:accounts])
        transactions (r/cursor page-state [:transactions])
        search (r/cursor page-state [:transaction-search])
        total (make-reaction #(->> (:items @receipt)
                                   (map :quantity)
                                   decimal/sum))]
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
                                (html/set-focus "search-term"))}
                   (bs/icon :search)]
         :attr {:auto-complete :off}
         :form-group-attr {:class (when @search "d-none")}}]
       [forms/typeahead-field
        search
        [:search-term]
        {:validate [:required]
         :form-group-attr {:class (when-not @search "d-none")}
         :list-attr {:style {:top "2em"}}
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
         :search-fn #(search-accounts @accounts %1 %2)
         :find-fn (fn [id callback]
                    (callback (@accounts id)))
         :caption-fn :path
         :value-fn :id}]
       [:table.table.table-borderless
        [:thead
         [:tr
          [:th "Category"]
          [:th "Amount"]]]
        [:tbody
         (->> (range @item-count)
              (map #(receipt-item-row % receipt accounts page-state))
              doall)]
        [:tfoot
         [:tr
          [:td.text-right {:col-span 2}
           (format-decimal @total)]]]]
       [:div
        [:button.btn.btn-primary
         {:type :submit
          :title "Click here to create this transaction."}
         (bs/icon-with-text :check "Enter")]]])))

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

(defn- load-accounts
  [page-state]
  (act/select (fn [result]
                (swap! page-state assoc :accounts (->> result
                                                       nest
                                                       unnest
                                                       util/->indexed-map)))
              (notify/danger-fn "Unable to load the accounts: %s")))

(defn- load-transactions
  [page-state]
  (trn/search {:transaction-date [:>= (-> 6 t/months t/ago)]
               :include-items true}
              (fn [transactions]
                (swap! page-state assoc
                       :transactions transactions
                       :receipts (filter #(t/after? (:created-at %)
                                                    (-> 12 t/hours t/ago))
                                         transactions)))
              (notify/danger-fn "Unable to load the transactions: %s")))

(defn- index []
  (let [page-state (r/atom {})]
    (new-receipt page-state)
    (load-accounts page-state)
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
