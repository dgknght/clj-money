(ns clj-money.views.receipts
  (:require [clojure.string :as string]
            [goog.string :as gstr]
            [cljs-time.core :as t]
            [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [dgknght.app-lib.web :refer [format-date
                                         format-decimal]]
            [dgknght.app-lib.dom :refer [set-focus]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.decimal :as decimal]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [dgknght.app-lib.forms-validation :as v]
            [clj-money.state :refer [app-state
                                     current-entity
                                     accounts
                                     accounts-by-id
                                     +busy
                                     -busy
                                     busy?]]
            [clj-money.accounts :refer [find-by-path]]
            [clj-money.api.transactions :as trn]))

(defn- new-receipt
  [page-state]
  (let [defaults (-> (get-in @page-state [:receipt])
                     (select-keys [:transaction-date :account-id])
                     (update-in [:transaction-date] (fnil identity (t/today)))
                     (assoc :items [{}]))]
    (swap! page-state assoc :receipt defaults)
    (set-focus "transaction-date")))

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
             :items (mapv #(select-keys % [:account-id :quantity :memo])
                          debit)}))))

(defn- create-transaction
  [receipt page-state]
  (+busy)
  (trn/create (->transaction receipt)
              (map (fn [result]
                     (-busy)
                     (swap! page-state
                            update-in
                            [:receipts]
                            (fnil conj '())
                            result)
                     (new-receipt page-state)))))

(defn- update-transaction
  [receipt page-state]
  (+busy)
  (trn/update (->transaction receipt)
              (map (fn [result]
                     (-busy)
                     (swap! page-state
                            update-in
                            [:receipts]
                            (fn [receipts]
                              (map #(if (= (:id receipt)
                                           (:id %))
                                      result
                                      %)
                                   receipts)))
                     (new-receipt page-state)))))

(defn- remove-empty-items
  [items]
  (remove (every-pred #(nil? (:account-id %))
                      #(nil? (:quantity %)))
          items))

(defn- save-transaction
  [page-state]
  (let [{:keys [receipt]} @page-state
        to-save (update-in receipt [:items] remove-empty-items)]
    (if (:id to-save)
      (update-transaction to-save page-state)
      (create-transaction to-save page-state))))

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
         {:search-fn (search-accounts)
          :find-fn (fn [id callback]
                     (callback (@accounts-by-id id)))
          :on-change #(ensure-blank-item page-state)
          :caption-fn #(string/join "/" (:path %))
          :value-fn :id}]]
   [:td [forms/decimal-input
         receipt
         [:items index :quantity]
         {:on-accept #(ensure-blank-item page-state)}]]
   [:td [forms/text-input
         receipt
         [:items index :memo]
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

(defn- receipt-form
  [page-state]
  (let [receipt (r/cursor page-state [:receipt])
        item-count (make-reaction #(count (:items @receipt)))
        transactions (r/cursor page-state [:transactions])
        total (make-reaction #(->> (:items @receipt)
                                   (map :quantity)
                                   decimal/sum))]
    (fn []
      [:form {:no-validate true
              :on-submit (fn [e]
                           (.preventDefault e)
                           (v/validate receipt)
                           (when (v/valid? receipt)
                             (save-transaction page-state)))}
       [forms/date-field receipt [:transaction-date] {:validations #{::v/required}}]
       [forms/typeahead-field
        receipt
        [:description]
        {:mode :direct
         :validations #{::v/required}
         :caption "Description"
         :search-fn #(search-transactions @transactions %1 %2)
         :find-fn (constantly nil)
         :caption-fn :description
         :list-caption-fn #(gstr/format "%s $%s %s"
                                        (format-date (:transaction-date %))
                                        (format-decimal (:value %))
                                        (:description %))
         :on-change #(swap! page-state reuse-trans %)
         :value-fn :description}]
       [forms/typeahead-field
        receipt
        [:account-id]
        {:validations #{::v/required}
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
        [bs/busy-button {:html {:class "btn-primary"
                                :type :submit
                                :disabled @busy?
                                :title "Click here to create this transaction."}
                         :icon :check
                         :caption "Enter"
                         :busy? busy?}]
        [bs/busy-button {:html {:class "btn-secondary ms-2"
                                :type :button
                                :disabled @busy?
                                :title "Click here to discard this receipt."
                                :on-click (fn []
                                            (swap! receipt select-keys [:transaction-date])
                                            (set-focus "transaction-date"))}
                         :icon :x
                         :caption "Cancel"
                         :busy? busy?}]]])))

(defn- result-row
  [transaction page-state]
  ^{:key (str "result-row-" (:id transaction))}
  [:tr
   [:td (format-date (:transaction-date transaction))]
   [:td (:description transaction)]
   [:td (format-decimal (:value transaction))]
   [:td
    [:button.btn.btn-sm.btn-light
     {:title "Click here to edit this transaction."
      :on-click #(swap! page-state assoc :receipt (->receipt transaction))}
     (bs/icon :pencil {:size :small})]]])

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

(defn- load-transactions
  [page-state]
  (+busy)
  (trn/search {:include-items true}
              (map (fn [transactions]
                     (-busy)
                     (swap! page-state
                            assoc
                            :transactions transactions
                            :receipts (filter #(t/after? (:created-at %)
                                                         (-> 12 t/hours t/ago))
                                              transactions))))))

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
