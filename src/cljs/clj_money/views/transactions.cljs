(ns clj-money.views.transactions
  (:require [clojure.set :refer [rename-keys]]
            [clojure.string :as string]
            [clojure.zip :as zip]
            [goog.string :as gstr]
            [cljs.core.async :refer [>! go]]
            [reagent.core :as r]
            [reagent.format :refer [currency-format]]
            [reagent.ratom :refer [make-reaction]]
            [cljs-time.core :as t]
            [clj-money.decimal :as decimal]
            [clj-money.inflection :refer [humanize]]
            [clj-money.dnd :as dnd]
            [clj-money.bootstrap :as bs]
            [clj-money.html :refer [set-focus
                                    space
                                    special-char]]
            [clj-money.util :as util :refer [parse-int
                                             debounce
                                             format-date
                                             format-decimal]]
            [clj-money.accounts :refer [polarize-quantity]]
            [clj-money.transactions :refer [simplify
                                            fullify
                                            can-simplify?
                                            entryfy
                                            unentryfy
                                            ensure-empty-item
                                            tradify
                                            untradify]]
            [clj-money.forms :as forms]
            [clj-money.components :refer [load-in-chunks]]
            [clj-money.api.transaction-items :as transaction-items]
            [clj-money.api.transactions :as transactions]
            [clj-money.api.attachments :as att]
            [clj-money.api.trading :as trading]
            [clj-money.notifications :as notify]))

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
  (transactions/get-one (item->tkey item)
                        (fn [result]
                          (let [prepared (prepare-transaction-for-edit
                                           result
                                           (:view-account @page-state))]
                            (swap! page-state assoc :transaction prepared))
                          (set-focus "transaction-date"))
                        notify/danger))

(defn stop-item-loading
  [page-state]
  (when-not (:all-items-fetched? @page-state)
    (go (>! (:ctl-chan @page-state) :quit))))

(defn load-unreconciled-items
  [page-state]
  (let [account (:view-account @page-state)]
    (transaction-items/search
      {:account-id (:id account)
       :unreconciled true
       :include-children (:include-children? @page-state)}
      #(swap! page-state assoc :items %)
      (notify/danger-fn "Unable to load the unreconciled items: %s"))))

(defn- load-attachments
  [page-state items]
  (let [[start end] ((juxt first last) (->> items
                                            (sort-by :transaction-date t/before?)
                                            (map :transaction-date)))
        ids (map :transaction-id items)]
    (att/search {:transaction-date [:between start end]
                 :transaction-id ids}
                #(swap! page-state
                       update-in
                       [:attachments]
                       (fnil (partial merge-with conj) {})
                       (group-by :transaction-id %))
                (notify/danger-fn "Unable to load the attachments: %s"))))

(defn init-item-loading
  [page-state]
  (let [account (:view-account @page-state)
        end (t/last-day-of-the-month (or (:latest-transaction-date account)
                                          (t/today))) ; This is probably only nil for newly imported entities
        start (t/first-day-of-the-month (or (:earliest-transaction-date account)
                                                 (t/minus- end (t/months 6))))]
    (swap! page-state dissoc :items :all-items-fetched?)
    (load-in-chunks
      {:start start
       :end end
       :ctl-chan (:ctl-chan @page-state)
       :fetch-fn (fn [date-range callback-fn]
                   (transaction-items/search
                     {:account-id (:id account)
                      :transaction-date date-range}
                     (fn [result]
                       (when (seq result)
                         (load-attachments page-state result))
                       (callback-fn result))
                     (notify/danger-fn "Unable to fetch transaction items: %s")))
       :receive-fn #(swap! page-state update-in [:items] (fnil concat []) %)
       :finish-fn #(swap! page-state assoc :all-items-fetched? true)})))

(defn reset-item-loading
  [page-state]
  (swap! page-state dissoc :items :transaction)
  (stop-item-loading page-state))

(defn- delete-transaction
  [item page-state]
  (when (js/confirm "Are you sure you want to delete this transaction?")
    (transactions/delete (item->tkey item)
                         #(reset-item-loading page-state)
                         notify/danger)))

(defn- handle-item-row-drop
  [item e page-state]
  (.preventDefault e)
  (att/create {:transaction-id (:transaction-id item) ; TODO: use transaction-ref to combine these?
               :transaction-date (:transaction-date item)
               :file (first (dnd/files e))}
              (fn [a]
                (swap! page-state
                       #(-> %
                            (update-in [:attachments]
                                       (fnil (partial merge-with conj) {}) {(:transaction-id a) [a]})
                            (update-in [:item-row-styles]
                                       dissoc (:id item)))))
              (notify/danger-fn "Unable to save the attachment: %s")))

(defn- item-row
  [item page-state]
  (let [account (r/cursor page-state [:view-account])
        reconciliation (r/cursor page-state [:reconciliation])
        attachments (r/cursor page-state [:attachments])
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
     [:td.text-right (format-date (:transaction-date item))]
     [:td {:style (get-in @styles [(:id item)])} (:description item)]
     [:td.text-right (currency-format (polarize-quantity item @account))]
     [:td.text-center
      (if @reconciliation
        [forms/checkbox-input reconciliation [:item-refs (:id item)]]
        (bs/icon
          (case (:reconciliation-status item)
            :completed :check-box
            :new       :dot
            :unchecked-box)))]
     (when-not @reconciliation
       [:td.text-right (currency-format (:balance item))])
     (when-not @reconciliation
       [:td
        [:div.btn-group
         [:button.btn.btn-info.btn-sm {:on-click #(edit-transaction item page-state)
                                       :title "Click here to edit this transaction."}
          (bs/icon :pencil)]
         [:button.btn.btn-sm {:on-click #(swap! page-state
                                                assoc
                                                :attachments-item
                                                item)
                              :class (if (get-in @attachments [(:transaction-id item)])
                                       "btn-info"
                                       "btn-outline-info")
                              :title "Click here to view attachments for this transaction"}
          (bs/icon :paperclip)]
         [:button.btn.btn-danger.btn-sm {:on-click #(delete-transaction item page-state)
                                         :title "Click here to remove this transaction."}
          (bs/icon :x-circle)]]])]))

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
         [:th.text-right "Date"]
         [:th "Description"]
         [:th.text-right "Amount"]
         [:th.text-center "Rec."]
         (when-not @reconciliation
           [:th.text-right "Balance"])
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
                               :transaction-date [:between
                                                  (:earliest-transaction-date @account)
                                                  (:latest-transaction-date @account)]}
                              #(swap! page-state assoc :items %)
                              (notify/danger-fn "Unable to load the transaction items: %s"))
    (fn []
      [:table.table.table-hover.table-borderless
       [:thead
        [:tr
         [:th.text-right "Transaction Date"]
         [:th "Description"]
         [:th.text-right "Qty."]
         [:th.text-right "Bal."]
         [:th.text-right "Value"]]]
       [:tbody
        (doall (for [item (sort-by :index > @items)]
                 ^{:key (str "item-" (:id item))}
                 [:tr
                  [:td.text-right (format-date (:transaction-date item))]
                  [:td (:description item)]
                  [:td.text-right (format-decimal (polarize-quantity item @account) 4)]
                  [:td.text-right (format-decimal (:balance item), 4)]
                  [:td.text-right (currency-format (:value item))]]))]])))

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

(defn- item-navigate
  [e item-count]
  (let [target-id (.-id (.-target e))
        [_ field raw-index] (re-find #"(.*)-(\d)" target-id)
        index (parse-int raw-index)
        [to-field to-index] (case (.-keyCode e)
                              ; up
                              38 [field (mod (dec index) item-count)]

                              ; down
                              40 [field (mod (inc index) item-count)]

                              ; left
                              37 [(prev-item-nav-field field) index]

                              ; right
                              39 [(next-item-nav-field field) index]
                              nil)]
    (when to-field
      (set-focus (str to-field "-" to-index)))))

(defn- item-input-row
  [item index item-count page-state]
  ^{:key (str "item-form-" index)}
  [:tr
   [:td [forms/typeahead-input
         item
         [:account-id]
         {:id (str "account-id-" index)
          :search-fn (fn [input callback]
                       (let [term (string/lower-case input)]
                         (->> (:accounts @page-state)
                              (filter #(string/includes? (string/lower-case (:path %))
                                                         term))
                              callback)))
          :on-change #(ensure-entry-state page-state)
          :on-key-up #(item-navigate % item-count)
          :caption-fn :path
          :value-fn :id
          :find-fn (fn [id callback]
                     (->> (:accounts @page-state)
                          (filter #(= id (:id %)))
                          first
                          callback))}]]
   [:td [forms/text-input item [:memo] {:on-change #(ensure-entry-state page-state)
                                        :on-key-up #(item-navigate % item-count)
                                        :id (str "memo-" index)}]]
   [:td [forms/decimal-input item [:credit-quantity] {:on-accept #(ensure-entry-state page-state)
                                                      :on-key-up #(item-navigate % item-count)
                                                      :id (str "credit-quantity-" index)}]]
   [:td [forms/decimal-input item [:debit-quantity] {:on-accept #(ensure-entry-state page-state)
                                                     :on-key-up #(item-navigate % item-count)
                                                     :id (str "debit-quantity-" index)}]]])

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
      [:form {:class (when-not (mode? @transaction ::full) "d-none")}
       (forms/date-field transaction [:transaction-date] {:validate [:required]})
       (forms/text-field transaction [:description] {:validate [:required]})
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
          [:td.text-right (format-decimal @total-credits)]
          [:td.text-right.d-flex.flex-row-reverse.justify-content-between
           (format-decimal @total-debits)
           (when-not (decimal/zero? @correction)
             [:span.text-danger.mr-3
              (special-char :pm)
              (format-decimal @correction)])]]]]])))

(defn simple-transaction-form
  [page-state]
  (let [transaction (r/cursor page-state [:transaction])
        accounts (r/cursor page-state [:accounts])]
    (fn []
      [:form {:class (when-not (mode? @transaction ::simple) "d-none")}
       (forms/date-field transaction [:transaction-date] {:validate [:required]})
       (forms/text-field transaction [:description] {:validate [:required]})
       (forms/decimal-field transaction [:quantity] {:validate [:required]})
       [forms/typeahead-field
         transaction
         [:other-account-id]
         {:search-fn (fn [input callback]
                       (let [term (string/lower-case input)]
                         (->> @accounts
                              (filter #(string/includes? (string/lower-case (:path %))
                                                         term))
                              callback)))
          :caption-fn :path
          :value-fn :id
          :find-fn (fn [id callback]
                     (->> @accounts
                          (filter #(= id (:id %)))
                          first
                          callback))}]])))

(defn dividend-transaction-form
  [page-state]
  (let [transaction (r/cursor page-state [:transaction])
        commodities (r/cursor page-state [:commodities])]
    (fn []
      [:form {:class (when-not (mode? @transaction ::dividend) "d-none")}
       (forms/date-field transaction [:transaction-date] {:validate [:required]})
       (forms/decimal-field transaction [:quantity] {:validate [:required]
                                                     :caption "Dividend"})
       (forms/decimal-field transaction [:shares] {:validate [:required]})
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
        commodities (r/cursor page-state [:commodities])]
    (fn []
      [:form {:class (when-not (mode? @transaction ::trade) "d-none")}
       [forms/date-field transaction [:trade-date] {:validate [:required]}]
       [forms/select-field transaction [:action] (map (juxt name humanize) [:buy :sell]) {}]
       [forms/decimal-field transaction [:shares] {:validate [:required]}]
       [forms/decimal-field transaction [:value] {:validate [:required]}]
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
          :caption-fn :symbol
          :value-fn :id
          :find-fn (fn [id callback]
                     (callback (get-in @commodities [id])))}]])))

(defn transformations
  [account accounts commodities]
  {::simple #(simplify % account)
   ::full entryfy
   ::trade #(tradify % {:find-account accounts
                        :find-commodity commodities})
   ::dividend #(simplify % account)})

(defn untransformations
  [accounts]
  {::simple #(fullify % accounts)
   ::full unentryfy
   ::trade #(untradify % {:find-account-by-commodity-id (->> accounts
                                                             vals
                                                             (map (juxt :commodity-id identity)) ; TODO: This will cause errors unless we lookup by parent also
                                                             (into {}))})
   ::dividend #(fullify % accounts)})

(defmulti save-transaction
  (fn [page-state _callback]
    (-> @page-state :transaction mode)))

(defmethod save-transaction :default
  [page-state callback]
  (let [{:keys [transaction]
         accounts :mapped-accounts} @page-state
        mode (mode transaction)
        prepare ((untransformations accounts) mode)]
    (-> transaction
        prepare
        (transactions/save callback
                           (notify/danger-fn "Unable to save the transaction: %s")))))

(defmethod save-transaction ::trade
  [page-state callback]
  (trading/create (:transaction @page-state)
                  callback
                  (notify/danger-fn "Unable to create the trade: %s")))

(defn- assoc-reinvest-desc
  [transaction commodity]
  (assoc transaction
         :description
         (gstr/format "Reinvest %.2f into %.3f shares of %s (%s)"
                      (:value transaction)
                      (:shares transaction)
                      (:name commodity)
                      (:symbol commodity))))

(defmethod save-transaction ::dividend
  [page-state callback]
  (let [{:keys [transaction commodities mapped-accounts]} @page-state
        commodity (get-in commodities [(:commodity-id transaction)])
        dividends-account (->> mapped-accounts ; TODO: Need a better way to make sure we have this value
                               vals
                               (filter #(= :income (:type %)))
                               (filter #(re-find #"(?i)dividend" (:name %)))
                               first)
        t1 (-> transaction
               (dissoc :commodity-id :shares)
               (assoc :description (gstr/format "%s (%s)"
                                                (:name commodity)
                                                (:symbol commodity))
                      :other-account-id (:id dividends-account))
               (fullify mapped-accounts))
        t2 (-> transaction
               (rename-keys {:quantity :value
                             :transaction-date :trade-date})
               (assoc-reinvest-desc commodity)
               (assoc :action :buy))]
    (transactions/save t1
                       (fn [_]
                         (trading/create t2
                                         callback
                                         (notify/danger-fn "Unable to create the trade: %s")))
                       (notify/danger-fn "Unable to create the dividend transaction: %s"))))
