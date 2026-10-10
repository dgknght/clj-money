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
            [dgknght.app-lib.notifications :as notify]
            [dgknght.app-lib.bootstrap-5 :as bs]
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
            [clj-money.api.receipt-ingestions :as ri]
            [clj-money.views.attachments :as atts-view]
            [clj-money.views.ingestion-settings :as ingestion-settings]
            [clj-money.views.recent-transactions :as recent-trx]))

(defn- clear-receipt-image
  [page-state]
  (some-> (get-in @page-state [:receipt-image :url]) js/URL.revokeObjectURL)
  (swap! page-state dissoc :receipt-image))

(defn- clear-ingestion
  "Stops following the read of a receipt image, and forgets the
  transaction created from it."
  [page-state]
  (swap! page-state dissoc :reading? :ingestion :ingested-receipt :rejection))

(defn- new-receipt
  [page-state]
  (clear-receipt-image page-state)
  (clear-ingestion page-state)
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
  (let [{:keys [receipt ingestion]} @page-state]
    (-> receipt
        receipts/->transaction
        ; saving a transaction read from a receipt image accepts it
        (cond-> ingestion (assoc :transaction/review-status :accepted))
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

(def ^:private ingestion-poll-interval
  "How long to wait, in milliseconds, between checks on the read of a
  receipt image."
  2000)

(defn- following?
  "Returns true if the page is still waiting on the specified ingestion."
  [page-state {:keys [id]}]
  (= id (get-in @page-state [:ingestion :id])))

(defn- load-ingested-receipt
  "Opens the transaction read from the receipt image in the form, for
  the user to review."
  [page-state {:receipt-ingestion/keys [receipt] :as ingestion}]
  (let [receipt (update-in receipt [:receipt/items] #(conj (vec %) {}))]
    (swap! page-state #(-> %
                           (dissoc :reading?)
                           (assoc :ingestion ingestion
                                  :receipt receipt
                                  :ingested-receipt receipt)))
    (set-focus "transaction-date")))

(defn- await-ingestion
  [page-state ingestion]
  (js/setTimeout
    (fn []
      (when (following? page-state ingestion)
        (ri/get ingestion
                :on-failure (fn [_] (clear-ingestion page-state))
                :on-success
                (fn [{:receipt-ingestion/keys [status error] :as updated}]
                  (when (following? page-state updated)
                    (case status
                      :complete (load-ingested-receipt page-state updated)
                      :failed (do
                                (notify/dangerf "Unable to read the receipt: %s" error)
                                (clear-ingestion page-state))
                      (do
                        (swap! page-state assoc :ingestion updated)
                        (await-ingestion page-state updated))))))))
    ingestion-poll-interval))

(defn- ingest-receipt
  "Uploads the receipt image to be read in the background, then waits
  for the transaction created from it."
  [page-state image]
  (when image
    (clear-ingestion page-state)
    ; the form is withheld until the read is finished
    (swap! page-state assoc :reading? true)
    (+busy)
    (ri/create image
               :callback -busy
               :on-failure (fn [_] (clear-ingestion page-state))
               :on-success (fn [ingestion]
                             (swap! page-state assoc :ingestion ingestion)
                             (await-ingestion page-state ingestion)))))

(defn- reject-transaction
  [page-state]
  (let [{:keys [ingestion] {:keys [reason]} :rejection} @page-state
        trx (:receipt-ingestion/transaction ingestion)]
    (+busy)
    (ri/reject ingestion
               reason
               :callback -busy
               :on-success (fn [_]
                             (swap! page-state
                                    update-in
                                    [:transactions]
                                    (partial remove #(util/id= trx %)))
                             (new-receipt page-state)))))

(defn- rejection-form
  [page-state]
  (let [rejection (r/cursor page-state [:rejection])]
    (fn []
      (when @rejection
        [:form.mb-2 {:no-validate true
                     :on-submit (fn [e]
                                  (.preventDefault e)
                                  (v/validate rejection)
                                  (when (v/valid? rejection)
                                    (reject-transaction page-state)))}
         [forms/text-field rejection [:reason] {:caption "Reason for Rejecting"
                                                :validations #{::v/required}}]
         [:button.btn.btn-danger
          {:type :submit
           :title "Click here to delete the transaction read from the receipt."}
          (icon-with-text :x "Reject")]
         [:button.btn.btn-secondary.ms-2
          {:type :button
           :title "Click here to keep the transaction."
           :on-click #(swap! page-state dissoc :rejection)}
          (icon-with-text :arrow-left-short "Back")]]))))

(defn- placeholder-field
  [caption]
  [:div.mb-3
   [:label.form-label caption]
   [:div.form-control.placeholder-glow
    [:span.placeholder.col-6]]])

(defn- reading-placeholder
  "Stands in for the receipt form while the receipt image is read, so no
  other action can be taken until the transaction is ready."
  []
  [:div
   [placeholder-field "Transaction Date"]
   [placeholder-field "Description"]
   [placeholder-field "Payment Method"]
   [placeholder-field "Payment Memo"]
   [:div.placeholder-glow.mb-3
    (for [i (range 3)]
      ^{:key (str "placeholder-item-" i)}
      [:div.d-flex.mb-2
       [:span.placeholder.col-5.me-2]
       [:span.placeholder.col-3.me-2]
       [:span.placeholder.col-3]])]
   [:div.mb-2.d-flex.align-items-center.text-muted
    [bs/spinner {:size :small}]
    [:span.ms-2 "Reading the receipt..."]]])

(def ^:private ingestion-settings-id "receipts-ingestion-settings")

(defn- receipt-form
  [page-state]
  (let [receipt (r/cursor page-state [:receipt])
        item-count (make-reaction #(count (:receipt/items @receipt)))
        history (r/cursor page-state [:historical-transactions])
        total (make-reaction #(receipts/total @receipt))
        ; an unchanged transaction read from a receipt image is
        ; offered for acceptance
        reading? (r/cursor page-state [:reading?])
        ingestion (r/cursor page-state [:ingestion])
        settings-draft (r/cursor page-state [:ingestion-settings])
        accepting? (make-reaction #(let [{:keys [receipt ingested-receipt]} @page-state]
                                     (and ingested-receipt
                                          (= receipt ingested-receipt))))]
    (fn []
      (if @reading?
          [reading-placeholder]
          [:<>
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
              ; shows the description when the form is mounted with one,
              ; as when a receipt image has been read
              :find-fn (fn [description callback]
                         (callback #:transaction{:description description}))
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
            [:div.mb-2.d-flex.align-items-center
             (if @accepting?
               [:button.btn.btn-success
                {:type :submit
                 :title "Click here to accept the transaction read from the receipt."}
                (icon-with-text :check "Accept")]
               [:button.btn.btn-primary
                {:type :submit
                 :title "Click here to create this transaction."}
                (icon-with-text :check "Enter")])
             (if @accepting?
               [:button.btn.btn-danger.ms-2
                {:type :button
                 :title "Click here to reject the transaction read from the receipt."
                 :on-click #(swap! page-state assoc :rejection {})}
                (icon-with-text :x "Reject")]
               [:button.btn.btn-secondary.ms-2
                {:type :button
                 :title "Click here to discard this receipt."
                 :on-click (fn [_]
                             (clear-receipt-image page-state)
                             (clear-ingestion page-state)
                             (swap! receipt select-keys [:receipt/transaction-date])
                             (set-focus "transaction-date"))}
                (icon-with-text :x "Cancel")])
             ; another image can't be chosen until this transaction is
             ; accepted or rejected
             (when-not @ingestion
               [:div.btn-group.ms-2
                ; opens the file chooser of the hidden image input
                [:label.btn.btn-secondary
                 {:for "receipt-image"
                  :title "Click here to take or choose a photo of a receipt."}
                 (icon-with-text :camera-fill "Scan" :size :small)]
                [ingestion-settings/toggle ingestion-settings-id settings-draft]])
             ; reading starts as soon as an image is chosen, so the
             ; preview and buttons of the image input aren't needed
             (when-not @ingestion
               [:div.d-none
                [forms/image-input
                 page-state
                 [:receipt-image]
                 {:capture "environment"
                  :on-change #(ingest-receipt page-state %)
                  ; large enough to keep the fine print on a receipt legible
                  :resize {:max-dimension 2048
                           :on-error #(notify/danger "Unable to read the image.")}}]])]]
           [rejection-form page-state]]))))

(defn- receipt-image
  "Shows the receipt image while it's read, and then beside the transaction
  read from it, so the user can compare them."
  [page-state]
  (when-let [url (get-in @page-state [:receipt-image :url])]
    [:div.card
     [:div.card-header [:strong "Receipt"]]
     [:img.card-img-bottom {:src url
                            :alt "The receipt the transaction was read from"}]]))

(defn- load-attachments
  [page-state]
  (let [{:keys [attachments-item]} @page-state]
    (+busy)
    (atts/select {:attachment/transaction attachments-item}
                 :callback -busy
                 :on-success #(swap! page-state assoc :attachments %))))

(defn update-attachment-count
  "Applies f to the attachment count of the transaction in :transactions
  identified by trx."
  [state trx f]
  (update-in state
             [:transactions]
             (fn [transactions]
               (map (fn [t]
                      (if (util/id= trx t)
                        (update-in t [:transaction/attachment-count] (fnil f 0))
                        t))
                    transactions))))

(defn- post-result-row-drop
  [page-state trx]
  (fn [_created]
    (swap! page-state
           (fn [state]
             (-> state
                 (update-in [:result-row-styles] dissoc (:id trx))
                 (update-attachment-count trx inc))))))

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
       :on-click (fn []
                   (clear-receipt-image page-state)
                   (clear-ingestion page-state)
                   (swap! page-state assoc :receipt (receipts/<-transaction trx)))}
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
        attachments-item (r/cursor page-state [:attachments-item])
        reading? (r/cursor page-state [:reading?])
        ingested-receipt (r/cursor page-state [:ingested-receipt])]
    (new-receipt page-state)
    (load-transactions page-state)
    (load-historical-transactions page-state)
    (add-watch page-state ::filter-date
               (fn [_ _ old new]
                 (when (not= (:filter-date old) (:filter-date new))
                   (load-transactions page-state))))
    (add-watch current-entity
               ::index
               (fn [_ _ previous current]
                 ; saving the entity's settings shouldn't reset the page
                 (when-not (util/id= previous current)
                   (swap! page-state
                          #(-> %
                               (dissoc :attachments-item :attachments)
                               (assoc :transactions [])))
                   (new-receipt page-state)
                   (load-transactions page-state)
                   (load-historical-transactions page-state))))
    (fn []
      [:<>
       [:h1.mt-3 "Receipts"]
       [:div.row
        [:div.col-md-6
         [:h3 "New Transaction"]
         [receipt-form page-state]
         [ingestion-settings/drawer
          ingestion-settings-id
          (r/cursor page-state [:ingestion-settings])]]
        [:div.col-md-6
         (cond
           (or @reading? @ingested-receipt)
           [receipt-image page-state]

           @attachments-item
           [:<>
            [atts-view/attachments-card page-state
             :on-delete #(swap! page-state
                                update-attachment-count
                                (:attachment/transaction %)
                                dec)]
            [atts-view/attachment-form page-state]]

           :else
           [results-table page-state])]]])))

(secretary/defroute "/receipts" []
  (swap! app-state assoc :page #'index :active-nav :receipts))
