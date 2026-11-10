(ns clj-money.views.transactions
  (:require [clojure.string :as string]
            [cljs.core.async :as a :refer [<! >! go go-loop]]
            [cljs-time.core :as t]
            [reagent.core :as r]
            [reagent.format :refer [currency-format]]
            [reagent.ratom :refer [make-reaction]]
            [dgknght.app-lib.web :refer [format-date
                                         format-decimal ]]
            [dgknght.app-lib.inflection :refer [humanize]]
            [dgknght.app-lib.dom :refer [set-focus]]
            [dgknght.app-lib.html :refer [space]]
            [dgknght.app-lib.decimal :as decimal]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [dgknght.app-lib.forms-validation :as v]
            [clj-money.icons :refer [icon]]
            [clj-money.state :refer [accounts
                                     accounts-by-id
                                     +busy
                                     -busy]]
            [clj-money.util :as util :refer [id=]]
            [clj-money.dates :as dates]
            [clj-money.commodities :as cmdts]
            [clj-money.accounts :refer [find-by-path
                                        transfer-destination?
                                        format-quantity
                                        polarize-quantity]]
            [clj-money.transactions :refer [->trade-price
                                            trade->transfer]]
            [clj-money.components :refer [load-in-chunks
                                          append-chunk
                                          audit-history-popover]]
            [clj-money.api.transaction-items :as trx-items]
            [clj-money.api.transactions :as transactions]
            [clj-money.api.reconciliations :as recs-api]
            [clj-money.api.attachments :as atts]
            [clj-money.views.attachments :as atts-view]
            [clj-money.views.recent-transactions :as recent-trx]
            [clj-money.api.trading :as trading]
            [clj-money.api.transfers :as transfers]
            [clj-money.api.audit :as audit]
            [clj-money.views.transactions.form :as tf]))

(defn edit-transaction
  [transaction-item page-state]
  (+busy)
  (transactions/get-by-transaction-item
    transaction-item
    :callback -busy
    :post-xf (comp (map first)
                   (map (tf/prepare-for-edit
                          (:view-account @page-state))))
    :on-success (fn [trx]
                  (swap! page-state assoc :transaction trx)
                  (set-focus "transaction-date"))))

(defn- polarize-quantities
  [account]
  (fn [items]
    (map (comp #(assoc %
                       :transaction-item/polarized-quantity
                       (polarize-quantity %))
               #(assoc % :transaction-item/account account))
         items)))

(defn- descendant-accounts
  [{:keys [id]}]
  (filter #(contains? (set (:account/parent-ids %)) id) @accounts))

(defn- effective-transaction-date-range
  [account include-children?]
  (->> (cons account (when include-children? (descendant-accounts account)))
       (map :account/transaction-date-range)
       (filter identity)
       (reduce (fn [r [start end]]
                 (dates/push-boundary r start end))
               [])))

(defn load-unreconciled-items
  [page-state]
  (+busy)
  (let [account (:view-account @page-state)
        include-children? (:include-children? @page-state)
        criteria {:transaction-item/account account
                  :transaction/transaction-date (apply vector
                                                       :between
                                                       (effective-transaction-date-range account include-children?))
                  :unreconciled true
                  :include-children include-children?}]
    (trx-items/select
      criteria
      :callback -busy
      :post-xf (map (polarize-quantities account))
      :on-success #(swap! page-state assoc :items %))))

(defn- load-attachments
  [page-state]
  (let [{{:transaction-item/keys [transaction]} :attachments-item} @page-state
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
        (trx-items/select criteria
                          :on-success #(xf ch %))
        (xf ch [])))))

(defn stop-item-loading
  [page-state]
  (let [{:keys [all-items-fetched? item-loader]} @page-state]
    (swap! page-state dissoc :item-loader)
    (when (and item-loader (not all-items-fetched?))
      (go (>! item-loader :quit)))))

(defn- load-open-reconciliations
  "Loads the ids of the account's reconciliations in progress, whose items
  aren't reconciled yet"
  [page-state account]
  (when account
    (recs-api/select #:reconciliation{:account account
                                      :status :new}
                     :on-success #(swap! page-state
                                         assoc
                                         :open-reconciliation-ids
                                         (set (map :id %))))))

(defn- reconciliation-status
  [{:transaction-item/keys [reconciliation] :keys [id]} open-reconciliation-ids]
  (cond
    (nil? id) :new
    (nil? reconciliation) :unreconciled
    (contains? open-reconciliation-ids (:id reconciliation)) :in-progress
    :else :completed))

(defn- reconciliation-icon
  [status]
  (case status
    :completed (icon :check-square :size :small)
    (:new :in-progress) (icon :dash-square :size :small)
    (icon :square :size :small)))

(defn init-item-loading
  ([page-state] (init-item-loading page-state nil))
  ([page-state {:keys [on-chunk]}]
   (let [account (get-in @page-state [:view-account])
         [first-date last-date :as range] (:account/transaction-date-range account)]
     (stop-item-loading page-state)
     (load-open-reconciliations page-state account)
     (if range
       (do (swap! page-state dissoc :items :all-items-fetched?)
           (let [{:keys [ctl-ch items-ch]} (->> (dates/desc-ranges first-date last-date (t/months 6))
                                                (load-in-chunks
                                                  {:fetch-xf (comp
                                                               (map (fn [[start end :as range]]
                                                                      (when (seq range)
                                                                        {:transaction-item/account (util/->entity-ref account)
                                                                         :transaction/transaction-date [:between> start end]})))
                                                               fetch-items)
                                                   :chunk-size 100}))
                 out-ch (a/chan
                          1
                          (map (polarize-quantities account)))
                 current? #(identical? ctl-ch (:item-loader @page-state))]
             (swap! page-state assoc :ctl-chan ctl-ch :item-loader ctl-ch)
             (a/pipe items-ch out-ch)
             (go-loop [items (<! out-ch)]
                      (when (current?)
                        (if items
                          (do
                            (swap! page-state append-chunk ctl-ch items)
                            (when on-chunk (on-chunk items))
                            (recur (<! out-ch)))
                          (swap! page-state assoc :all-items-fetched? true))))
             (go (>! ctl-ch :fetch))))
       (swap! page-state assoc :items [] :items-sort nil)))))

(defn reset-item-loading
  [page-state]
  (swap! page-state dissoc :items :transaction)
  (stop-item-loading page-state)
  (go (<! (a/timeout 250)) ; give the pipes some time to flush out
      (init-item-loading page-state)))

(defn- delete-transaction
  [transaction-item page-state]
  (when (js/confirm "Are you sure you want to delete this transaction?")
    (+busy)
    (trx-items/delete
      transaction-item
      :callback -busy
      :on-success (fn []
                    (if (:reconciliation @page-state)
                      (do
                        (swap! page-state
                               update-in
                               [:reconciliation :clj-money.views.reconciliations/item-selection]
                               dissoc
                               (:id transaction-item))
                        (load-unreconciled-items page-state))
                      (reset-item-loading page-state))))))

(defn update-attachment-count
  "Applies f to the attachment count of each item in :items that belongs to
  the transaction of the given attachment."
  [state attachment f]
  (update-in state
             [:items]
             (fn [items]
               (map (fn [item]
                      (if (id= (:attachment/transaction attachment)
                               (:transaction-item/transaction item))
                        (update-in item [:transaction/attachment-count] (fnil f 0))
                        item))
                    items))))

(defn- post-item-row-drop
  [page-state item]
  (fn [created]
    (swap! page-state
           (fn [state]
             (-> state
                 (update-in [:item-row-styles]
                            dissoc
                            (:id item))
                 (update-attachment-count created inc))))))

(defn attachments-card
  [page-state]
  [atts-view/attachments-card page-state
   :on-delete #(swap! page-state update-attachment-count % dec)])

(defn pending-attachment-form
  [page-state]
  [atts-view/pending-attachment-form page-state
   :on-save-success (fn [{:keys [item]} created]
                       ((post-item-row-drop page-state item) created))])

(defn- item-row-buttons
  [{:as item
    :transaction/keys [attachment-count]
    :reconciliation/keys [status]}
   page-state]
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
      [:span.badge.bg-info.text-dark attachment-count])]
   [:button.btn.btn-danger.btn-sm
    (if (= :completed status)
      {:disabled true
       :title "This transaction is part of a completed reconciliation, so it can't be removed."}
      {:on-click #(delete-transaction item page-state)
       :title "Click here to remove this transaction."})
    (icon :x-circle :size :small)]])

(defn- item-row
  [{:keys [account
           reconciliation
           styles]}
   page-state]
  (fn [{:transaction-item/keys [polarized-quantity
                                balance]
        :reconciliation/keys [status]
        :transaction/keys [description
                           transaction-date
                           review-status]
        :as item}]
    (let [checked? (get-in @reconciliation [:clj-money.views.reconciliations/item-selection (:id item)])
          muted-class (when checked? "text-body-secondary")]
      ^{:key (str "item-row-" (:id item))}
      [:tr.align-middle
       (atts-view/drop-handlers page-state :item-row-styles (:id item)
                                {:item item
                                 :attachment #:attachment{:transaction (:transaction-item/transaction item)
                                                          :caption ""}})
       [:td.text-end
        {:class muted-class}
        [:span.d-md-none (format-date transaction-date "M/d")]
        [:span.d-none.d-md-inline (format-date transaction-date)]]
       [:td {:class muted-class
             :style (get-in styles [(:id item)])}
        [:div.d-flex.justify-content-between
         [:span description]
         (when (= :pending review-status)
           [:span.badge.text-bg-warning.ms-2
            {:title "This transaction was read from a statement or receipt. Edit it to review it, or complete the reconciliation to accept it."}
            "Review"])]]
       [:td.text-end {:class muted-class} (format-quantity polarized-quantity account)]
       [:td.text-center.d-none.d-md-table-cell
        {:class muted-class}
        (if @reconciliation
          [forms/checkbox-input
           reconciliation
           [:clj-money.views.reconciliations/item-selection (:id item)]
           {::forms/decoration ::forms/none}]
          (reconciliation-icon status))]
       (when-not @reconciliation
         [:td.text-end.d-none.d-md-table-cell (format-quantity balance
                                                               account)])
       [:td.d-flex.justify-content-end
        {:class muted-class}
        (if @reconciliation
          [:div.btn-group
           [:button.btn.btn-secondary.btn-sm
            {:on-click #(edit-transaction item page-state)
             :title "Click here to edit this transaction."}
            (icon :pencil :size :small)]
           [:button.btn.btn-danger.btn-sm
            {:on-click #(delete-transaction item page-state)
             :title "Click here to remove this transaction."}
            (icon :x-circle :size :small)]]
          [item-row-buttons item page-state])]])))

(defn- date-compare
  [d1 d2]
  (t/before? (or d1 (t/epoch))
             (or d2 (t/epoch))))

(defn- description-compare
  [d1 d2]
  (compare (or d1 "") (or d2 "")))

(defn- amount-compare
  [a1 a2]
  (decimal/< (or a1 (decimal/zero)) (or a2 (decimal/zero))))

(def ^:private sort-fns
  {:transaction/transaction-date date-compare
   :transaction/description description-compare
   :transaction-item/polarized-quantity amount-compare})

(defn- apply-sort-direction
  [cmp dir]
  (if (= dir :desc)
    #(cmp %2 %1)
    cmp))

(defn- sort-link
  [attr label data-type [sort-on sort-dir] page-state]
  [:span.d-flex
   [:a.me-2.text-muted.text-decoration-none
    {:href "#"
     :on-click (fn [_]
                 (if (= attr sort-on)
                   (swap! page-state
                          update-in
                          [:items-sort 1]
                          #(if (= % :desc) :asc :desc))
                   (swap! page-state assoc
                          :items-sort [attr :asc])))}
    label]
   [:span.text-muted
    {:class (when-not (= attr sort-on) "invisible")}
    (icon (keyword (str "sort-"
                        (name data-type)
                        (if (= :asc sort-dir)
                          "-up"
                          "-down-alt"))))]])

(defn items-table
  [page-state]
  (let [raw-items (r/cursor page-state [:items])
        open-recons (r/cursor page-state [:open-reconciliation-ids])
        items (make-reaction (fn []
                               (when @raw-items
                                 (map #(assoc %
                                             :reconciliation/status
                                             (reconciliation-status % @open-recons))
                                      @raw-items))))
        styles (r/cursor page-state [:item-row-styles])
        include-children? (r/cursor page-state [:include-children?])
        account (r/cursor page-state [:view-account])
        recon (r/cursor page-state [:reconciliation])
        items-sort (r/cursor page-state [:items-sort])
        sort-on (r/cursor items-sort [0])
        sort-dir (r/cursor items-sort [1])
        sort-fn (make-reaction (fn []
                                 (when @sort-on
                                   (apply-sort-direction (sort-fns @sort-on) @sort-dir))))
        filter-fn (make-reaction (fn []
                                   (if @include-children?
                                     identity
                                     #(id= @account
                                           (:transaction-item/account %)))))]
    (fn []
      [:table.table.table-striped.table-hover
       [:thead
        [:tr
         [:th.text-end (sort-link :transaction/transaction-date "Date" :numeric @items-sort page-state)]
         [:th (sort-link :transaction/description "Description" :alpha @items-sort page-state)]
         [:th.text-end (sort-link :transaction-item/polarized-quantity "Amount" :numeric @items-sort page-state)]
         [:th.text-center.d-none.d-md-table-cell "Rec."]
         (when-not @recon
           [:th.text-end.d-none.d-md-table-cell "Balance"])
         [:th (space)]]]
       [:tbody
        (cond
          (seq @items)
          (->> @items
               (filter @filter-fn)
               (#(if @sort-on (sort-by @sort-on @sort-fn %) %))
               (map (item-row {:account @account
                               :reconciliation recon
                               :styles styles}
                              page-state))
               doall)

          @items
          [:tr [:td.text-center {:col-span 6} "No transactions"]]

          :else
          [:tr [:td.text-center {:col-span 6} (bs/spinner {:size :small})]])]])))

(defn- recent-transaction-row
  [account page-state]
  (fn [{:transaction/keys [transaction-date description]
        :transaction-item/keys [polarized-quantity]
        :as item}]
    ^{:key (str "recent-trx-" (:id item))}
    [:tr
     [:td.text-end (format-date transaction-date)]
     [:td description]
     [:td.text-end (format-quantity polarized-quantity account)]
     [:td.text-end
      [:button.btn.btn-sm.btn-secondary
       {:on-click #(edit-transaction item page-state)
        :title "Click here to edit this transaction."}
       (icon :pencil :size :small)]]]))

(def ^:private recent-options-id "trade-recent-transactions-options")

(defn recent-transactions-table
  [page-state]
  (let [account (r/cursor page-state [:view-account])]
    (fn []
      [recent-trx/table
       {:id recent-options-id
        :page-state page-state
        :items-path [:items]
        :row-fn (recent-transaction-row @account page-state)}])))

(defn- load-trx-item-audit!
  [page-state item]
  (audit/select item :transaction-item/quantity
                :on-success
                #(swap! page-state
                        assoc-in
                        [:trx-item-audit-histories (:id item)]
                        %)))

(defn init-fund-item-loading
  [page-state]
  (init-item-loading page-state
                      {:on-chunk (fn [items]
                                   (doseq [item items]
                                     (load-trx-item-audit! page-state item)))}))

(defn- toggle-trx-item-audit!
  [page-state item]
  (let [id (:id item)]
    (swap! page-state update :trx-item-audit-expanded
           (fn [expanded]
             (let [s (or expanded #{})]
               (if (s id) (disj s id) (conj s id)))))))

(defn- fund-transaction-row
  [{:as item
    :transaction/keys [transaction-date
                       description]
    :transaction-item/keys [polarized-quantity
                            balance
                            value]
    :reconciliation/keys [status]}
   page-state]
  (let [id (:id item)
        audit-history (get-in @page-state [:trx-item-audit-histories id])
        expanded? (contains? (:trx-item-audit-expanded @page-state) id)]
    ^{:key (str "item-" id)}
    [:tr
     [:td.text-end (format-date transaction-date)]
     [:td description]
     [:td.text-end
      [:div.d-flex.align-items-center
       [audit-history-popover
        audit-history
        expanded?
        #(toggle-trx-item-audit! page-state item)]
       [:div.ms-auto (format-decimal polarized-quantity 4)]]]
     [:td.text-end (format-decimal balance 4)]
     [:td.text-end (currency-format (or value polarized-quantity))]
     [:td.text-center.d-none.d-md-table-cell
      (reconciliation-icon status)]]))

(defn- lot-note-row
  [{:lot-note/keys [transaction-date memo] :as note}]
  ^{:key (str "note-" (:id note))}
  [:tr
   [:td.text-end (format-date transaction-date)]
   [:td.text-muted.fst-italic {:col-span 5} memo]])

(defn fund-transactions-table
  [page-state]
  (let [raw-items (r/cursor page-state [:items])
        lot-notes (r/cursor page-state [:lot-notes])
        open-recons (r/cursor page-state [:open-reconciliation-ids])
        items (make-reaction
                (fn []
                  (when @raw-items
                    (map #(assoc %
                                :reconciliation/status
                                (reconciliation-status % @open-recons))
                         @raw-items))))
        all-rows (make-reaction
                   #(sort-by
                      (fn [row]
                        (or (some-> (:transaction/transaction-date row)
                                    dates/serialize-local-date)
                            (some-> (:lot-note/transaction-date row)
                                    dates/serialize-local-date)
                            ""))
                      (fn [a b] (compare b a))
                      (concat @items @lot-notes)))]
    (init-fund-item-loading page-state)
    (fn []
      [:table.table.table-hover.table-borderless
       [:thead
        [:tr
         [:th.text-end "Transaction Date"]
         [:th "Description"]
         [:th.text-end "Qty."]
         [:th.text-end "Bal."]
         [:th.text-end "Value"]
         [:th.text-center.d-none.d-md-table-cell "Rec."]]]
       [:tbody
        (cond
          (nil? @raw-items)
          [:tr
           [:td.text-center.fw-lighter {:col-span 6}
            [:div.d-flex.justify-content-center.m2
             [:div.spinner-border {:role :status}
              [:span.visually-hidden "Loading"]]]]]

          (seq @items)
          (doall
            (for [row @all-rows]
              (if (:lot-note/transaction-date row)
                (lot-note-row row)
                (fund-transaction-row row page-state))))

          :else
          [:tr [:td.text-center.fw-lighter
                {:col-span 6}
                "No transaction for this commodity."]])]])))

(defn- drop-hidden-field-rules
  "Validation rules stay in the model's metadata after their field is
  unmounted, so remove the rules for the fields that the selected action
  doesn't show"
  [trade]
  (let [hidden (if (= :transfer (:trade/action @trade))
                 [[:trade/value]]
                 [[:trade/to-account]])]
    (swap! trade
           vary-meta
           update-in
           [::v/validation ::v/rules]
           #(apply dissoc % hidden))))

(defn- shares-held
  "Returns the shares of the trade's commodity held in the trade's account,
  read from the balance of the commodity account beneath it"
  [{:trade/keys [account commodity]}]
  (when (and account commodity)
    (let [commodity-id (or (:id commodity) commodity)]
      (some (fn [{:account/keys [parent quantity] :as a}]
              (when (and (id= parent account)
                         (= commodity-id
                            (get-in a [:account/commodity :id])))
                (or quantity (decimal/zero))))
            @accounts))))

(defn trade-transaction-form
  [page-state & {:keys [on-save]}]
  (let [trade (r/cursor page-state [:trade])
        dividend? (r/cursor trade [:trade/dividend?])
        transfer? (make-reaction #(= :transfer (:trade/action @trade)))
        held (make-reaction #(when @transfer? (shares-held @trade)))
        price (make-reaction #(->trade-price @trade))
        commodities (r/cursor page-state [:commodities])]
    (fn []
      [:form#trade-form
       {:no-validate true
        :on-submit (fn [e]
                     (.preventDefault e)
                     (drop-hidden-field-rules trade)
                     (v/validate trade)
                     (when (v/valid? trade)
                       (+busy)
                       (if @transfer?
                         (transfers/create (trade->transfer @trade)
                                           :callback -busy
                                           :on-success on-save)
                         (trading/create @trade
                                         :callback -busy
                                         :on-success on-save))))}
       [forms/date-field trade [:trade/date] {:validations #{::v/required}}]
       (when-not @dividend?
         [forms/select-field
          trade
          [:trade/action]
          (map (juxt name humanize)
               [:buy :sell :transfer])
          {:transform-fn keyword
           :validations #{::v/required}}])
       (if @transfer?
         [:<>
          [forms/decimal-field trade [:trade/shares] {:validations #{::v/required}}]
          (when @held
            [:p#shares-held.form-text.mt-n2
             "Shares held: "
             (format-decimal @held {:fraction-digits 4})])
          [forms/typeahead-field
           trade
           [:trade/to-account]
           {:search-fn (fn [input callback]
                         (->> @accounts
                              (filter transfer-destination?)
                              (remove #(util/entity= % (:trade/account @trade)))
                              (find-by-path input)
                              callback))
            :caption-fn (comp (partial string/join "/") :account/path)
            :find-fn (fn [{:keys [id]} callback]
                       (callback (@accounts-by-id id)))
            :validations #{::v/required}}]]
         [:<>
          [:div.row
           [:div.col-md-4
            [forms/decimal-field trade [:trade/shares] {:validations #{::v/required}}]]
           [:div.col-md-4
            [forms/decimal-field trade [:trade/value] {:validations #{::v/required}}]]
           [:div.col-md-4.d-flex.flex-column
            [:span.mb-2 "Est. Price"]
            [:span.mb-3.ms-3 (when @price (format-decimal @price))]]]
          [:div.row
           [:div.col-md-4
            [forms/decimal-field trade [:trade/fee]]]
           [:div.col-md-4
            [forms/typeahead-field
             trade
             [:trade/fee-account]
             {:search-fn (fn [input callback]
                           (->> @accounts
                                (find-by-path input)
                                callback))
              :caption-fn (comp (partial string/join "/") :account/path)
              :find-fn (fn [{:keys [id]} callback]
                         (callback (@accounts-by-id id)))}]]
           [:div.col-md-4.d-flex.align-items-end.checkbox-beside-input
            [forms/checkbox-field trade [:trade/value-includes-fee?] {:caption "Value includes fee"}]]]])
       [forms/typeahead-field
        trade
        [:trade/commodity]
        {:search-fn (fn [input callback]
                      (callback (cmdts/search input (vals @commodities))))
         :caption-fn cmdts/description
         :value-fn :id
         :find-fn (fn [id callback]
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
           :validations #{::v/required}}])
       [forms/checkbox-field
        page-state
        [:trade-repeat?]
        {:caption "Save and add another"}]])))
