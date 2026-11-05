(ns clj-money.views.transactions.form
  "The form for entering or editing a transaction, usable from any page.

  The caller owns an atom (or cursor) holding the transaction being edited,
  prepared with prepare-for-edit (or, for a new one, an accountified
  transaction), and renders transaction-form with it. The form saves or
  deletes the transaction itself and reports the outcome through callbacks;
  what happens next (closing the form, reloading a list, navigating away) is
  up to the caller."
  (:require [clojure.string :as string]
            [clojure.zip :as zip]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [dgknght.app-lib.core :refer [parse-int]]
            [dgknght.app-lib.web :refer [format-decimal]]
            [dgknght.app-lib.dom :refer [set-focus
                                         key-code
                                         shift-key?]]
            [dgknght.app-lib.html :refer [space
                                          special-char]]
            [dgknght.app-lib.decimal :as decimal]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.forms-validation :as v]
            [clj-money.icons :refer [icon
                                     icon-with-text]]
            [clj-money.state :refer [accounts
                                     accounts-by-id
                                     +busy
                                     -busy]]
            [clj-money.accounts :refer [find-by-path]]
            [clj-money.transactions :refer [accountify
                                            unaccountify
                                            accountified?
                                            can-accountify?
                                            entryfy
                                            unentryfy
                                            ensure-empty-item
                                            ->unilateral
                                            ->bilateral]]
            [clj-money.api.transactions :as transactions]
            [clj-money.pending-review :as pending-review]))

(defn- supply-accounts
  [items]
  (map #(update-in %
                   [:transaction-item/account]
                   (comp @accounts-by-id
                         :id))
       items))

(defn prepare-for-edit
  "Prepares a transaction, as retrieved from the server, for the form. It's
  shown in simplified form, from the perspective of the given account, if it
  can be, or with all of its items otherwise."
  ([account]
   #(prepare-for-edit % account))
  ([trx account]
   (let [prepare (if (and account (can-accountify? trx))
                   (accountify account)
                   entryfy)]
     (-> trx
         (update-in [:transaction/items] supply-accounts)
         prepare))))

(defn- accept-review
  "Saving a transaction awaiting review, like one read from a statement,
  accepts it"
  [{:transaction/keys [review-status] :as trx}]
  (cond-> trx
    (= :pending review-status) (assoc :transaction/review-status :accepted)))

(defn- save-transaction
  "Saves the transaction, accepting it if it's awaiting review, in which
  case the count of transactions awaiting review is refreshed"
  [trx on-save]
  (let [reviewed? (= :pending (:transaction/review-status trx))]
    (+busy)
    (transactions/save (accept-review trx)
                       :callback -busy
                       :on-success (fn [saved]
                                     (when reviewed? (pending-review/load-count))
                                     (when on-save (on-save saved))))))

(defn- delete-transaction
  "Deletes the transaction, after confirmation. If it was awaiting review,
  the count of those is refreshed."
  [{:transaction/keys [review-status] :as trx} on-delete]
  (when (js/confirm "Are you sure you want to delete this transaction?")
    (+busy)
    (transactions/delete
      trx
      :callback -busy
      :on-success (fn []
                    (when (= :pending review-status)
                      (pending-review/load-count))
                    (when on-delete (on-delete trx))))))

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
  [transaction index item-count]
  (let [item (r/cursor transaction [:transaction/items index])
        reconciled? (boolean (:transaction-item/reconciliation @item))
        ensure-entry-state #(swap! transaction ensure-empty-item)]
    ^{:key (str "item-form-" index)}
    [:tr
     [:td.text-center
      (when reconciled?
        [:span {:title "This item has been reconciled and cannot be changed."}
         (icon :lock-fill :size :small)])]
     [:td [forms/typeahead-input
           item
           [:transaction-item/account]
           {:search-fn (fn [input callback]
                         (callback (find-by-path input @accounts)))
            :on-change ensure-entry-state
            :caption-fn #(string/join "/" (:account/path %))
            :find-fn (fn [{:keys [id]} callback]
                       (callback (@accounts-by-id id)))
            :html {:id (str "account-id-" index)
                   :on-key-up #(item-navigate % item-count)
                   :disabled reconciled?}}]]
     [:td [forms/text-input
           item
           [:transaction-item/memo]
           {:on-change ensure-entry-state
            :html {:on-key-up #(item-navigate % item-count)
                   :id (str "memo-" index)}}]]
     [:td [forms/decimal-input
           item
           [:transaction-item/credit-quantity]
           {:fraction-digits 2
            :on-accept ensure-entry-state
            :disabled-fn (constantly reconciled?)
            :html {:on-key-up #(item-navigate % item-count)
                   :on-key-down #(when (arrow-key? %) (.preventDefault %))
                   :id (str "credit-quantity-" index)}}]]
     [:td [forms/decimal-input
           item
           [:transaction-item/debit-quantity]
           {:fraction-digits 2
            :on-accept ensure-entry-state
            :disabled-fn (constantly reconciled?)
            :html {:id (str "debit-quantity-" index)
                   :on-key-up #(item-navigate % item-count)
                   :on-key-down #(when (arrow-key? %) (.preventDefault %))}}]]]))

(defn- full-transaction-form
  [transaction _on-save]
  (let [total-credits (make-reaction #(->> (:transaction/items @transaction)
                                           (map :transaction-item/credit-quantity)
                                           (reduce decimal/+ 0M)))
        total-debits (make-reaction #(->> (:transaction/items @transaction)
                                          (map :transaction-item/debit-quantity)
                                          (reduce decimal/+ 0M)))
        correction (make-reaction #(decimal/abs (- @total-debits @total-credits)))
        item-count (make-reaction #(count (:transaction/items @transaction)))]
    (fn [_transaction on-save]
      [:div
       [:div.alert.alert-warning.d-md-none "Please use a larger screen to use full transaction mode."]
       [:form#transaction-form.d-none.d-md-block
        {:no-validate true
         :on-submit (fn [e]
                      (.preventDefault e)
                      (v/validate transaction)
                      (when (v/valid? transaction)
                        (save-transaction (unentryfy @transaction) on-save)))}
        [forms/date-field transaction [:transaction/transaction-date] {:validations #{::v/required}}]
        [forms/text-field transaction [:transaction/description] {:validations #{::v/required}}]
        [:table.table
         [:thead
          [:tr
           [:td (space)]
           [:td "Account"]
           [:td "Memo"]
           [:td "Credit Amount"]
           [:td "Debit Amount"]]]
         [:tbody
          (doall (for [index (range @item-count)]
                   (item-input-row transaction index @item-count)))]
         [:tfoot
          [:tr
           [:td {:col-span 3} (special-char :nbsp)]
           [:td.text-end (format-decimal @total-credits)]
           [:td.text-end.d-flex.flex-row-reverse.justify-content-between
            (format-decimal @total-debits)
            (when-not (decimal/zero? @correction)
              [:span.text-danger.me-3
               (special-char :pm)
               (format-decimal @correction)])]]]]]])))

(defn- simple-transaction-form
  [transaction on-save]
  [:form#transaction-form
   {:no-validate true
    :on-submit (fn [e]
                 (.preventDefault e)
                 (v/validate transaction)
                 (when (v/valid? transaction)
                   (-> @transaction
                       unaccountify
                       ->unilateral
                       (save-transaction on-save))))}
   [forms/date-field
    transaction
    [:transaction/transaction-date]
    {:validations #{::v/required}}]
   [forms/text-field transaction [:transaction/description] {:validations #{::v/required}}]
   [forms/text-field transaction [:transaction/item :account-item/memo]]
   [forms/decimal-field transaction [:transaction/quantity] {:fraction-digits 2
                                                             :validations #{::v/required}}]
   [forms/typeahead-field
    transaction
    [:transaction/other-account]
    {:search-fn (fn [input callback]
                  (callback (find-by-path input @accounts)))
     :caption-fn #(string/join "/" (:account/path %))
     :find-fn (fn [{:keys [id]} callback]
                (callback (@accounts-by-id id)))}]])

(defn- expand
  [trx]
  (-> trx
      v/reset
      unaccountify
      ->unilateral
      entryfy))

(defn- collapse
  [trx account]
  (-> trx
      v/reset
      unentryfy
      ->bilateral
      (accountify account)))

(defn- mode-toggle
  [transaction account]
  (cond
    (accountified? @transaction)
    [:button.btn.btn-secondary {:title "Click here to show full transaction details."
                                :on-click #(swap! transaction expand)}
     (icon :arrows-expand)]

    (and account
         (can-accountify? (->bilateral (unentryfy @transaction))))
    [:button.btn.btn-secondary {:title "Click here to simplify transaction entry."
                                :on-click #(swap! transaction collapse account)}
     (icon :arrows-collapse)]))

(defn transaction-form
  "Renders the form for the transaction in the given atom (or cursor), which
  must be prepared with prepare-for-edit. Renders nothing while it's empty.

  Options:
    :account   - the account from whose perspective the simplified form shows
                 the transaction; without it, the form can be expanded but
                 not simplified
    :on-save   - called with the saved transaction
    :on-cancel - called when the user cancels the edit
    :on-delete - called with the deleted transaction"
  [transaction & {:keys [account on-save on-cancel on-delete]}]
  (when @transaction
    [:<>
     [:div.d-flex.justify-content-between
      [:h3 (if (:id @transaction)
             "Edit Transaction"
             "New Transaction")]
      [mode-toggle transaction account]]
     [:div.mt-3
      (if (accountified? @transaction)
        [simple-transaction-form transaction on-save]
        [full-transaction-form transaction on-save])]
     [:div
      [:button.btn.btn-primary
       {:type :submit
        :form "transaction-form"
        :title "Click here to save the transaction"}
       (icon-with-text :check "Save")]
      [:button.btn.btn-secondary.ms-2
       {:on-click #(when on-cancel (on-cancel))
        :title "Click here to cancel this transaction"}
       (icon-with-text :x "Cancel")]
      (when (:id @transaction)
        [:button.btn.btn-danger.ms-2
         {:on-click #(delete-transaction @transaction on-delete)
          :title "Click here to delete this transaction"}
         (icon-with-text :x-circle "Delete")])]]))
