(ns clj-money.views.transactions.page
  "Pages for entering a new transaction (/transactions/new) or editing one
  (link to it with path), independent of the page they're opened from. Once
  the transaction is saved, deleted, or the edit cancelled, they return to
  the page they were opened from. Saving a transaction awaiting review
  accepts it; one read from a receipt is shown beside the receipt image,
  and can be rejected instead."
  (:require [clojure.string :as string]
            [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [cljs-time.core :as t]
            [dgknght.app-lib.dom :refer [set-focus]]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.forms-validation :as v]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [clj-money.icons :refer [icon-with-text]]
            [clj-money.object-url :as obj-url]
            [clj-money.state :refer [app-state
                                     current-entity
                                     accounts-by-id
                                     +busy
                                     -busy]]
            [clj-money.cached-accounts :refer [when-accounts-loaded]]
            [clj-money.api.transactions :as transactions]
            [clj-money.api.receipt-ingestions :as ri]
            [clj-money.pending-review :as pending-review]
            [clj-money.views.attachments :as atts-view]
            [clj-money.views.transactions.form :as tf]))

(defn path
  "Returns the path of the page for the transaction. With :account-id, the
  transaction is shown from the perspective of that account, if it's one of
  the transaction's, when it can be shown in simplified form."
  [{:keys [id]} & {:keys [account-id]}]
  (cond-> (str "/transactions/" (js/encodeURIComponent id))
    account-id (str "?account-id=" (js/encodeURIComponent account-id))))

(defn- perspective-account
  "The account the transaction is shown from: the preferred one, if the
  transaction has an item for it, otherwise the account of its first item."
  [{:transaction/keys [items]} preferred-account-id]
  (let [account-ids (map (comp :id :transaction-item/account) items)]
    ; the preferred id comes from the query string, so it's a string
    (@accounts-by-id (or (some #(when (= preferred-account-id (str %)) %)
                               account-ids)
                         (first account-ids)))))

(defn- load-receipt-image
  [page-state {:receipt-ingestion/keys [image]}]
  (when image
    (atts-view/fetch-image
      image
      (fn [blob]
        ; a receipt emailed as a PDF isn't shown
        (when (string/starts-with? (.-type blob) "image/")
          (swap! page-state assoc :receipt-image-url (obj-url/create blob)))))))

(defn- load-ingestion
  "Loads the receipt-ingestion a transaction awaiting review was read from,
  if it was read from a receipt, and the receipt image, so the user can
  compare them, and reject the transaction if it's wrong."
  [page-state {:transaction/keys [review-status] :as trx}]
  (when (= :pending review-status)
    (ri/select-for-transactions
      [trx]
      :on-success (fn [[ingestion]]
                    (when ingestion
                      (swap! page-state assoc :ingestion ingestion)
                      (load-receipt-image page-state ingestion))))))

(defn- load-transaction
  [page-state id preferred-account-id]
  (+busy)
  (transactions/get
    {:id id}
    :callback -busy
    :on-success (fn [trx]
                  (when-accounts-loaded
                    (fn []
                      (let [account (perspective-account trx preferred-account-id)]
                        (swap! page-state assoc
                               :account account
                               :transaction (tf/prepare-for-edit trx account))
                        (load-ingestion page-state trx)
                        (set-focus "transaction-date")))))))

(defn- return
  "Returns to the page the transaction was opened from, or to the home page
  if it was opened directly"
  []
  (if (< 1 (.-length js/history))
    (.back js/history)
    ; not accountant, which can't be loaded by the client tests
    (.assign js/location "/")))

(defn- reject
  "Rejects the transaction read from a receipt, which deletes it and records
  the reason on the receipt-ingestion"
  [page-state]
  (let [{:keys [ingestion] {:keys [reason]} :rejection} @page-state]
    (+busy)
    (ri/reject ingestion
               reason
               :callback -busy
               :on-success (fn [_]
                             (pending-review/load-count)
                             (return)))))

(defn- rejection
  "Offers to reject a transaction read from a receipt, with a reason, rather
  than deleting it, so the reason is kept with the receipt."
  [page-state]
  (let [ingestion (r/cursor page-state [:ingestion])
        rejection (r/cursor page-state [:rejection])]
    (fn []
      (when @ingestion
        [:div.mt-3.border-top.pt-3
         (if @rejection
           [:form {:no-validate true
                   :on-submit (fn [e]
                                (.preventDefault e)
                                (v/validate rejection)
                                (when (v/valid? rejection)
                                  (reject page-state)))}
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
             (icon-with-text :arrow-left-short "Back")]]
           [:button.btn.btn-outline-danger
            {:type :button
             :title "Click here to reject the transaction read from the receipt."
             :on-click #(swap! page-state assoc :rejection {})}
            (icon-with-text :x "Reject")])]))))

(defn- receipt-image
  [page-state]
  (when-let [url (:receipt-image-url @page-state)]
    [:div.card
     [:div.card-header [:strong "Receipt"]]
     [:img.card-img-bottom {:src url
                            :alt "The receipt the transaction was read from"}]]))

(defn- page
  [id {:keys [account-id]}]
  (let [page-state (r/atom {})
        transaction (r/cursor page-state [:transaction])
        account (r/cursor page-state [:account])]
    (load-transaction page-state id account-id)
    (fn []
      [:div.row.mt-3
       [:div.col-lg-8
        (if @transaction
          [:<>
           [tf/transaction-form transaction
            :account @account
            :on-save return
            :on-cancel return
            :on-delete return]
           [rejection page-state]]
          [:div.d-flex.justify-content-center
           (bs/spinner)])]
       [:div.col-lg-4.mt-3.mt-lg-0
        [receipt-image page-state]]])))

(defn- track-account
  "Keeps the account last chosen in the simplified form in :account, so the
  form can be simplified again after it's expanded (which drops it from the
  transaction)."
  [page-state]
  (add-watch page-state
             ::account
             (fn [_ _ _ {:keys [transaction account]}]
               (when-let [chosen (:transaction/account transaction)]
                 (when-not (= chosen account)
                   (swap! page-state assoc :account chosen))))))

(defn- new-page []
  (let [page-state (r/atom {})
        transaction (r/cursor page-state [:transaction])
        account (r/cursor page-state [:account])]
    (track-account page-state)
    ; the entity and its accounts may still be loading when the page is
    ; loaded directly
    (when-accounts-loaded
      #(swap! page-state
              assoc
              :transaction
              #:transaction{:entity @current-entity
                            :transaction-date (t/today)
                            :account nil}))
    (fn []
      [:div.row.mt-3
       [:div.col-lg-8
        (if @transaction
          [tf/transaction-form transaction
           :account @account
           :choose-account? true
           :on-save return
           :on-cancel return]
          [:div.d-flex.justify-content-center
           (bs/spinner)])]])))

; Defined before the route for an existing transaction, which would
; otherwise take "new" as its id: secretary uses the first route that
; matches.
(secretary/defroute "/transactions/new" []
  (swap! app-state assoc
         :page #'new-page
         :active-nav :add))

; A regex route, because secretary (1.2.3) fails to extract the params of a
; route like "/transactions/:id" under the current ClojureScript. Dispatch
; appends {:query-params ...}, if there are any, to the captured groups. (The
; bindings must be plain symbols: defroute also uses them as map keys.)
(secretary/defroute #"/transactions/([^/?#]+)" [id query]
  (swap! app-state assoc
         :page #(vector page (js/decodeURIComponent id) (:query-params query))
         :active-nav nil))
