(ns clj-money.views.transactions.page
  "A page for editing one transaction, independent of the page it's opened
  from. Link to it with path. Once the transaction is saved, deleted, or the
  edit cancelled, it returns to the page it was opened from."
  (:require [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [dgknght.app-lib.dom :refer [set-focus]]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [clj-money.state :refer [app-state
                                     accounts-by-id
                                     +busy
                                     -busy]]
            [clj-money.api.transactions :as transactions]
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

(defn- when-accounts-loaded
  "Calls f once the cached accounts are loaded. They may still be loading,
  e.g., when the page is loaded directly."
  [f]
  (if (seq (:accounts @app-state))
    (f)
    ; app-state rather than the accounts cursor, which only notifies its
    ; watches when it's dereferenced
    (add-watch app-state
               ::accounts-loaded
               (fn [_ _ _ {:keys [accounts]}]
                 (when (seq accounts)
                   (remove-watch app-state ::accounts-loaded)
                   (f))))))

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
                        (set-focus "transaction-date")))))))

(defn- return
  "Returns to the page the transaction was opened from, or to the home page
  if it was opened directly"
  []
  (if (< 1 (.-length js/history))
    (.back js/history)
    ; not accountant, which can't be loaded by the client tests
    (.assign js/location "/")))

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
          [tf/transaction-form transaction
           :account @account
           :on-save return
           :on-cancel return
           :on-delete return]
          [:div.d-flex.justify-content-center
           (bs/spinner)])]])))

; A regex route, because secretary (1.2.3) fails to extract the params of a
; route like "/transactions/:id" under the current ClojureScript. Dispatch
; appends {:query-params ...}, if there are any, to the captured groups. (The
; bindings must be plain symbols: defroute also uses them as map keys.)
(secretary/defroute #"/transactions/([^/?#]+)" [id query]
  (swap! app-state assoc
         :page #(vector page (js/decodeURIComponent id) (:query-params query))
         :active-nav nil))
