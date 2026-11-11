(ns clj-money.views.accounts.filter
  "Options that filter the accounts list, in an offcanvas opened by
  filter-toggle.

  The caller owns an atom (or cursor) holding the options,
  :hide-zero-balances?, :show-hidden? and :tags (the names of the tags whose
  accounts are shown, or \"_untagged\"), and renders
  [account-filter options]. account-filter-fn turns the options into a
  predicate for the accounts to show."
  (:require [clojure.set :refer [intersection]]
            [reagent.ratom :refer [make-reaction]]
            [dgknght.app-lib.forms :as forms]
            [clj-money.icons :refer [icon]]
            [clj-money.state :refer [accounts]]))

(defn account-filter-fn
  "Returns a predicate for the accounts the options leave in the list, apart
  from those with zero balances"
  [{:keys [tags show-hidden?]}]
  (let [tags (set (map keyword tags))
        tag-match? (cond
                     (tags :_untagged) (comp empty? :account/user-tags)
                     (seq tags) (comp seq
                                      (partial intersection tags)
                                      :account/user-tags)
                     :else (constantly true))]
    (every-pred tag-match?
                (fn [account]
                  (or show-hidden?
                      (not (:account/hidden account)))))))

(defn filter-toggle []
  [:button.btn.btn-outline-secondary
   {:type :button
    :data-bs-toggle "offcanvas"
    :data-bs-target "#account-filter"
    :aria-controls "account-filter"}
   (icon :funnel :size :small)])

(defn- filter-options
  [options]
  (let [tag-items (make-reaction #(->> @accounts
                                       (mapcat :account/user-tags)
                                       set
                                       (map name)
                                       (cons [:_untagged "untagged"])))]
    (fn []
      [:div.accounts-options
       [:div.form-check.mb-1
        [forms/checkbox-input
         options
         [:hide-zero-balances?]
         {:caption "Hide Zero-Balance Accounts"}]
        [:label.form-check-label {:for "hide-zero-balances"}
         "Hide zero balances"]]
       [:div.form-check.mb-1
        [forms/checkbox-input
         options
         [:show-hidden?]
         {:caption "Show Hidden Accounts"}]
        [:label.form-check-label {:for "show-hidden"}
         "Show hidden accounts"]]
       [forms/checkbox-inputs
        options
        [:tags]
        tag-items
        {:container-html {:class ["d-flex flex-column"]}
         :input-container-html {:class "mb-1"}}]])))

(defn account-filter
  [options]
  [:div#account-filter.offcanvas.offcanvas-end {:tab-index -1}
   [:div.offcanvas-header
    [:h3.off-canvas-title "Filter"]
    [:button.btn-close.text-reset {:data-bs-dismiss "offcanvas"
                                   :aria-label "Close"}]]
   [:div.offcanvas-body [filter-options options]]])
