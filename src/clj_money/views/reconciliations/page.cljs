(ns clj-money.views.reconciliations.page
  "The Reconciliations page, /reconciliations. Lists the entity's
  reconciliations, latest first, a page at a time, optionally limited to
  one account (chosen in the filter, or given as ?account-id=) and to those
  ending within a range of dates. An unfinished reconciliation
  links to the accounts page, where it's completed. Uploading a statement
  starts the reconciliation of the account it's for."
  (:require [clojure.string :as string]
            [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [cljs-time.core :as t]
            [accountant.core :as accountant]
            [dgknght.app-lib.web :refer [format-date
                                         format-decimal]]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [dgknght.app-lib.notifications :as notify]
            [dgknght.app-lib.core :refer [index-by]]
            [clj-money.util :refer [id=]]
            [clj-money.icons :refer [icon icon-with-text]]
            [clj-money.state :refer [app-state
                                     current-entity
                                     accounts
                                     accounts-by-id]]
            [clj-money.accounts :refer [find-by-path
                                        system-tagged?]]
            [clj-money.reconciliations :refer [split-page]]
            [clj-money.cached-accounts :refer [when-accounts-loaded]]
            [clj-money.api.reconciliations :as recs]
            [clj-money.api.commodities :as commodities]
            [clj-money.views.attachments :refer [view-image]]
            [clj-money.views.accounts.page :refer [reconcile-path]]
            [clj-money.views.statement-upload :refer [statement-input
                                                      reading-indicator]]))

(def ^:private page-size 25)

(defn- criteria
  [{:keys [account start-date end-date status]} on-or-before]
  (let [on-or-before (or on-or-before end-date)]
    (cond-> {:desc :reconciliation/end-of-period}
      account (assoc :reconciliation/account account)
      status (assoc :reconciliation/status status)
      start-date (assoc :reconciliation/end-of-period-on-or-after start-date)
      on-or-before (assoc :reconciliation/end-of-period-on-or-before on-or-before))))

(defn- append
  "Adds the reconciliations to those shown. next-on-or-before is the end of
  period through which the next page is read, or nil if they've all been
  read."
  [page-state recons next-on-or-before]
  (swap! page-state
         #(-> %
              (update-in [:reconciliations] (fnil into []) recons)
              (assoc :next-on-or-before next-on-or-before)
              (dissoc :loading?))))

(defn- read-whole-date
  "Reads all of the reconciliations ending on the date, which are more than
  fit on a page, then leaves the next page to start the day before"
  [page-state date current?]
  (let [{:keys [start-date] :as fltr} (:applied-filter @page-state)
        day-before (t/minus date (t/days 1))]
    (recs/select (assoc (criteria fltr date)
                        :reconciliation/end-of-period-on-or-after date)
                 :on-error (fn [e]
                             (swap! page-state dissoc :loading?)
                             (notify/danger (str "Unable to retrieve the reconciliations: "
                                                 (ex-message e))))
                 :on-success (fn [recons]
                               (when (current?)
                                 (append page-state
                                         recons
                                         (when-not (and start-date
                                                        (t/before? day-before start-date))
                                           day-before)))))))

(defn- load-page
  "Reads the next page of reconciliations"
  [page-state]
  (let [{fltr :applied-filter :keys [next-on-or-before]} @page-state
        request-id (gensym "reconciliations")
        ; the response is ignored if another read started in the meantime,
        ; e.g., with a different filter
        current? #(= request-id (:request-id @page-state))]
    (swap! page-state assoc :loading? true :request-id request-id)
    (recs/select (assoc (criteria fltr next-on-or-before) :limit page-size)
                 :on-error (fn [e]
                             (swap! page-state dissoc :loading?)
                             (notify/danger (str "Unable to retrieve the reconciliations: "
                                                 (ex-message e))))
                 :on-success
                 (fn [recons]
                   (when (current?)
                     (let [{:keys [reconciliations next-on-or-before fill-date]}
                           (split-page recons page-size)]
                       (if fill-date
                         (read-whole-date page-state fill-date current?)
                         (append page-state reconciliations next-on-or-before))))))))

(defn- load-reconciliations
  "Reads the first page of reconciliations with the filter as it's been
  entered, replacing those shown"
  [page-state]
  (swap! page-state
         #(-> %
              ; only the values: the form's validation state is kept with
              ; the filter as it's entered
              (assoc :applied-filter (select-keys (:filter %)
                                                  [:account
                                                   :start-date
                                                   :end-date
                                                   :status]))
              (dissoc :reconciliations :next-on-or-before)))
  (load-page page-state))

(defn- load-commodities
  "Loads the entity's commodities, for the symbols of commodity accounts"
  [page-state]
  (commodities/select {}
                      :on-success #(swap! page-state
                                          assoc
                                          :commodities
                                          (index-by :id %))))

(defn- account-name
  [account]
  (string/join "/" (:account/path account)))

(defn- short-name
  "The symbol of the commodity held in a tradable account (e.g., VTI),
  otherwise the account's name, without its parents"
  [{:account/keys [name commodity] :as account} commodities]
  (or (when (system-tagged? account :tradable)
        (get-in commodities [(:id commodity) :commodity/symbol]))
      name))

(defn- account-caption
  "The account, compactly: on a small screen, only its short name; on a
  larger one, its name beneath the path of its parents"
  [{:account/keys [path name] :as account} commodities]
  [:<>
   (when-let [parents (seq (butlast path))]
     [:div.small.text-body-secondary.d-none.d-sm-block
      (string/join "/" parents)])
   [:span.d-sm-none (short-name account commodities)]
   [:span.d-none.d-sm-inline name]])

(defn- reconciliation-row
  [{:reconciliation/keys [end-of-period balance status statement account] :as recon}
   commodities]
  ; accounts-by-id is nil until the accounts are loaded, e.g., when the
  ; page is loaded directly, so the name is filled in once they are
  (let [caption (some-> (get @accounts-by-id (:id account))
                        (account-caption commodities))
        completed? (= :completed status)]
    ^{:key (str "reconciliation-row-" (:id recon))}
    [:tr.align-middle
     [:td (format-date end-of-period)]
     [:td (if completed?
            caption
            ; navigation to a link within the app is handled by accountant
            [:a {:href (reconcile-path account)
                 :title "Click here to continue this reconciliation."}
             (or caption "Continue")])]
     [:td.text-end (format-decimal balance {:fraction-digits 2})]
     [:td.d-none.d-sm-table-cell
      (if completed?
        [:span.badge.rounded-pill.text-bg-success "Completed"]
        [:span.badge.rounded-pill.text-bg-warning "In progress"])]
     [:td.text-center
      (when statement
        [:button.btn.btn-sm.btn-outline-primary
         {:title "Click here to view the statement in a new tab."
          :on-click #(view-image statement)}
         (icon :file-earmark-pdf :size :small)])]]))

(defn- load-more
  [page-state]
  (let [more? (r/cursor page-state [:next-on-or-before])
        loading? (r/cursor page-state [:loading?])
        recons (r/cursor page-state [:reconciliations])]
    (fn []
      (cond
        ; the table shows its own spinner until the first page is read
        (nil? @recons)
        nil

        @loading?
        [:div.d-flex.justify-content-center.mb-3 (bs/spinner {:size :small})]

        @more?
        [:div.d-flex.justify-content-center.mb-3
         [:button.btn.btn-outline-secondary
          {:type :button
           :title "Click here to show earlier reconciliations."
           :on-click #(load-page page-state)}
          "Load more"]]))))

(defn- reconciliations-table
  [page-state]
  (let [recons (r/cursor page-state [:reconciliations])]
    (fn []
      (cond
        (nil? @recons)
        [:div.d-flex.justify-content-center (bs/spinner)]

        (and (empty? @recons)
             (not (:next-on-or-before @page-state)))
        [:p.text-body-secondary "There are no reconciliations."]

        :else
        [:table.table.table-hover
         [:thead
          [:tr
           [:th "End of Period"]
           [:th "Account"]
           [:th.text-end "Balance"]
           [:th.d-none.d-sm-table-cell "Status"]
           ; on a small screen, the statement buttons need no heading
           [:th.text-center [:span.d-none.d-sm-inline "Statement"]]]]
         [:tbody
          (->> @recons
               (map #(reconciliation-row % (:commodities @page-state)))
               doall)]]))))

(defn- filter-toggle []
  [:button.btn.btn-outline-secondary
   {:type :button
    :title "Click here to filter the reconciliations."
    :data-bs-toggle "offcanvas"
    :data-bs-target "#reconciliation-filter"
    :aria-controls "reconciliation-filter"}
   (icon :funnel :size :small)])

(defn- reconcilable?
  [{:account/keys [type]}]
  (#{:asset :liability} type))

(defn- reconciliation-filter
  "The filter, which is applied, and the reconciliations read with it, when
  the user clicks Refresh"
  [page-state options]
  [:div#reconciliation-filter.offcanvas.offcanvas-end {:tab-index -1}
   [:div.offcanvas-header
    [:h3.off-canvas-title "Filter"]
    [:button.btn-close.text-reset {:data-bs-dismiss "offcanvas"
                                   :aria-label "Close"}]]
   [:div.offcanvas-body
    [forms/typeahead-field
     options
     [:account]
     {:caption "Account"
      :search-fn (fn [input callback]
                   (->> @accounts
                        (filter reconcilable?)
                        (find-by-path input)
                        callback))
      :caption-fn account-name
      :value-fn identity
      :find-fn (fn [current callback] (callback current))}]
    [forms/select-field
     options
     [:status]
     [["" "All"]
      [:new "In progress"]
      [:completed "Completed"]]
     {:caption "Status"
      :transform-fn keyword}]
    [forms/date-field options [:start-date] {:caption "Ending on or after"}]
    [forms/date-field options [:end-date] {:caption "Ending on or before"}]
    [:div.d-flex.gap-2
     [:button.btn.btn-primary
      {:type :button
       :title "Click here to show the reconciliations that match the filter."
       :data-bs-dismiss "offcanvas"
       :on-click #(load-reconciliations page-state)}
      (icon-with-text :arrow-clockwise "Refresh")]
     [:button.btn.btn-secondary
      {:type :button
       :title "Click here to clear the filter. Click Refresh to apply it."
       :on-click #(reset! options {})}
      "Clear"]]]])

(defn- filter-by-account-id
  "Limits the list to the account with the id, which comes from the query
  string, so it's a string"
  [page-state id]
  (when-accounts-loaded
    (fn []
      (when-let [account (some #(when (= id (str (:id %))) %)
                               @accounts)]
        (swap! page-state assoc-in [:filter :account] account))
      (load-reconciliations page-state))))

(def ^:private status-captions
  {:new "In progress"
   :completed "Completed"})

(defn- filter-description
  [{:keys [start-date end-date status]}]
  (let [dates (cond
                (and start-date end-date) (str "ending " (format-date start-date)
                                               " through " (format-date end-date))
                start-date (str "ending on or after " (format-date start-date))
                end-date (str "ending on or before " (format-date end-date)))
        description (->> [(status-captions status) dates]
                         (filter identity)
                         (string/join ", "))]
    (when (seq description)
      (str (string/upper-case (subs description 0 1))
           (subs description 1)))))

(defn- index
  [& [{:keys [account-id]}]]
  (let [page-state (r/atom {})
        options (r/cursor page-state [:filter])
        applied (r/cursor page-state [:applied-filter])
        account (r/cursor applied [:account])
        reading (r/atom nil)]
    (load-commodities page-state)
    (if account-id
      (filter-by-account-id page-state account-id)
      (load-reconciliations page-state))
    (add-watch current-entity
               ::index
               (fn [_ _ previous current]
                 ; saving the entity's settings shouldn't reload the page
                 (when-not (id= previous current)
                   (reset! options {})
                   (load-commodities page-state)
                   (load-reconciliations page-state))))
    (fn [& _]
      [:div.mt-3
       [:div.d-flex.justify-content-between.align-items-center
        [:h1.me-auto (str "Reconciliations"
                          (when @account
                            (str " - " (:account/name @account))))]
        [filter-toggle]]
       (when-let [description (filter-description @applied)]
         [:p.text-body-secondary description])
       [reconciliation-filter page-state options]
       [reconciliations-table page-state]
       [load-more page-state]
       [:div.d-flex.gap-2
        [statement-input reading
         :caption "Upload a statement"
         :on-complete #(accountant/navigate! (reconcile-path %))]
        (when @account
          [:a.btn.btn-secondary
           {:href (reconcile-path @account)
            :title "Click here to reconcile this account without a statement."}
           (icon-with-text :list-check "Reconcile")])]
       [reading-indicator reading]])))

(secretary/defroute "/reconciliations" [query-params]
  (swap! app-state assoc
         :page #(vector index query-params)
         :active-nav :reconciliations))
