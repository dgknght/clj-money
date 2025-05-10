(ns clj-money.views.scheduled
  (:require [cljs.pprint :refer [pprint]]
            [clojure.string :as string]
            [cljs-time.core :as t]
            [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [camel-snake-kebab.core :as csk]
            [dgknght.app-lib.core :as lib]
            [dgknght.app-lib.web :refer [format-date
                                         format-decimal]]
            [dgknght.app-lib.inflection :refer [title-case]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.dom :refer [set-focus]]
            [dgknght.app-lib.calendar :as calendar]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.decimal :as decimal]
            [dgknght.app-lib.notifications :as notify]
            [dgknght.app-lib.bootstrap-5 :as bs]
            [clj-money.icons :refer [icon
                                     icon-with-text]]
            [clj-money.state :refer [app-state
                                     current-entity
                                     accounts
                                     accounts-by-id
                                     +busy
                                     -busy
                                     busy?]]
            [clj-money.accounts :refer [find-by-path]]
            [clj-money.scheduled-transactions :refer [next-transaction-date
                                                      pending? ]]
            [clj-money.api.scheduled-transactions :as sched-trans]))

(defonce ^:private auto-loaded (atom []))

(defn- map-next-occurrence
  [sched-trans]
  (map #(assoc %
               :scheduled-transaction/next-occurrence
               (next-transaction-date %))
       sched-trans))

(defn- load-sched-trans
  [page-state]
  (+busy)
  (sched-trans/select {}
                      :callback -busy
                      :on-success #(swap! page-state
                                          assoc
                                          :scheduled-transactions
                                          (map-next-occurrence %))))

(defn set-next-occurrence
  [sched-tran]
  (assoc sched-tran :scheduled-transaction/next-occurrence (next-transaction-date sched-tran)))

(defn- update-sched-trans
  [state transactions]
  (let [mapped-trans (lib/index-by (comp :id
                                         :transaction/scheduled-transaction)
                                   transactions)]
    (update-in state
               [:scheduled-transactions]
               (fn [sched-trans]
                 (map (fn [sched-tran]
                        (if-let [trans (get-in mapped-trans [(:id sched-tran)])]
                          (-> sched-tran
                              (assoc :scheduled-transaction/last-occurrence
                                     (:transaction/transaction-date trans))
                              set-next-occurrence)
                          sched-tran))
                      sched-trans)))))

(defn- realize
  ([page-state] (realize nil page-state))
  ([sched-tran page-state]
   (+busy)
   (let [on-success (fn [result]
                      (swap! page-state #(-> %
                                             (update-sched-trans result)
                                             (update-in [:created] (fnil concat []) result)))
                      (notify/toast "Success" (if (empty? result)
                                                "No transactions are ready to be created."
                                                "The scheduled transactions where created")))]
     (if sched-tran
       (sched-trans/realize sched-tran
                            :callback -busy
                            :on-success on-success)
       (sched-trans/realize :callback -busy
                            :on-success on-success)))))

(defn- delete-sched-tran
  [_sched-tran _page-state]
  (js/alert "Not implemented"))

(defn- expired?
  [{:scheduled-transaction/keys [end-date]}]
  (and end-date (t/after? (t/today) end-date)))

(def ^:private disabled? (complement :scheduled-transaction/enabled))

(def ^:private active? (complement (some-fn expired? disabled?)))

(defn- ->editable-item
  [{:scheduled-transaction-item/keys [action quantity] :as item}]
  (cond-> (dissoc item
                  :scheduled-transaction-item/quantity
                  :scheduled-transaction-item/action)
    (= :debit action) (assoc :debit-quantity quantity)
    (= :credit action) (assoc :credit-quantity quantity)))

(defn- ->editable
  [sched-tran]
  (update-in sched-tran
             [:scheduled-transaction/items]
             (fn [items]
               (conj
                 (mapv ->editable-item items)
                 {}))))

(defn- sched-tran-row
  [{:scheduled-transaction/keys [description last-occurrence next-occurrence] :as sched-tran} page-state busy?]
  ^{:key (str "sched-tran-row-" (:id sched-tran))}
  [:tr {:class (cond-> []
                 (disabled? sched-tran) (conj "sched-tran-disabled")
                 (expired? sched-tran) (conj "sched-tran-expired"))}
   [:td description]
   [:td.d-none.d-md-table-cell (format-date last-occurrence)]
   [:td
    [:span.d-none.d-md-inline (format-date next-occurrence)]
    [:span.d-md-none (format-date (:scheduled-transaction/next-occurrence sched-tran) "M/d")]]
   [:td
    [:div.btn-group
     [:button.btn.btn-sm {:title "Click here to realize transactions for this schedule."
                          :class (if (pending? sched-tran)
                                   "btn-success"
                                   "btn-secondary")
                          :on-click #(realize sched-tran page-state)}
      (if busy?
        (bs/spinner {:size :small})
        (icon :lightning-fill :size :small))]
     [:button.btn.btn-secondary.btn-sm {:title "Click here to edit this scheduled transaction."
                                   :on-click (fn [_]
                                               (swap! page-state assoc :selected (->editable sched-tran))
                                               (set-focus "description"))}
      (icon :pencil :size :small)]
     [:button.btn.btn-danger.btn-sm {:title "Click here to remove this scheduled transaction."
                                     :on-click #(delete-sched-tran sched-tran page-state)}
      (icon :x :size :small)]]]])

(defn- date-compare
  [d1 d2]
  (t/before? (or d1 (t/epoch))
             (or d2 (t/epoch))))

(def ^:private table-headers
  [{:key :scheduled-transaction/description}
   {:key :scheduled-transaction/last-occurrence
    :caption "Last"
    :css ["d-none"
          "d-md-table-cell"]}
   {:key :scheduled-transaction/next-occurrence
    :caption "Next"}])

(defn- table-header
  [page-state]
  (let [sort-on (r/cursor page-state [:sort-on])]
    (fn []
      [:thead
       [:tr
        (->> table-headers
             (map (fn [h]
                    ^{:key (str "table-header-" (name (:key h)))}
                    [:th {:class (:css h)}
                     (title-case (or (:caption h)
                                     (name (:key h))))
                     [:a.ms-3 {:href "#"
                               :class (if (= (:key h) @sort-on)
                                          "text-dark"
                                          "text-muted")
                               :on-click #(reset! sort-on (:key h))}
                      (icon :sort-down-alt)]]))
             doall)
        [:th (html/space)]]])))

(defn- sched-trans-table
  [page-state]
  (let [sched-trans (r/cursor page-state [:scheduled-transactions])
        hide-inactive? (r/cursor page-state [:hide-inactive?])
        sort-on (r/cursor page-state [:sort-on])
        sort-fn (make-reaction #(#:scheduled-transaction{:next-occurrence date-compare
                                                         :last-occurrence date-compare
                                                         :description compare}
                                                                          @sort-on))
        filter-fn (make-reaction #(if @hide-inactive?
                                    active?
                                    (constantly true)))]
    (fn []
      [:table.table.table-hover
       [table-header page-state]
       [:tbody
        (if @sched-trans
          (->> @sched-trans
               (filter @filter-fn)
               (sort-by @sort-on @sort-fn)
               (map #(sched-tran-row % page-state @busy?))
               doall)
          [:tr
           [:td {:col-span 3}
            [:div.text-center
             (bs/spinner)]]])]])))

(defn- ->saveable-item
  [{:keys [debit-quantity credit-quantity] :as item}]
  (-> item
      (assoc :scheduled-transaction-item/action (if debit-quantity
                                                  :debit
                                                  :credit)
             :scheduled-transaction-item/quantity (or debit-quantity credit-quantity))
      (dissoc :debit-quantity :credit-quantity)))

(defn- empty-item?
  [{:keys [debit-quantity credit-quantity] :scheduled-transaction-item/keys [account]}]
  (and (nil? (:id account))
       (nil? debit-quantity)
       (nil? credit-quantity)))

(defn- ->saveable
  [sched-tran]
  (update-in sched-tran
             [:scheduled-transaction/items]
             (fn [items]
               (->> items
                    (remove empty-item?)
                    (mapv ->saveable-item)))))

(defn- save-sched-tran
  [page-state]
  (+busy)
  (-> (:selected @page-state)
      ->saveable
      (sched-trans/save :callback -busy
                        :on-success (fn [_]
                                      (load-sched-trans page-state)
                                      (swap! page-state dissoc :selected)))))

(defn- adj-items
  [page-state]
  (swap! page-state
         update-in
         [:selected]
         (fn [sched-tran]
           (update-in sched-tran
                      [:scheduled-transaction/items]
                      (fn [items]
                        (conj
                          (->> items
                               (remove empty-item?)
                               vec)
                          {:debit-quantity nil
                           :credit-quantity nil
                           :scheduled-transaction-item/account nil}))))))

(defn- item-row
  [index sched-tran page-state]
  ^{:key (str "item-row-" index)}
  [:tr
   [:td [forms/typeahead-input
         sched-tran
         [:scheduled-transaction/items index :scheduled-transaction-item/account :id]
         {:id (str "account-id-" index)
          :search-fn (fn [input callback]
                       (callback (find-by-path input @accounts)))
          ;:on-change #(ensure-entry-state page-state)
          ;:on-key-up #(item-navigate % item-count)
          :caption-fn #(string/join "/" (:account/path %))
          :value-fn :id
          :find-fn (fn [id callback]
                     (callback
                       (get-in @accounts-by-id
                               [id])))
          :on-change #(adj-items page-state)}]]
   [:td [forms/text-input sched-tran    [:scheduled-transaction/items index :scheduled-transaction-item/memo] {:on-change #(adj-items page-state)}]]
   [:td [forms/decimal-input sched-tran [:scheduled-transaction/items index :debit-quantity] {:on-accept #(adj-items page-state)}]]
   [:td [forms/decimal-input sched-tran [:scheduled-transaction/items index :credit-quantity] {:on-accept #(adj-items page-state)}]]])

(defn- items-table-footer
  [page-state]
  (let [items (r/cursor page-state [:selected :scheduled-transaction/items])
        total-credits (make-reaction #(->> @items
                                           (map :credit-quantity)
                                           (filter identity)
                                           (reduce decimal/+ (decimal/zero))))
        total-debits (make-reaction #(->> @items
                                          (map :debit-quantity)
                                          (filter identity)
                                          (reduce decimal/+ (decimal/zero))))
        css-class (make-reaction #(if (= @total-debits
                                         @total-credits)
                                    "text-success"
                                    "text-danger"))]
    (fn []
      [:tfoot
       [:tr
        [:td {:col-span 2} (html/space)]
        [:td {:class @css-class}
         (format-decimal @total-debits)]
        [:td  {:class @css-class}
         (format-decimal @total-credits)]]])))

(defn- items-table
  [page-state]
  (let [sched-tran (r/cursor page-state [:selected])]
    (fn []
      [:table.table
       [:thead
        [:tr
         [:th "Account"]
         [:th "Memo"]
         [:th "Debit"]
         [:th "Credit"]]]
       [:tbody
        (->> (range (count (:scheduled-transaction/items @sched-tran)))
             (map #(item-row % sched-tran page-state))
             doall)]
       [items-table-footer page-state]])))

(defn- sched-tran-form
  [page-state]
  (let [sched-tran (r/cursor page-state [:selected])]
    (fn []
      [:form {:no-validate true
              :class (when-not @sched-tran "d-none")
              :on-submit (fn [e]
                           (.preventDefault e)
                           (save-sched-tran page-state))}
       [forms/text-field sched-tran [:scheduled-transaction/description] {:validation [:required]}]
       [forms/text-field sched-tran [:scheduled-transaction/memo]]
       [:div.row
        [:div.col
         [forms/date-field sched-tran [:scheduled-transaction/start-date] {:validation [:required]}]]
        [:div.col
         [forms/date-field sched-tran [:scheduled-transaction/end-date] {:validation [:required]}]]]
       [:div.row
        [:div.col
         [forms/select-field
          sched-tran
          [:scheduled-transaction/interval-type]
          [[:year "Yearly"]
           [:month "Monthly"]
           [:week "Weekly"]]
          {:transform-fn keyword}]]
        [:div.col
         [forms/integer-field sched-tran [:scheduled-transaction/interval-count] {:validation [:required]}]]]
       (case (:scheduled-transaction/interval-type @sched-tran)
         :year [:div.row [:div.col [forms/select-field
                                    sched-tran
                                    [:scheduled-transaction/date-spec :month]
                                    (map #(vector (csk/->kebab-case-keyword %)
                                                  %)
                                         calendar/month-names)
                                    {:transform-fn keyword}]]
                [:div.col
                 [forms/integer-field sched-tran [:scheduled-transaction/date-spec :day] {:validation [:required]}]]]
         :month [forms/integer-field sched-tran [:scheduled-transaction/date-spec :day] {:validation [:required]}]
         :week [forms/checkboxes-field sched-tran [:scheduled-transaction/date-spec :days] (map #(vector (csk/->kebab-case-keyword (:name %))
                                                                                                         (:abbreviation %))
                                                                                                calendar/day-data)]
         "")
       [items-table page-state]
       [forms/checkbox-field sched-tran [:scheduled-transaction/enabled]]
       [:button.btn.btn-primary {:type :submit
                                 :title "Click here to save this scheduled transaction."}
        (icon-with-text :check "Save")]
       [:button.btn.btn-secondary.ms-2 {:type :button
                                        :title "Click here to cancel this operation."
                                        :on-click #(swap! page-state dissoc :selected)}
        (icon-with-text :x-circle "Cancel")]])))

(defn- created-row
  [{:transaction/keys [transaction-date description value] :as trx}]
  ^{:key (str "created-transaction-row-" (:id trx))}
  [:tr
   [:td (format-date transaction-date)]
   [:td description]
   [:td (format-decimal value)]])

(defn- created
  [page-state]
  (let [created (r/cursor page-state [:created])]
    (fn []
      [:div {:class (when-not (seq @created) "d-none")}
       [:h3.mt-3 "Created Transactions"]
       [:table.table.table-hover
        [:thead
         [:tr
          [:th "Date"]
          [:th "Description"]
          [:th "Value"]]]
        [:tbody
         (->> @created
              (map created-row)
              doall)]]])))

(defn- index
  []
  (let [page-state (r/atom {:scheduled-transactions @auto-loaded
                            :hide-inactive? true
                            :sort-on :scheduled-transaction/next-occurrence})
        selected (r/cursor page-state [:selected])]
    (when-not @auto-loaded
      (load-sched-trans page-state))
    (add-watch current-entity ::index (fn [& _] (load-sched-trans page-state)))
    (reset! auto-loaded nil)
    (fn []
      [:div.mt-3
       [:h1 "Scheduled Transactions"]
       [:div {:class (when @selected "d-none")}
        [forms/checkbox-field page-state [:hide-inactive?]]
        [sched-trans-table page-state]
        [:div.d-flex.mb-5
         [:button.btn.btn-primary
          {:title "Click here to schedule a recurring transaction."
           :disabled @selected
           :on-click (fn []
                       (swap! page-state
                              assoc
                              :selected
                              #:scheduled-transaction{:entity @current-entity
                                                      :interval-type :yearly
                                                      :enabled true
                                                      :items [{:debit-quantity nil}
                                                              {:credit-quantity nil}]})
                       (set-focus "description"))}
          (icon-with-text :plus "Add")]
         [:button.btn.btn-secondary.ms-2 {:title "Click here to new transactions from the schedule."
                                          :type :button
                                          :disabled @busy?
                                          :on-click #(realize page-state)}
          (if @busy?
            [:div.d-flex.align-items-center
             [:div.spinner-border.spinner-border-sm {:role :status}
              [:span.visually-hidden "Working..."]]
             [:span.ms-1 "Realize"]]
            (icon-with-text :gear "Realize"))]]
        [created page-state]]
       [:div {:class (when-not @selected "d-none")}
        [sched-tran-form page-state]]])))

(defn- autorun []
  (+busy)
  (sched-trans/select {:scheduled-transaction/enabled true
                       :scheduled-transaction/start-date [:<= (t/today)]
                       :scheduled-transaction/end-date [:> (t/today)]}
                      :callback -busy
                      :on-success (fn [results]
                                    (let [r (map-next-occurrence results)
                                          destination (if (some pending? r)
                                                        "/scheduled"
                                                        "/")]
                                      (reset! auto-loaded (seq r))
                                      (secretary/dispatch! destination))))
  (fn []
    [:div.row.mt-3
     [:div.col-md-4.offset-md-4
      [:h1 "Welcome!"]
      [:div "We are automatically processing scheduled transactions. One moment, please."]
      [:div.d-flex.justify-content-around
       [:div.spinner-border {:role :status}
        [:span.visually-hidden "Loading..."]]]]]))

(secretary/defroute "/scheduled" []
  (swap! app-state assoc :page #'index))
(secretary/defroute "/scheduled/autorun" []
  (swap! app-state assoc :page #'autorun))
