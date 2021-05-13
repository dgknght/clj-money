(ns clj-money.views.scheduled
  (:require [clojure.string :as string]
            [cljs-time.core :as t]
            [secretary.core :as secretary :include-macros true]
            [reagent.core :as r]
            [reagent.ratom :refer [make-reaction]]
            [camel-snake-kebab.core :as csk]
            [dgknght.app-lib.core :as lib]
            [dgknght.app-lib.models :refer [map-index]]
            [dgknght.app-lib.web :refer [format-date
                                         format-decimal]]
            [dgknght.app-lib.inflection :refer [title-case]]
            [dgknght.app-lib.html :as html]
            [dgknght.app-lib.calendar :as calendar]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.decimal :as decimal]
            [dgknght.app-lib.notifications :as notify]
            [clj-money.state :refer [app-state
                                     current-entity]]
            [clj-money.bootstrap :as bs]
            [clj-money.api.accounts :as accounts]
            [clj-money.accounts :refer [nest
                                        unnest
                                        find-by-path]]
            [clj-money.scheduled-transactions :refer [next-transaction-date
                                                      pending? ]]
            [clj-money.api.scheduled-transactions :as sched-trans]))

(defonce ^:private auto-loaded (atom []))

(defn set-next-occurrence
  [sched-tran]
  (assoc sched-tran :next-occurrence (next-transaction-date sched-tran)))

(defn- update-sched-trans
  [state transactions]
  (let [mapped-trans (lib/index-by :scheduled-transaction-id transactions)]
    (update-in state
               [:scheduled-transactions]
               (fn [sched-trans]
                 (map (fn [sched-tran]
                        (if-let [trans (get-in mapped-trans [(:id sched-tran)])]
                          (-> sched-tran
                              (assoc :last-occurrence (:transaction-date trans))
                              set-next-occurrence)
                          sched-tran))
                      sched-trans)))))

(defn- realize
  [& args]
  (let [[sched-tran page-state] (if (= 1 (count args))
                                  (cons nil args)
                                  args)
        success-fn (fn [result]
                     (swap! page-state #(-> %
                                            (update-sched-trans result)
                                            (dissoc :busy?)
                                            (update-in [:created] (fnil concat []) result)))
                     (notify/toast "Success" (if (empty? result)
                                               "No transactions are ready to be created."
                                               "The scheduled transactions where created")))
        error-fn (fn [error]
                   (swap! page-state dissoc :busy?)
                   (notify/danger (str "Unable to realize the transaction: " error)))]
    (swap! page-state assoc :busy? true)
    (if sched-tran
      (sched-trans/realize sched-tran success-fn error-fn)
      (sched-trans/realize success-fn error-fn))))

(defn- delete-sched-tran
  [_sched-tran _page-state]
  (js/alert "Not implemented"))

(defn- expired?
  [{:keys [end-date]}]
  (and end-date (t/after? (t/today) end-date)))

(def ^:private disabled? (complement :enabled))

(def ^:private active? (complement (some-fn expired? disabled?)))

(defn- ->editable-item
  [{:keys [action quantity] :as item}]
  (cond-> (dissoc item :quantity :action)
    (= :debit action) (assoc :debit-quantity quantity)
    (= :credit action) (assoc :credit-quantity quantity)))

(defn- ->editable
  [sched-tran]
  (update-in sched-tran
             [:items]
             (fn [items]
               (conj
                 (mapv ->editable-item items)
                 {}))))

(defn- sched-tran-row
  [sched-tran page-state busy?]
  ^{:key (str "sched-tran-row-" (:id sched-tran))}
  [:tr {:class (cond-> []
                 (disabled? sched-tran) (conj "sched-tran-disabled")
                 (expired? sched-tran) (conj "sched-tran-expired"))}
   [:td (:description sched-tran)]
   [:td (format-date (:last-occurrence sched-tran))]
   [:td (format-date (:next-occurrence sched-tran))]
   [:td
    [:div.btn-group
     [:button.btn.btn-sm {:title "Click here to realize transactions for this schedule."
                          :class (if (pending? sched-tran)
                                   "btn-success"
                                   "btn-light")
                          :on-click #(realize sched-tran page-state)}
      (if busy?
        (bs/spinner {:size :small})
        (bs/icon :gear))]
     [:button.btn.btn-info.btn-sm {:title "Click here to edit this scheduled transaction."
                                   :on-click (fn [_]
                                               (swap! page-state assoc :selected (->editable sched-tran))
                                               (html/set-focus "description"))}
      (bs/icon :pencil)]
     [:button.btn.btn-danger.btn-sm {:title "Click here to remove this scheduled transaction."
                                     :on-click #(delete-sched-tran sched-tran page-state)}
      (bs/icon :x)]]]])

(defn- date-compare
  [d1 d2]
  (t/before? (or d1 (t/epoch))
             (or d2 (t/epoch))))

(defn- table-header
  [page-state]
  (let [sort-on (r/cursor page-state [:sort-on])]
    (fn []
      [:thead
       [:tr
        (->> [:description :last-occurrence :next-occurrence]
             (map (fn [k]
                    ^{:key (str "table-header-" (name k))}
                    [:th
                     (title-case (name k))
                     [:a.ml-3 {:href "#"
                               :class (if (= k @sort-on)
                                        "text-dark"
                                        "text-muted")
                               :on-click #(reset! sort-on k)}
                      (bs/icon :sort-down-alt)]]))
             doall)
        [:th (html/space)]]])))

(defn- sched-trans-table
  [page-state]
  (let [sched-trans (r/cursor page-state [:scheduled-transactions])
        busy? (r/cursor page-state [:busy?])
        hide-inactive? (r/cursor page-state [:hide-inactive?])
        sort-on (r/cursor page-state [:sort-on])
        sort-fn (make-reaction #({:next-occurrence date-compare
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

(defn- map-next-occurrence
  [sched-trans]
  (map #(assoc % :next-occurrence (next-transaction-date %))
       sched-trans))

(defn- load-sched-trans
  [page-state]
  (sched-trans/search (fn [result]
                        (swap! page-state
                               assoc
                               :scheduled-transactions
                               (map-next-occurrence result)))
                      (notify/danger-fn "Unable to load the scheduled transactions: %s")))

(defn- ->saveable-item
  [{:keys [debit-quantity credit-quantity] :as item}]
  (-> item
      (assoc :action (if debit-quantity
                       :debit
                       :credit)
             :quantity (or debit-quantity credit-quantity))
      (dissoc :debit-quantity :credit-quantity)))

(defn- empty-item?
  [{:keys [account-id debit-quantity credit-quantity]}]
  (and (nil? account-id)
       (nil? debit-quantity)
       (nil? credit-quantity)))

(defn- ->saveable
  [sched-tran]
  (update-in sched-tran
             [:items]
             (fn [items]
               (->> items
                    (remove empty-item?)
                    (mapv ->saveable-item)))))

(defn- save-sched-tran
  [page-state]
  (-> (:selected @page-state)
      ->saveable
      (sched-trans/save (fn [_]
                          (load-sched-trans page-state)
                          (swap! page-state dissoc :selected))
                        (notify/danger-fn "Unable to schedule the transaction: %s"))))

(defn- adj-items
  [page-state]
  (swap! page-state
         update-in
         [:selected]
         (fn [sched-tran]
           (update-in sched-tran
                      [:items]
                      (fn [items]
                        (conj
                          (->> items
                               (remove empty-item?)
                               vec)
                          {:debit-quantity nil
                           :credit-quantity nil
                           :account-id nil}))))))

(defn- item-row
  [index sched-tran page-state]
  ^{:key (str "item-row-" index)}
  [:tr
   [:td [forms/typeahead-input
         sched-tran
         [:items index :account-id]
         {:id (str "account-id-" index)
          :search-fn (fn [input callback]
                       (callback (find-by-path input (vals (:accounts @page-state)))))
          ;:on-change #(ensure-entry-state page-state)
          ;:on-key-up #(item-navigate % item-count)
          :caption-fn #(string/join "/" (:path %))
          :value-fn :id
          :find-fn (fn [id callback]
                     (callback
                       (get-in (:accounts @page-state)
                               [id])))
          :on-change #(adj-items page-state)}]]
   [:td [forms/text-input sched-tran [:items index :memo] {:on-change #(adj-items page-state)}]]
   [:td [forms/decimal-input sched-tran [:items index :debit-quantity] {:on-accept #(adj-items page-state)}]]
   [:td [forms/decimal-input sched-tran [:items index :credit-quantity] {:on-accept #(adj-items page-state)}]]])

(defn- items-table-footer
  [page-state]
  (let [items (r/cursor page-state [:selected :items])
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
        (->> (range (count (:items @sched-tran)))
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
       [forms/text-field sched-tran [:description] {:validation [:required]}]
       [forms/text-field sched-tran [:memo]]
       [:div.row
        [:div.col
         [forms/date-field sched-tran [:start-date] {:validation [:required]}]]
        [:div.col
         [forms/date-field sched-tran [:end-date] {:validation [:required]}]]]
       [:div.row
        [:div.col
         [forms/select-field
          sched-tran
          [:interval-type]
          [[:year "Yearly"]
           [:month "Monthly"]
           [:week "Weekly"]]
          {:transform-fn keyword}]]
        [:div.col
         [forms/integer-field sched-tran [:interval-count] {:validation [:required]}]]]
       (case (:interval-type @sched-tran)
         :year [:div.row [:div.col [forms/select-field
                                    sched-tran
                                    [:date-spec :month]
                                    (map #(vector (csk/->kebab-case-keyword %)
                                                  %)
                                         calendar/month-names)
                                    {:transform-fn keyword}]]
                [:div.col
                 [forms/integer-field sched-tran [:date-spec :day] {:validation [:required]}]]]
         :month [forms/integer-field sched-tran [:date-spec :day] {:validation [:required]}]
         :week [forms/checkboxes-field sched-tran [:date-spec :days] (map #(vector (csk/->kebab-case-keyword (:name %))
                                                                                   (:abbreviation %))
                                                                          calendar/day-data)]
         "")
       [items-table page-state]
       [forms/checkbox-field sched-tran [:enabled]]
       [:button.btn.btn-primary {:type :submit
                                 :title "Click here to save this scheduled transaction."}
        (bs/icon-with-text :check "Save")]
       (html/space)
       [:button.btn.btn-secondary {:type :button
                                   :title "Click here to cancel this operation."
                                   :on-click #(swap! page-state dissoc :selected)}
        (bs/icon-with-text :x-circle "Cancel")]])))

(defn- created-row
  [{:keys [id transaction-date description value]}]
  ^{:key (str "created-transaction-row-" id)}
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

(defn- load-accounts
  [page-state]
  (accounts/select #(swap! page-state
                           assoc
                           :accounts
                           (->> %
                                nest
                                unnest
                                map-index))
                   (notify/danger-fn "Unable to load the accounts: %s")))

(defn- index
  []
  (let [page-state (r/atom {:scheduled-transactions @auto-loaded
                            :hide-inactive? true
                            :sort-on :next-occurrence})
        selected (r/cursor page-state [:selected])]
    (when-not @auto-loaded
      (load-sched-trans page-state))
    (reset! auto-loaded nil)
    (load-accounts page-state)
    (fn []
      [:div.mt-5
       [:h1 "Scheduled Transactions"]
       [:div {:class (when @selected "d-none")}
        [forms/checkbox-field page-state [:hide-inactive?]]
        [sched-trans-table page-state]
        [:div.d-flex
         [:button.btn.btn-primary {:title "Click here to schedule a recurring transaction."
                                   :disabled @selected
                                   :on-click (fn []
                                               (swap! page-state
                                                      assoc
                                                      :selected
                                                      {:entity-id (:id @current-entity)
                                                       :interval-type :yearly
                                                       :enabled true
                                                       :items [{:debit-quantity nil}
                                                               {:credit-quantity nil}]})
                                               (html/set-focus "description"))}
          (bs/icon-with-text :plus "Add")]
         (html/space)
         [:button.btn.btn-info {:title "Click here to new transactions from the schedule."
                                :type :button
                                :on-click #(realize page-state)}
          (bs/icon-with-text :gear "Realize")]]
        [created page-state]]
       [:div {:class (when-not @selected "d-none")}
        [sched-tran-form page-state]]])))

(defn- autorun []
  (sched-trans/search
    (fn [results]
      (let [r (map-next-occurrence results)]
        (reset! auto-loaded (seq r))
        (secretary/dispatch! (if (some pending?
                                       r)
                               "/scheduled"
                               "/"))))
    (notify/danger-fn "Unable to auto-create scheduled transactions: %s"))
  [:div.mt-5
   [:h1 "Welcome!"]
   [:div.spinner-border {:role :status}
    [:span.sr-only "Loading..."]]
   [:p "We are automatically processing scheduled transactions. One moment, please."]])

(secretary/defroute "/scheduled" []
  (swap! app-state assoc :page #'index))
(secretary/defroute "/scheduled/autorun" []
  (swap! app-state assoc :page #'autorun))
