(ns clj-money.web.budgets
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint cl-format]]
            [environ.core :refer [env]]
            [clj-time.core :as t]
            [clj-time.format :as tf]
            [clj-time.coerce :as tc]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [ring.util.codec :refer [url-encode]]
            [clj-money.url :refer :all]
            [clj-money.inflection :refer [humanize]]
            [clj-money.util :refer [format-number format-date]]
            [clj-money.validation :as validation]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.budgets :as budgets]
            [clj-money.schema :as schema]
            [clj-money.web.money-shared :refer [account-options]])
  (:use [clj-money.web.shared :refer :all])
  (:import org.joda.time.Months
           org.joda.time.Weeks
           org.joda.time.format.DateTimeFormat))

(defn- budget-row
  [budget]
  [:tr
   [:td (:name budget)]
   [:td (:period budget)]
   [:td.text-right (:period-count budget)]
   [:td.text-right (format-date (:start-date budget))]
   [:td.text-right (format-date (:end-date budget))]
   [:td
    [:div.btn-group
     (glyph-button :pencil
                   (format "/budgets/%s/edit" (:id budget))
                   {:level :info
                    :size :extra-small
                    :title "Click here to edit this budget."})
     (glyph-button :list-alt
                   (format "/budgets/%s" (:id budget))
                   {:level :default
                    :size :extra-small
                    :title "Click here to view the details of this budget."})
     (glyph-button :remove
                   (format "/budgets/%s/delete" (:id budget))
                   {:level :danger
                    :size :extra-small
                    :title "Click here to remove this budget."
                    :data-confirm "Are you sure you want to remove this budget?"
                    :data-method :post})]]])

(defn index
  ([entity-id] (index entity-id {}))
  ([entity-id options]
   (with-layout "Budgets" options
     [:div.row
      [:div.col-md-6
       [:table.table.table-striped
        [:tr
         [:th "Name"]
         [:th "Period"]
         [:th.text-right "Period count"]
         [:th.text-right "Start date"]
         [:th.text-right "End date"]
         [:th "&nbsp;"]]
        (map budget-row (budgets/select-by-entity-id (env :db) entity-id))]
       [:a.btn.btn-primary {:href (format "/entities/%s/budgets/new" entity-id)}
        "Add"]]])))

(defn default-start-date
  []
  (let [now (t/now)]
    (t/local-date (+ 1 (t/year now)) 1 1)))

(defn form-fields
  [budget]
  (html
    (text-input-field budget :name {:autofocus true})
    (select-field budget :period [{:value :week    :caption "Week"}
                                  {:value :month   :caption "Month"}
                                  {:value :quarter :caption "Quarter"}])
    (number-input-field budget :period-count {:step "1"
                                              :format-fn #(format-number % {:format :integer})})
    (text-input-field budget :start-date {:class "date-field"
                                          :format-fn format-date})
    [:button.btn.btn-primary {:type :submit} "Save"]
    "&nbsp;"
    [:a.btn.btn-default {:href (format "/entities/%s/budgets" (:entity-id budget))}
     "Back"]))

(defn new-budget
  ([entity-id]
   (let [start-date (default-start-date)]
     (new-budget entity-id {:entity-id entity-id
                            :name (str (t/year start-date))
                            :period :month
                            :period-count 12
                            :start-date start-date})))
  ([entity-id budget]
   (with-layout "New budget" {}
     [:div.row
      [:div.col-md-3
       [:form {:action (format "/entities/%s/budgets" entity-id)
               :method :post}
        (form-fields budget)]]])))

(defn create
  "Creates the budget and redirects to the index page on success, or
  re-renders the new form on failure"
  [params]
  (let [budget (select-keys params [:entity-id :name :period :period-count :start-date])
        saved (budgets/create (env :db) budget)]
    (if (validation/has-error? saved)
      (new-budget (:entity-id saved) saved)
      (redirect (format "/entities/%s/budgets" (:entity-id params))))))

(defn- budget-period-start-date
  [budget index]
  (tf/unparse-local (tf/formatters :year-month)
                    (t/plus (:start-date budget)
                            (t/months index))))

(defn- period-heading
  [budget index]
  [:th.text-right (budget-period-start-date budget index)])

(defn- budget-header-row
  [budget]
  [:tr
   (html
     [:th "Account"]
     (map #(period-heading budget %) (range 0 (:period-count budget)))
     [:th.text-right "Total"]
     [:td "&nbsp;"])])

(defn- budget-item-row
  "Renders a budget item row. Accepts a render-ready data structure that looks like this:

    :caption - Row header
    :data    - sequence of values, 1 for each period in the budget
    :style   - header|data"
  [item]
  [:tr {:class (case (:style item)
                 :header :report-header
                 :summary :report-summary
                 :data :report-data
                 nil)}
   (html
     [:td (:caption item)]
     (map #(vector :td.text-right (format-number (:value %))) (:data item))
     [:td.text-right (format-number (reduce + 0 (map :value (:data item))))]
     [:td
      (when (:id item)
        [:div.btn-group
         [:div.btn-group
          [:button.btn.btn-info.btn-xs.dropdown-toggle
           {:type :button
            :title "Click here to edit this budget item."
            :data-toggle :dropdown
            :aria-haspop true
            :aria-expanded false}
           [:span.glyphicon.glyphicon-pencil {:aria-hidden true}]
           "&nbsp;"
           [:span.caret]]
          [:ul.dropdown-menu
           [:li [:a {:href (format "/budget-items/%s/edit/average" (:id item))} "By average"]]
           [:li [:a {:href (format "/budget-items/%s/edit/total" (:id item))} "By total"]]
           [:li [:a {:href (format "/budget-items/%s/edit/detail" (:id item))} "By period"]]]]])])])

(defn- summarize-periods
  [items]
  (if (seq items)
    (let [period-matrix (map (fn [item]
                               (map :amount (:periods item)))
                             items)]
      (reduce (fn [totals periods]
                (->> (interleave totals periods)
                     (partition 2)
                     (map #(apply + %))))
              period-matrix))
    items))

(defn- process-budget-item-group
  "Process budget items by group and return a render-ready structure

  context has this shape:
  :items   - the items being processed
  :result  - the result of the processing
  :totals  - a map of account type to totals"
  [context account-type]
  (let [typed-items (->> context
                         :items
                         (filter #(= account-type (-> % :account :type)))
                         (sort-by #(-> % :account :name)))
        totals (vec (summarize-periods typed-items))]
    (-> context
        (update-in [:result]
                   (fn [result]
                     (concat result
                             ; header
                             [{:caption (humanize account-type)
                               :style :header
                               :data (map #(hash-map :value %) totals)}]
                             ; data
                             (map #(hash-map :caption (-> % :account :name)
                                             :style :data
                                             :id (:id %)
                                             :data (map (fn [p] {:value (:amount p)})
                                                        (:periods %)))
                                  typed-items))))
        (update-in [:totals] #(assoc % account-type totals)))))

(defn- append-account
  "Assoces an account onto an item that contains an account-id value"
  [{account-id :account-id :as item}]
  (assoc item :account (accounts/find-by-id (env :db) account-id)))

(defn- group-budget-items
  "Accepts raw budget items and returns render-ready data structures

  :caption - The row header, either the account name or the account type header
  :data    - The period data"
  [items]
  (if (seq items)
    (let [items-with-accounts (map append-account items)
          {result :result
           {:keys [income expense]} :totals} (reduce process-budget-item-group
                                                     {:result []
                                                      :items items-with-accounts
                                                      :totals {}}
                                                     [:income :expense])]
      (-> result
          (concat [{:caption "Net"
                    :style :summary
                    :data (->> (interleave income expense)
                               (partition 2)
                               (map #(apply - %))
                               (map #(hash-map :value %)))}])))
    items))

(defn for-display
  "Returns a budget that has been prepared for rendering in the UI"
  [id]
  (-> (budgets/find-by-id (env :db) id)
      (update-in [:items] group-budget-items)))

(defn show
  "Renders the budet details"
  [id]
  (let [budget (for-display id)]
    (with-layout (str "Budget: " (:name budget)) {}
      [:div.row
       [:div.col-md-12
        [:table.table.table-striped
         (html
           (budget-header-row budget)
           (map budget-item-row (:items budget)))]
        [:div.btn-group
         [:button.btn.btn-primary.dropdown-toggle
          {:type "button"
           :data-toggle "dropdown"
           :aria-haspopup true
           :aria-expanded false}
          "Add"
          "&nbsp;"
          [:span.caret]]
         [:ul.dropdown-menu
          [:li
           [:a {:href (format "/budgets/%s/items/new/average" (:id budget))} "By average"]]
          [:li
           [:a {:href (format "/budgets/%s/items/new/total" (:id budget))} "By total"]]
          [:li
           [:a {:href (format "/budgets/%s/items/new/detail" (:id budget))} "By period"]]]]
        "&nbsp;"
        [:a.btn.btn-default {:href (format "/entities/%s/budgets" (:entity-id budget))} "Back"]]])))

(defn edit
  "Renders an edit form for the specified budget"
  [id-or-budget]
  (with-layout "Edit budget" {}
    (let [budget (if (map? id-or-budget)
                   id-or-budget
                   (budgets/find-by-id (env :db) id-or-budget))]
      [:div.row
       [:div.col-md-3
        [:form {:action (format "/budgets/%s" (:id budget))
                :method :post}
         (form-fields budget)]]])))

(defn update
  "Updates the specified budget and redirects to the index page
  on success or the edit page on failure"
  [params]
  (let [budget (select-keys params [:id
                                    :name
                                    :period
                                    :period-count
                                    :start-date])
        updated (budgets/update (env :db) budget)]
    (if (validation/valid? updated)
      (redirect (format "/entities/%s/budgets" (:entity-id updated)))
      (edit updated))))

(defn delete
  "Deletes the specified budget and redirects to the budget index page"
  [id]
  (let [budget (budgets/find-by-id (env :db) id)]
    (try
      (budgets/delete (env :db) (:id budget))
      (redirect (format "/entities/%s/budgets" (:entity-id budget)))
      (catch Exception e
        (log/error e "Unable to delete the budget")
        (index (:entity-id budget) {:alerts [{:type :danger
                                              :message (html [:strong "Unable to delete the budget."
                                                              "&nbps;"
                                                              (.getMessage e)])}]})))))

(defmulti item-form-fields
  (fn [item _]
    (-> item :method keyword)))

(defmethod item-form-fields :average
  [item budget]
  [:div.row
   [:div.col-md-3
    (html
      (select-field item :account-id (account-options (:entity-id budget) {:types #{:income :expense}}) {:autofocus true})
      (number-input-field item :average)
      [:input {:type :hidden :name :method :value :average}]
      [:button.btn.btn-primary {:type :submit
                                :title "Click here to save this budget item."} "Save"]
      "&nbsp;"
      [:a.btn.btn-default {:href (format "/budgets/%s" (:id budget))} "Back"])]])

(defmethod item-form-fields :total
  [item budget]
  [:div.row
   [:div.col-md-3
    (html
      (select-field item :account-id (account-options (:entity-id budget) {:types #{:income :expense}}) {:autofocus true})
      (number-input-field item :total)
      [:input {:type :hidden :name :method :value :total}]
      [:button.btn.btn-primary {:type :submit
                                :title "Click here to save this budget item."} "Save"]
      "&nbsp;"
      [:a.btn.btn-default {:href (format "/budgets/%s" (:id budget))} "Back"])]])

(defn period-label
  [budget index]
  (->> (.multipliedBy Months/ONE index)
       (t/plus (:start-date budget))
       (.print (DateTimeFormat/forPattern "MMM yyyy"))))

(defn- period-input-field
  [budget period]
  [:div.form-group
   [:label.control-label (period-label budget (:index period))]
   [:input.form-control {:type :number
                         :step "0.01"
                         :name (format "period-%s" (:index period))
                         :value (format-number (:amount period) {:format :no-comma})}]])

(defn- period-input-group
  [budget periods]
  [:div.row
   (map #(vector :div.col-sm-4 (period-input-field budget %))
        periods)])

(defmethod item-form-fields :detail
  [item budget]
  (html
    [:div.row
     [:div.col-md-3
      (select-field item :account-id (account-options (:entity-id budget) {:types #{:income :expense}}) {:autofocus true})]]
    [:div.row
     [:div.col-md-8
      (map #(period-input-group budget %)
           (if (:periods item)
             (->> item
                  :periods
                  (partition 3))
             (->> budget
                  :period-count
                  range
                  (map #(hash-map :index % :amount 0M))
                  (partition 3))))]]
    [:input {:type :hidden :name :method :value :detail}]
    [:button.btn.btn-primary {:type :submit
                              :title "Click here to save this budget item."} "Save"]
    "&nbsp;"
    [:a.btn.btn-default {:href (format "/budgets/%s" (:id budget))} "Back"]))

(defn new-item
  "Renders a form for creating a new item"
  [{budget-id :budget-id :as item}]
  (let [budget (budgets/find-by-id (env :db) budget-id)]
    (with-layout (str "Budget " (:name budget) ": New item") {}
      [:form {:action (format "/budgets/%s/items" budget-id)
              :method :post}
       (item-form-fields item budget)])))

(defmulti extract-periods
  (fn [item _]
    (-> item :method keyword)))

(defmethod extract-periods :average
  [item budget]
  (let [amount (bigdec (:average item))]
    (-> item
        (assoc :periods
               (->> budget
                    :period-count
                    range
                    (mapv #(hash-map :amount amount :index %))))
        (dissoc :average))))

;TODO remove dulication between the above and below functions

(defmethod extract-periods :total
  [item budget]
  (let [amount (with-precision 10
                 (/ (bigdec (:total item))
                    (:period-count budget)))]
    (-> item
        (assoc :periods
               (->> budget
                    :period-count
                    range
                    (mapv #(hash-map :amount amount :index %))))
        (dissoc :total))))

(defmethod extract-periods :detail
  [item budget]
  (-> item
      (assoc :periods (->> (range (:period-count budget))
                           (map #(format "period-%s" %))
                           (map keyword)
                           (map #(% item))
                           (map bigdec)
                           (map-indexed #(hash-map :index %1 :amount %2))))
      (select-keys [:id :budget-id :account-id :periods])))

(defn create-item
  "Creates an budget item"
  [params]
  (let [budget (budgets/find-by-id (env :db) (Integer. (:budget-id params)))
        item (-> params
                 (update-in [:budget-id] #(Integer. %))
                 (update-in [:account-id] #(Integer. %))
                 (extract-periods budget)
                 (select-keys [:budget-id :account-id :periods]))
        saved (budgets/create-item (env :db) item)]
    (if (validation/valid? saved)
      (redirect (format "/budgets/%s" (:budget-id saved)))
      (new-item saved))))

(defmulti prepare-item-for-edit
  (fn [item]
    (-> item :method keyword)))

(defmethod prepare-item-for-edit :average
  [item]
  (-> item
      (assoc :average (with-precision 10
                        (/ (reduce + (map :amount (:periods item)))
                           (count (:periods item)))))
      (dissoc :periods)))

(defmethod prepare-item-for-edit :total
  [item]
  (-> item
      (assoc :total (reduce + (map :amount (:periods item))))
      (dissoc :periods)))

(defmethod prepare-item-for-edit :detail
  [item]
  item)

(defn edit-item
  "Renders a form for editing a budget item"
  [item-or-id method]
  (let [item (if (map? item-or-id)
               item-or-id
               (budgets/find-item-by-id (env :db) item-or-id))
        budget (budgets/find-by-id (env :db) (:budget-id item))
        account (accounts/find-by-id (env :db) (:account-id item))]
    (with-layout (format "Budget %s: %s" (:name budget) (:name account)) {}
      [:form {:action (format "/budget-items/%s" (:id item))
              :method :post}
       (-> item
           (assoc :method method)
           prepare-item-for-edit
           (item-form-fields budget))])))

(defn update-item
  "Updates the specified item and redirects to the budget on success or renders the
  edit from on failure"
  [params]
  (let [existing (budgets/find-item-by-id (env :db) (Integer. (:id params)))
        budget (budgets/find-by-id (env :db) (:budget-id existing))
        item (-> params
                 (extract-periods budget)
                 (select-keys [:id
                               :account-id
                               :periods]))
        updated (budgets/update-item (env :db) item)]
    (if (validation/valid? updated)
      (redirect (format "/budgets/%s" (:budget-id updated)))
      (edit updated))))

(defn new-monitor
  [entity-id]
  (with-layout "Budget Monitors" {}
    "Coming soon..."))