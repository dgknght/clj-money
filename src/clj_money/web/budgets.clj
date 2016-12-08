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
  (:use [clj-money.web.shared :refer :all]))

(defn- budget-row
  [budget]
  [:tr
   [:td (:name budget)]
   [:td (:period budget)]
   [:td.text-right (:period-count budget)]
   [:td.text-right (format-date (:start-date budget))]
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
   (layout
     "Budgets" options
     [:div.row
      [:div.col-md-6
       [:table.table.table-striped
        [:tr
         [:th "Name"]
         [:th "Period"]
         [:th.text-right "Period count"]
         [:th.text-right "Start date"]
         [:th "&nbsp;"]]
        (map budget-row (budgets/select-by-entity-id (env :db) entity-id))]
       [:a.btn.btn-primary {:href (format "/entities/%s/budgets/new" entity-id)} "Add"]]])))

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
    (number-input-field budget :period-count)
    (text-input-field budget :start-date {:class "date-field"} format-date)
    [:button.btn.btn-primary {:type :submit} "Save"]
    "&nbsp;"
    [:a.btn.btn-default {:href (format "/entities/%s/budgets" (:entity-id budget))} "Back"]))

(defn new-budget
  ([entity-id]
   (let [start-date (default-start-date)]
     (new-budget entity-id {:entity-id entity-id
                            :name (str (t/year start-date))
                            :period :month
                            :period-count 12
                            :start-date start-date})))
  ([entity-id budget]
   (layout
     "New budget" {}
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
     [:th.text-right "Total"])])

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
     [:td.text-right (format-number (reduce + 0 (map :value (:data item))))])])

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
          result (reduce process-budget-item-group
                       {:result []
                        :items items-with-accounts
                        :totals {}}
                       [:income :expense])]
      (-> (:result result)
          (concat [{:caption "Net"
                    :style :summary
                    :data (->> (interleave (-> result :totals :income)
                                           (-> result :totals :expense))
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
  (layout
    (str "Budget: " (:name budget)) {}
    [:div.row
     [:div.col-md-12
      [:table.table.table-striped
       (html
       (budget-header-row budget)
       (map budget-item-row (:items budget)))]
      [:a.btn.btn-primary {:href (format "/budgets/%s/items/new" (:id budget))} "Add"]
      "&nbsp;"
      [:a.btn.btn-default {:href (format "/entities/%s/budgets" (:entity-id budget))} "Back"]]])))

(defn edit
  "Renders an edit form for the specified budget"
  [id-or-budget]
  (layout
    "Edit budget" {}
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

(defn new-item
  "Renders a form for creating a new item"
  ([budget-id] (new-item budget-id {:budget-id budget-id}))
  ([budget-id item]
  (let [budget (budgets/find-by-id (env :db) budget-id)]
    (layout
      (str "Budget " (:name budget) ": New item") {}
      [:div.row
       [:div.col-md-3
        [:form {:action (format "/budgets/%s/items" budget-id)
                :method :post}
         (select-field item :account-id (account-options (:entity-id budget) {:types #{:income :expense}}) {:autofocus true})
         (number-input-field item :average)
         [:button.btn.btn-primary {:type :submit
                                   :title "Click here to save this budget item."} "Save"]
         "&nbsp;"
         [:a.btn.btn-default {:href (format "/budgets/%s" budget-id)} "Back"]]]]))))

(defn- extract-periods-from-average
  [item]
  (let [budget (budgets/find-by-id (env :db) (:budget-id item))
        amount (bigdec (:average item))]
    (->> budget
         :period-count
         range
         (mapv #(hash-map :amount amount :index %)))))

(defn create-item
  "Creates an budget item"
  [params]
  (let [item (-> params
                 (select-keys [:budget-id :account-id :average])
                 (update-in [:budget-id] #(Integer. %))
                 (update-in [:account-id] #(Integer. %)))
        adjusted (-> item
                     (assoc :periods (extract-periods-from-average item))
                     (dissoc :average))
        saved (budgets/create-item (env :db) adjusted)]
    (if (validation/valid? saved)
      (redirect (format "/budgets/%s" (:budget-id saved)))
      (new-item saved))))
