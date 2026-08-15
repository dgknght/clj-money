(ns clj-money.views.recent-transactions
  "Shared sort-by / direction / result-count controls and logic for the
  'Recent Transactions' tables on the receipt entry and dividend entry
  pages."
  (:require [reagent.core :as r]
            [cljs-time.core :as t]
            [dgknght.app-lib.core :refer [parse-int]]
            [dgknght.app-lib.forms :as forms]
            [clj-money.icons :refer [icon]]))

(def default-settings
  {:sort-on :transaction/created-at
   :dir :desc
   :limit 10})

(def ^:private sort-fields
  [["created-at" "Entry Time"]
   ["transaction-date" "Transaction Date"]])

(def ^:private limit-options
  (map (juxt str str) [10 25 50 100]))

(defn- date-compare
  [d1 d2]
  (t/before? (or d1 (t/epoch)) (or d2 (t/epoch))))

(defn- base-compare
  [sort-on t1 t2]
  (let [d1 (get t1 sort-on)
        d2 (get t2 sort-on)]
    (cond
      (date-compare d1 d2) -1
      (date-compare d2 d1) 1
      :else (compare (:id t1) (:id t2)))))

(defn compare-transactions
  "Compares two transactions according to the given settings map
  (:sort-on and :dir), breaking ties on :id, which increases monotonically
  with insertion, since many transactions can share a date or timestamp."
  [{:keys [sort-on dir]} t1 t2]
  (cond-> (base-compare sort-on t1 t2)
    (= dir :desc) -))

(defn sort-and-limit
  [transactions settings]
  (->> transactions
       (sort (partial compare-transactions settings))
       (take (:limit settings))))

(defn controls
  "Renders sort-by, sort-direction, and result-count controls. settings-path
  identifies, within page-state, a map containing :sort-on, :dir, and
  :limit."
  [page-state settings-path]
  (let [settings (r/cursor page-state settings-path)]
    (fn []
      [:div.d-flex.gap-2.align-items-end.mb-2
       [forms/select-field
        settings
        [:sort-on]
        sort-fields
        {:caption "Sort By"
         :transform-fn #(keyword "transaction" %)}]
       [:button.btn.btn-secondary
        {:type :button
         :title "Click here to reverse the sort direction."
         :on-click #(swap! settings update :dir (fn [dir] (if (= dir :desc) :asc :desc)))}
        (icon (if (= :desc (:dir @settings)) :sort-down :sort-up))]
       [forms/select-field
        settings
        [:limit]
        limit-options
        {:caption "Show"
         :transform-fn parse-int}]])))
