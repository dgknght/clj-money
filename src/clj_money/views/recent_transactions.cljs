(ns clj-money.views.recent-transactions
  "Shared sort-by / direction / result-count controls and logic for the
  'Recent Transactions' tables on the receipt entry and dividend entry
  pages."
  (:require [reagent.core :as r]
            [cljs-time.core :as t]
            [dgknght.app-lib.core :refer [parse-int]]
            [dgknght.app-lib.forms :as forms]
            [clj-money.icons :refer [icon icon-with-text]]))

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

(defn- controls
  "Renders sort-by, sort-direction, and result-count controls. settings-path
  identifies, within page-state, a map containing :sort-on, :dir, and
  :limit."
  [page-state settings-path]
  (let [settings (r/cursor page-state settings-path)]
    (fn []
      [:div
       [forms/select-field
        settings
        [:sort-on]
        sort-fields
        {:caption "Sort By"
         :transform-fn #(keyword "transaction" %)}]
       [:button.btn.btn-secondary.mb-3
        {:type :button
         :on-click #(swap! settings update :dir (fn [dir] (if (= dir :desc) :asc :desc)))}
        (if (= :desc (:dir @settings))
          (icon-with-text :sort-down "Newest First")
          (icon-with-text :sort-up "Oldest First"))]
       [forms/select-field
        settings
        [:limit]
        limit-options
        {:caption "Show"
         :transform-fn parse-int}]])))

(defn toggle
  "Renders a button that opens the recent-transactions options drawer with
  the given DOM id."
  [id]
  [:button.btn.btn-outline-secondary
   {:type :button
    :data-bs-toggle "offcanvas"
    :data-bs-target (str "#" id)
    :aria-controls id
    :title "Click here to change how recent transactions are sorted and how many are shown."}
   (icon :sliders :size :small)])

(defn drawer
  "Renders an off-canvas drawer, docked to the right side of the screen,
  containing the sort-by, sort-direction, and result-count controls.
  settings-path identifies, within page-state, a map containing :sort-on,
  :dir, and :limit. extra, if given, is additional hiccup rendered above
  the shared controls, for page-specific options like a date filter."
  ([id page-state settings-path] (drawer id page-state settings-path nil))
  ([id page-state settings-path extra]
   [:div.offcanvas.offcanvas-end {:id id :tab-index -1}
    [:div.offcanvas-header
     [:h3 "Options"]
     [:button.btn-close.text-reset {:data-bs-dismiss "offcanvas"
                                    :aria-label "Close"}]]
    [:div.offcanvas-body
     extra
     [controls page-state settings-path]]]))
