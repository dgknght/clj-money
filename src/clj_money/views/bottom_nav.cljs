(ns clj-money.views.bottom-nav
  "The tab bar fixed to the bottom of small screens (below lg), in place of
  the navbar's collapsed menu (issue #308). It has a slot each for receipts
  (labeled Add, as entering a receipt is how a transaction is added on the
  go), accounts, reports and search, and a More slot that opens a sheet
  holding the rest of the nav items, under the headings of their groups,
  as in the desktop navbar's dropdowns. The navbar's logo goes to the
  dashboard."
  (:require [clj-money.icons :refer [icon]]
            [clj-money.nav :refer [sections]]))

(def ^:private slots
  [{:id :receipts
    :label "Add"
    :icon :plus-circle}
   {:id :accounts
    :label "Accounts"
    :icon :wallet2}
   {:id :reports
    :label "Reports"
    :icon :bar-chart}
   {:id :search
    :label "Search"
    :icon :search}])

(def ^:private slot-ids
  (set (map :id slots)))

(defn- label
  [item]
  (or (:label item)
      (:infered-label item)))

(defn- slot-items
  "Merges each slot over the nav item with the same id, if there is one,
  so the slot keeps the item's path and tool-tip."
  [items active-nav]
  (let [by-id (into {} (map (juxt :id identity)) items)]
    (map (fn [{:keys [id] :as slot}]
           (merge (by-id id)
                  slot
                  {:active? (= id active-nav)}))
         slots)))

(defn- slot-link
  [{:keys [id path tool-tip active? badge badge-class] :as item}]
  ^{:key (str "bottom-nav-" (name id))}
  [:li.nav-item
   [:a.nav-link {:href path
                 :title tool-tip
                 :class (when active? "active")
                 :aria-current (when active? "page")}
    (icon (:icon item))
    [:span (label item)]
    (when badge
      [:span.bottom-nav-badge.badge.rounded-pill {:class badge-class}
       badge])]])

(defn- more-button
  [more-items]
  (let [active? (some :active? more-items)]
    [:li.nav-item
     [:button.nav-link {:type :button
                        :class (when active? "active")
                        :title "Click here for more pages"
                        :data-bs-toggle "offcanvas"
                        :data-bs-target "#more-nav"
                        :aria-controls "more-nav"}
      (icon :three-dots)
      [:span "More"]
      (when (some :badge more-items)
        [:span.bottom-nav-indicator.badge.rounded-pill.bg-danger.p-1
         [:span.visually-hidden "Items need attention"]])]]))

(defn- more-link
  [{:keys [id path tool-tip active? badge badge-class] :as item}]
  ^{:key (str "more-nav-" (name id))}
  [:a.list-group-item.list-group-item-action.d-flex.justify-content-between.align-items-center
   {:href path
    :title tool-tip
    :class (when active? "active")
    :aria-current (when active? "page")
    :data-bs-dismiss :offcanvas}
   (label item)
   (when badge
     [:span.badge.rounded-pill {:class badge-class} badge])])

(defn- more-sheet
  [more-items]
  [:div#more-nav.offcanvas.offcanvas-bottom.d-lg-none
   {:tab-index -1
    :aria-labelledby "more-nav-label"}
   [:div.offcanvas-header
    [:h5#more-nav-label.offcanvas-title "More"]
    [:button.btn-close {:type :button
                        :data-bs-dismiss :offcanvas
                        :aria-label "Close"}]]
   [:div.offcanvas-body
    (doall
      (map-indexed
        (fn [i [heading items]]
          ^{:key (str "more-nav-section-" i)}
          [:<>
           (when heading
             [:h6.text-body-secondary.mt-3.mb-2 heading])
           [:div.list-group
            (doall (map more-link items))]])
        (sections more-items)))]])

(defn bottom-nav
  "Renders the bottom tab bar for the nav items (as decorated for the
  navbar, with their :active? flags), highlighting the slot for
  active-nav. The page's content needs the with-bottom-nav class, so it
  isn't hidden behind the bar."
  [items active-nav]
  (let [more-items (remove (comp slot-ids :id) items)]
    [:<>
     [:nav.bottom-nav.fixed-bottom.d-lg-none.d-print-none.bg-body-tertiary.border-top
      {:aria-label "Primary"}
      [:ul.nav.nav-fill
       (doall (map slot-link (slot-items items active-nav)))
       [more-button more-items]]]
     [more-sheet more-items]]))
